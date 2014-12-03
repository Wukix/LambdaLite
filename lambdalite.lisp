;;;; -*- coding:utf-8 -*-

(in-package :cl-user)

(defpackage :lambdalite
  (:use :cl)
  (:use :wu-sugar)
  (:export
   #:insert
   #:select
   #:select1
   #:update
   #:del
   ;; clauses
   #:where
   #:where-on
   #:keyset
   ;; misc
   #:load-db
   #:list-tables
   #:defattributes
   #:with-tx))

(in-package :lambdalite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :wu-sugar)
  (require :bordeaux-threads))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :wu-sugar))

(defparameter *db-path* nil)
(defparameter *db* nil)
(defparameter *tx* nil)
(defparameter *tx-modified-list* nil)
(defparameter *tx-lock* (bt:make-recursive-lock))

(defmacro with-lock (&body body)
  `(bt:with-recursive-lock-held (*tx-lock*)
     ,@body))

(defun load-db (&key (path "~/db/data/"))
  "Loads data files if they exist at PATH, or initializes an empty setup if not. Call once at startup."
  (let ((*read-eval* nil))
    (unless (stringp path)
      (setf path (namestring path)))
    (unless (ends-with-p path "/")
      (setf path (& path "/")))
    (unless (probe-file path) 
      (warn "Directory ~A does not exist. Creating." path)
      (ensure-directories-exist path))
    (setf *db* (make-hash-table)
          *db-path* path)        
    (dolist (file (directory (merge-pathnames path "*")))
      (when (pathname-name file) 
        (setf (gethash (intern (string-upcase (pathname-name file)) "KEYWORD") *db*) 
	      (read-from-string (file-to-string file)))))))

(defun make-filename (name)
  (string-downcase (symbol-name name)))

(defun persist (name &optional filename)
  (string-to-file 
   (prin1-to-string (gethash name *db*)) 
   (merge-pathnames *db-path* (or filename (make-filename name)))))

(defun list-tables ()
  "Returns a list of table names (as keywords)."
  (with-lock
    (let (table-names)
      (maphash (lambda (k v) (push k table-names)) *db*)
      table-names)))

(defun insert (name &rest rows)
  "Example: (insert :users (:/user-id 1 :/name \"Bob\") (:/user-id 2 :/name \"Alice\"))"
  (dolist (row rows)
    (loop for e in row
       for i from 0 do
         (when (and (evenp i) 
                    (not (and (keywordp e) 
                              (char= (char (symbol-name e) 0) #\/))))
           (error "Invalid attribute name ~S." e))))
  (with-lock
    (when *tx*
      (let ((tmp-name (cdr (assoc name *tx-modified-list*)))) 
        (unless tmp-name
          (setf tmp-name (clone-temporary name))
          (push (cons name tmp-name) *tx-modified-list*)
          (setf name tmp-name))
        (setf name tmp-name)))
    (setf (gethash name *db*) (append (gethash name *db*) rows))
    (unless *tx* (persist name))
    (length rows)))

(defun select (name &optional where-predicate)
  "Example: (select :users (where (search \"Bob\" :/name)))"
  (with-lock
    (when *tx*
      (let ((tmp-name (cdr (assoc name *tx-modified-list*)))) 
        (when tmp-name
          (setf name tmp-name))))
    (let ((rows (gethash name *db*)))
      (if where-predicate 
	  (remove-if-not where-predicate rows)
	  rows))))

(defun select1 (name &optional where-predicate)
  "Like SELECT, but returns the first row found."
  (first (select name where-predicate)))

(defun update (name where-predicate update-function)
  "Example: (update :things (where (evenp :/foo)) (keyset :/bar 1 :/baz 2))"
  (with-lock
    (when *tx*
      (let ((tmp-name (cdr (assoc name *tx-modified-list*)))) 
        (unless tmp-name
          (setf tmp-name (clone-temporary name))
          (push (cons name tmp-name) *tx-modified-list*)
          (setf name tmp-name))
        (setf name tmp-name)))
    (let* ((rows (gethash name *db*))
           (update-count 0))
      (setf (gethash name *db*) 
	    (mapcar (lambda (row) 
		      (if (funcall where-predicate row) 
			  (progn
			    (incf update-count)
			    (funcall update-function row)) 
			  row)) 
		    rows))
      (unless *tx* (persist name))
      update-count)))

(defmacro keyset (&rest key/value-pairs)
  "An 'update function' for use in UPDATE. Example: (update :things (where foo) (keyset :/bar 1 :/baz 2))"
  (let (first
        second
        ops
        (row (gensym "ROW"))) 
    (loop 
       (if (consp key/value-pairs)
           (setf first (pop key/value-pairs))
           (return `(lambda (,row) ,@(nreverse ops) ,row)))
       (if (consp key/value-pairs)
           (setf second (pop key/value-pairs))
           (error "Odd number of key/value arguments."))
       (push `(setf (getf ,row ,first) ,second) ops))))

;; to do
;; (defmacro keydel (&rest keys))

(defun del (name where-predicate)
  "Usage: (del :foo (where ...))"
  (with-lock
    (when *tx*
      (let ((tmp-name (cdr (assoc name *tx-modified-list*)))) 
        (unless tmp-name
          (setf tmp-name (clone-temporary name))
          (push (cons name tmp-name) *tx-modified-list*)
          (setf name tmp-name))
        (setf name tmp-name)))
    (let* ((rows (gethash name *db*))
           (row-count (length rows)))
      (prog1 (- row-count (length 
                           (setf (gethash name *db*) 
                                 (remove-if where-predicate rows))))
        (unless *tx* (persist name))))))

(defmacro where-on (row-binding expression)
  "Like WHERE, except giving direct access to the row, bound to
ROW-BINDING. Unlike WHERE, EXPRESSION runs unmodified.
Example: (select :users (where-on u (equal (:/age u) 30)))"
  `(lambda (,row-binding) ,expression))

(defmacro where (expression)
  (let ((row (gensym "ROW"))
	bindings) 
    (labels ((d (x)
	       (typecase x
		 (cons (mapcar #'d x))
		 (keyword (if (char= (char (symbol-name x) 0) #\/)
			      (cadr (or (assoc x bindings)
                                        (car (push (cons x (list (gensym "VAR") `(getf ,row ,x))) bindings))))
			      x))
		 (t x))))
      (setf expression (d expression)))
    `(lambda (,row)
       (let (,@(mapcar #'cdr bindings))
	 ,expression))))

(defmacro defattributes (&body body)
  "Defines getter functions and validation functions for row attributes. Example:
 (defattributes
   :/item-id
   :/title (lambda (x) (<= 1 (length x) 200))) 
This would create the getter functions :/item-id and :/title, plus the validation function valid-title-p."
  (let (result
	current)
    (loop
       (if body
	   (setf current (pop body))
	   (return `(progn ,@(reverse result))))
       (push `(defun ,current (row) (getf row ,current)) result)
       (unless (keywordp (first body))
	 (let ((fun (pop body)))
	   (push `(defun ,(intern (& "VALID-" (subseq (symbol-name current) 1) "-P")) (x) 
                    (let ((f ,fun)) (funcall f x)))
		 result))))))

(defun clone-temporary (name)
  (let ((tmp-name (gensym "TMP")))
    (setf (gethash tmp-name *db*) (copy-list (gethash name *db*)))
    tmp-name))

(defmacro with-tx (&body body)
  "Execute a transaction as follows:
1. Every command that writes executes against a temporary table.
2. Back up the real target tables on disk.
3. Swap the target tables with the temporary ones in memory and persist.
4. If 3 does not complete, reverse the original swaps with the temporary tables and restore the backups as primary.
5. Delete backup files and temporary tables."
  `(with-lock 
     (let ((*tx-modified-list* nil)
           (*tx* t))
       (unwind-protect 
            (multiple-value-prog1 
              (progn ,@body)
              (let (backup-files
                    finished) 
                (unwind-protect
                     (progn 
                       (loop for (name . tmp-name) in *tx-modified-list* do
                            (let ((backup-filename (& (make-filename name) "-backup")))
                              (push backup-filename backup-files)
                              (persist name backup-filename)))
                       (loop for (name . tmp-name) in *tx-modified-list* do
                            (rotatef (gethash name *db*) (gethash tmp-name *db*))
                            (persist name))
                       (setf finished t))
                  (if finished
                      (dolist (file backup-files)
                        (delete-file (merge-pathnames *db-path* file)))
                      (progn
                        (loop for (name . tmp-name) in *tx-modified-list* do
                             (rotatef (gethash name *db*) (gethash tmp-name *db*)))
                        (dolist (file backup-files)
                          (rename-file (merge-pathnames *db-path* file) 
                                       (merge-pathnames *db-path* (subseq file 0 (- (length file) #.(length "-backup"))))))
                        (error "tx failed"))))))
         (loop for (name . tmp-name) in *tx-modified-list* do
              (remhash tmp-name *db*))))))
