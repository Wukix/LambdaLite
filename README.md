LambdaLite
==========

A functional, relational database in about 250 lines of Common Lisp

SQL. NoSQL. ORMs. Key-value stores. There are a variety of approaches available for dealing with data. LambdaLite might be called functional and relational, for lack of better terms. The "relational" part is straightfoward: data is organized into tables. The "functional" part comes from a break with traditional SQL-style query languages: Lisp function closures are used to express queries over in-memory, in-process Lisp data.

## Lisp Plug
People often ask "why Lisp?" It's hard to sum up all the benefits of Lisp in a few words. However, what LambdaLite does would be impossible in most languages. LambdaLite's `where` clauses are macros that seek out '/'-prefixed keywords and replace them with row attribute references. The resulting expression becomes a function closure that is compiled by most Lisp implementations into native code. Is that cool or what?

## Motivation
Consider the following SQL query:

    "SELECT * FROM USERS WHERE UPPER(name) = 'BOB'"
A few problems are apparent:
* The UPPER function is part of a separate language with its own syntax and semantics
* The whole query is represented as a string in our application language, so it misses out on highlighting, completion, validation, etc.
* Parameterization of 'BOB' into a variable lacks elegance in most SQL client libraries

Under LambdaLite, we could instead write:

    (select :users (where (equal (string-upcase :/name) "BOB")))
Why is this useful?
* It's actual Lisp syntax: everything is a Lisp function (e.g. string-upcase) or macro
* "BOB" can be parameterized natively by any Lisp variable
* The where clause forms a compiled function closure with the full expressiveness of Lisp

Notice that :/name begins with a slash; this is to distinguish it as a row attribute instead of an ordinary Lisp keyword.

## Getting Started
LambdaLite is schemaless for flexibility. Rather than defining tables themselves, you define attributes that can be used on any table, using `defattributes` like so:

    (defmacro str-member (&rest strings)
      `(lambda (x) (member x '(,@strings) :test #'string=)))

    (defattributes
      :/ticket-id
      :/title (lambda (x) (<= 1 (length x) 200))
      :/ticket-type (str-member "defect" "enhancement" "question")
      :/mocl-version (str-member "14.08" "14.05" "14.02" "13.08" "13.06" "n/a")
      :/target-os (str-member "iOS" "Android" "OS X" "n/a")
      :/dev-os (str-member "Mac OS X" "Linux 32-bit" "Linux 64-bit" "n/a")
      :/description (lambda (x) (<= 1 (length x) 64000))
      :/created-date
      :/created-by
      :/modified-date
      :/user-id
      :/display-name (lambda (x) (and x (<= 3 (length x) 20) (ppcre:scan "^[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]+$" x))))

This will define getter functions that can be used like `(:/ticket-id row)` as well as validation functions like `(valid-title-p title)`.

## Example Session
    (require :lambdalite)
    (use-package :lambdalite)
    (load-db "~/db/")
    (insert :cars '(:/car-id 1 :/make "Honda" :/color "blue")
                  '(:/car-id 2 :/make "Ford" :/color "red"))
      => 2
    (select :cars (where (equal :/color "red")))
      => ((:/CAR-ID 2 :/MAKE "Ford" :/COLOR "red"))
    (defmacro str-member (&rest strings)
      `(λ (x) (member x '(,@strings) :test #'string=)))
    (defattributes
      :/car-id #'integerp 
      :/make #'stringp 
      :/color (str-member "red" "green" "blue"))
    (valid-color-p "asdf")
      => nil
    (dolist (row (select :cars)) 
      (format t "Make: ~A, Color: ~A~%" (:/make row) (:/color row)))
      >> Make: Honda, Color: blue
         Make: Ford, Color: red
    (mapcar #':/color (select :cars))
      => ("blue" "red")

## Transactions
LambdaLite provides the `with-tx` macro to wrap transactions, which are executed serially across threads. For example, the following code is safe under a multi-threaded web server like Hunchentoot:

    ;; ... create a new ticket
    (with-tx 
      (let* ((user-id (logged-in-user-id))
             (ticket-id (1+ (length (select :tickets)))))
        (unless user-id 
          (error "Not logged in"))
        (insert :tickets (list :/ticket-id ticket-id :/created-by user-id :/ticket-status "open"))))
Any data commands that are used outside of a with-tx transaction will automatically be treated each individually as separate transactions.

## Caveats
"Do things that don't scale" &mdash; Paul Graham

LambdaLite is completely unscalable &mdash; by design. Don't use it for heavy loads or large data sets. Even medium-sized jobs are a stretch. That's why 'Lite' is in the name.

## Compatibility
* LambdaLite works under SBCL and probably other Lisps
* LambdaLite runs on [mocl](https://wukix.com/mocl) for mobile data storage

## Who is using LambdaLite?
LambdaLite powers the mocl support site at https://wukix.com/support/ with delightfully low query latency on a single Amazon EC2 micro instance.
