; David Nixon
; Jeremy Novak

; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
(load "classParser.scm")

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file main-class)
    (scheme->language
     (eval-main-method main-class (interpret-statement-list (parser file) (newenvironment) invalid-return invalid-break invalid-continue invalid-throw main-class '())  main-class '())))) 

(define eval-main-method
  (lambda (class-name environment true-type instance)
    (call/cc
      (lambda (return)
        (interpret-statement-list (cadr (dot-lookup-function 'main (caadr (lookup class-name environment true-type instance)) (cadadr (lookup class-name environment true-type instance)) true-type instance))  
                                  environment return invalid-break invalid-continue invalid-throw true-type (lookup true-type environment true-type instance))))))  

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw true-type instance)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw true-type instance) return break continue throw true-type instance))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw true-type instance)
    (cond
      ((eq? 'class (statement-type statement)) (interpret-class statement environment true-type instance))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw true-type instance))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment true-type instance))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall (eval-funcall statement environment throw true-type instance) environment))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw true-type instance))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw true-type instance))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw true-type instance))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw true-type instance))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw true-type instance))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw true-type instance))
      ((eq? 'try (statement-type statement)) (begin (set-box! throwenv environment)(interpret-try statement environment return break continue throw) true-type instance))
      (else (myerror "Unknown statement: " (statement-type statement))))))


; adds instance of class to state
(define interpret-constructor 
  (lambda (statement environment true-type instance)
    (cond
      ((exists? (cadr statement) environment true-type instance)  (boxlist (cadr (caddr (lookup (cadr statement) environment true-type instance)))) environment)
      (else (error "Type not declared")))))

;helper method for boxing every atom in a list
(define boxlist
  (lambda (list)
    (cond
      ((null? list) '())
      (else (cons (box (car list)) (boxlist (cdr list))))))) ;should this be set-box! instead of "box" ?
    
; Executes the function then returns the state
(define interpret-funcall
  (lambda (function environment)
    environment))

; Evaluates a function and returns the value
(define eval-funcall
  (lambda (statement environment throw true-type instance)
    (call/cc
      (lambda (return)
        (interpret-statement-list (function-body statement environment true-type instance) (push-function-frame statement environment throw true-type instance) return invalid-break invalid-continue throw
                                  (new-type statement environment true-type instance) (lookup (cadr statement)) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Class Closure Functions;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Creates a new class closure based on the current class fields and functions, and those of all superclasses
(define interpret-class
  (lambda (statement environment true-type instance)
    (insert (cadr statement) (interpret-class-super statement '((()())(()())) environment (cadr statement) instance) environment )))

; creates the closure of a class
; the first case is if the class does not have a parent class to inherit from
; the second case adds the functions and fields of the new class to the functions and fields it is inheriting from the parent class
(define interpret-class-super
  (lambda (statement closure environment true-type instance)
    (if (null? (caddr statement))
        (cons '() (cons (car (class-closure-list (cadddr statement) closure true-type instance)) (cadr (class-closure-list (cadddr statement) closure true-type instance))))
        (cons (cadr (caddr statement)) (cons (car (class-closure-list (cadddr statement) (cdr (lookup (cadr (caddr statement)) environment true-type instance)) true-type instance))
                                             (list (cadr (class-closure-list (cadddr statement) (cdr (lookup (cadr (caddr statement)) environment true-type instance)) true-type instance)))))))) 

(define class-closure-list
  (lambda (statement-list closure true-type instance)
     (if (null? statement-list)
         closure
        (class-closure-list (cdr statement-list) (class-closure-statement (car statement-list) closure true-type instance) true-type instance))))

;because we are not handling the the extra challenge of static functions and variables, we handle them the same
(define class-closure-statement
  (lambda (statement closure true-type instance)
    (cond
      ((or (eq? 'function (statement-type statement)) (eq? 'static-function (statement-type statement)))
       (insert-func-closure (get-function-name statement) (get-function-closure statement true-type) closure))
      ((or (eq? 'var (statement-type statement)) (eq? 'static-var (statement-type statement))) (insert-var (cadr statement) (caddr statement) closure)   ))))


;the next two functions separate the functions and fields within classes
(define insert-func-closure
  (lambda (name func closure)
    (cons (cons (cons name (caar closure)) (list (cons func (cadar closure)))) (list (cdr closure)))))

(define insert-var
  (lambda (name value closure)
    (cons (car closure) (list (cons (cons name (caadr closure)) (list (cons value (cadadr closure))))))))
 

     
; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw true-type instance)
    (return (eval-expression (get-expr statement) environment throw true-type instance))))

; Adds the function binding to the environment
(define interpret-function
  (lambda (statement environment true-type instance)
    (insert (get-function-name statement) (get-function-closure statement true-type) environment)))


; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw true-type instance)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (box (eval-expression (get-declare-value statement) environment throw true-type instance)) environment)
        (insert (get-declare-var statement) (box 'novalue) environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw true-type instance)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw true-type instance) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw true-type instance)
    (cond
      ((eval-expression (get-condition statement) environment throw true-type instance) (interpret-statement (get-then statement) environment return break continue throw true-type instance))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw true-type instance))
      (else environment))))


; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw true-type instance)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw true-type instance)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw true-type instance))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw true-type instance)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))) true-type instance)))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw true-type instance)
    (throw (box (eval-expression (get-expr statement) environment throw true-type instance)) (push-frame (unbox throwenv)))))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block true-type instance)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw))))
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list
                                                 (get-body catch-statement)
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return
                                                 (lambda (env2) (break (pop-frame env2)))
                                                 (lambda (env2) (continue (pop-frame env2)))
                                                 (lambda (v env2) (throw v (pop-frame env2)))) true-type instance)
                                     return break continue throw true-type instance) ))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw true-type instance)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw true-type instance) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw true-type instance))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw true-type instance))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block true-type instance)))(interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw true-type instance)
                          return break continue throw true-type instance))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw true-type instance)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((eq? (operator expr) 'new) (interpret-constructor expr environment true-type instance))
      ((not (list? expr)) (unbox (lookup expr environment true-type instance))) ;for evalaluating variables
      ((eq? 'new (cadr expr)) (interpret-constructor expr environment))
      ((eq? 'funcall (operator expr)) (eval-funcall expr environment throw true-type instance)) ; interpret-funcall is not implemented yet
      ((eq? 'dot (operator expr)) (dot-lookup-field-obj expr environment throw true-type instance))
      (else (eval-operator expr environment throw true-type instance)))))

(define dot-lookup-field-obj ;takes the expr "(dot objname varname)" and returns the value of the var
  (lambda (expr environment throw true-type instance)
    (if (eq? (cadr expr) 'this)
        (dot-lookup-field (caddr expr) (car (caddr (lookup true-type environment true-type instance))) (cadr (caddr instance)) throw true-type instance)
        (dot-lookup-field (caddr expr) (car (caddr (lookup (car (lookup (cadr expr) environment true-type instance)) environment true-type instance))) (cadr (lookup (cadr expr) environment true-type instance))  environment throw true-type instance))))

(define dot-lookup-field
  (lambda (var-name class-field-names obj-field-values throw true-type instance)
    (cond
      ((> (length obj-field-values) (length class-field-names) ) (dot-lookup-field var-name class-field-names (cdr obj-field-values) throw true-type instance))
      ((null? obj-field-values) (myerror "Field not found:" var-name))
      ((eq? var-name (car class-field-names)) (car obj-field-values))
      (else (dot-lookup-field var-name (cdr class-field-names) (cdr obj-field-values) throw true-type instance)))))


(define dot-lookup-function-obj ;takes the expr "(dot objname funcname)" and returns the function closure
  (lambda (expr environment true-type instance)
    (if (eq? (cadr expr) 'this)
        (dot-lookup-function (caddr expr) (caadr (lookup true-type environment true-type instance)) (cadadr (lookup true-type environment true-type instance))  true-type instance)   
        (dot-lookup-function (caddr expr) (caadr (lookup (car (lookup (cadr expr) environment true-type instance)) environment true-type instance))
                             (cadadr (lookup (car (lookup (cadr expr) environment true-type instance)) environment true-type instance)) true-type instance))))

(define dot-lookup-function
  (lambda (name func-names func-values  true-type instance)
    (cond
      ((null? func-names) (myerror "Function not found:" name))
      ((eq? name (car func-names)) (car func-values))
      (else (dot-lookup-function name (cdr func-names) (cdr func-values) true-type instance)))))

(define eval-operator
  (lambda (expr environment throw true-type instance)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw true-type instance)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw true-type instance)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw true-type instance) environment throw true-type instance)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw true-type instance))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw true-type instance)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define throwenv (box '()))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-function-name operand1)
(define get-function-closure
  (lambda (statement class)
    (cons (cons 'this (operand2 statement)) (cons (operand3 statement) (list class)))))
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Function helper methods
;;;;;;;;;;;;;;;;;;;;;;;;;

; Defines the main method call
(define main-method '(funcall main))

(define function-body
  (lambda (statement environment true-type instance)
    (cadr (dot-lookup-function (cadr statement) environment true-type instance))))

(define parameters
  (lambda (statement instance)
    (cons (instance (cddr statement)))))

(define parameter-names
  (lambda (statement environment true-type instance)
    (car (dot-lookup-function (cadr statement) environment true-type instance))))

(define new-type
  (lambda (statement environment true-type instance)
    (caddr (dot-lookup-function (cadr statement) environment true-type instance))))

; Evaluates the parameters
(define parameter-values
  (lambda (parameters environment throw instance)
    (if (null? parameters)
         '()
         (cons (box (eval-expression (car parameters) environment throw)) (parameter-values (cdr parameters) environment throw)))))

; Creates frame where the names are parameter names and values are parameter values
(define function-frame
  (lambda (statement environment throw true-type instance)
    (if (matching-parameters? (parameter-names statement environment true-type instance) (parameter-values (parameters statement instance) environment throw))
      (cons (parameter-names statement environment true-type instance) (cons (parameter-values (parameters statement instance) environment throw) '()))
      (myerror "Mismatched paramters"))))

; Gets the static link for a function
(define get-static-link
  (lambda (name environment)
    (cond
      ((null? environment) (myerror "function not found: " name)) 
      ((exists-in-list? name (caar environment)) environment)
      (else (get-static-link name (pop-frame environment))))))

; Verifies that the parameter count matches the function call
(define matching-parameters?
  (lambda (names values)
    (if (null? names)
      (null? values)
      (matching-parameters? (cdr names) (cdr values)))))

; Returns the function frame and the environment of the static link
(define push-function-frame
  (lambda (statement environment throw true-type instance)
    (cons (function-frame statement environment throw true-type instance) (get-static-link (cadr statement) environment))))

;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment true-type instance)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment) true-type instance)))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment true-type instance)
    (lookup-variable var environment)))

; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment true-type instance)
    (if (exists? var environment true-type instance)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (begin (set-box! (car vallist) (scheme->language val)) vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

;;;;;;;;;;;;;;;;;;;
; ERROR FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;

(define invalid-return
  (lambda (env)
    (myerror "Return called outside a function")))

(define invalid-break
  (lambda (env)
    (myerror "Break used outside of loop")))

(define invalid-continue
  (lambda (env)
    (myerror "Continue used outside of loop")))

(define invalid-throw
  (lambda (v env)
    (myerror "Uncaught exception thrown")))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
  (call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(interpret "test.txt" 'A)
