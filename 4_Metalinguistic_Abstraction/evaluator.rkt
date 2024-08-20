#lang racket

(require racket/mpair)

(provide
  eval
  setup-environment)

(define apply-in-underlying-scheme apply)


; In the environment model of evaluation, a procedure is always a pair consisting of some code and a pointer
; to an environment. Procedures are created in one way only: by evaluating a λ-expression.
; This produces a procedure whose code is obtained from the text of the λ-expression and whose environment
; is the environment in which the λ-expression was evaluated to produce the procedure. 

(define (eval exp env)
  (display 'eval:) (display exp) (newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)        (lookup-variable-value exp env))
        ((quoted? exp)          (text-of-quotation exp))
        ((assignment? exp)      (eval-assignment exp env))
        ((definition? exp)      (eval-definition exp env))
        ((if? exp)              (eval-if exp env))
        ((and? exp)             (eval-and exp env))
        ((or? exp)              (eval-or exp env))
        ((lambda? exp)          (make-procedure (lambda-parameters exp)
                                                (lambda-body exp)
                                                env))
        ((begin? exp)           (eval-sequence (begin-actions exp) env))
        ((cond? exp)            (eval (cond->if exp) env))
        ((let? exp)             (eval (let->combination exp) env))
        ((while? exp)           (eval (while->combination exp) env))
        ((application? exp)     (my-apply (eval (operator exp) env)
                                          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


(define (my-apply procedure arguments)
  (display 'my-apply:) (display 'procedure:)  (display procedure)
  (newline) (display 'arguments:) (display arguments) (newline)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)  (eval-sequence (procedure-body procedure)
                                                         (extend-environment
                                                          (procedure-parameters procedure)
                                                          arguments
                                                          (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))


;; Procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))) ;Ex4.1 left to right
        (cons left
              (list-of-values (rest-operands exps) env)))))

;; Conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Booleans
(define (eval-and exp env)
  (define (iter preds)
    (cond ((null? preds) true)
          ((true? (eval (car preds) env)) (iter (cdr preds)))
          (else
            false)))
  (iter (cdr exp)))

(define (eval-or exp env)
  (define (iter preds)
    (cond ((null? preds) false)
          ((true? (eval (car preds) env)) true)
          (else
            (iter (cdr preds)))))
  (iter (cdr exp)))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))


;; Assignments and definitions
(define (eval-assignment exp env)
  (display 'eval-assignment:) (display exp) (newline)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


#|  Representing Expressions    |#

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

; Quotations have the form (quote ⟨text-of-quotation⟩)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; Assignments have the form (set! ⟨var⟩ ⟨value⟩)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; Definitions have the form (define ⟨var⟩ ⟨value⟩) or the form
; (define (⟨var⟩ ⟨parameter1⟩ . . . ⟨parametern⟩)
;    ⟨body⟩)
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; formal parameters
                   (cddr exp))))    ; body

;; lambda expressions are lists that begin with the symbol lambda:
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Conditionals begin with if and have a predicate, a consequent, and an (optional) alternative. If the expression has no alternative part, we provide false as the alternative
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
; to be used by cond->if to transform cond expressions into if expressions
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

; begin packages a sequence of expressions into a single expression
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; We also include a constructor sequence->exp (for use by cond->if) that transforms a sequence into a single expression
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; A procedure application is any compound expression that is not one of the above expression types. The car of the expression is the operator, and the cdr is the list of operands:
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


#|  Derived expressions |#

;; We include syntax procedures that extract the parts of a cond expression, and a procedure cond->if that transforms cond expressions into if expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Transforms let:
;; (let ((v1 e1) ... (vn en)) ⟨body⟩) = ((lambda (v1 ... vn) ⟨body⟩) e1 ... en) or
;; (let name ((v1 e1) ... (vn en)) ⟨body⟩) = (begin (define name (lambda (v1 ... vn) ⟨body⟩)) (name e1 ... en))
;; Note that name is bound within ⟨body⟩ to a procedure whose body is ⟨body⟩ and whose parameters are the variables in ((v1 e1) ... (vn en))
(define (let? exp) (tagged-list? exp 'let))
(define (let-variables exp)
  (define (iter lets)
    (if (null? lets)
      '()
      (cons (caar lets)
            (iter (cdr lets)))))
  (iter (cadr exp)))
(define (let-expressions exp)
  (define (iter lets)
    (if (null? lets)
      '()
      (cons (cadar lets)
            (iter (cdr lets)))))
  (iter (cadr exp)))
(define (let-body exp) (cddr exp))

(define (named-let? exp) (and (tagged-list? exp 'let)
                              (symbol? (cadr exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-variables exp)
  (map car (caddr exp)))
(define (named-let-expressions exp)
  (map cadr (caddr exp)))
(define (named-let-body exp) (cdddr exp))
(define (named-let->combination exp)
  (make-begin
    (list
      (list 'define
            (named-let-name exp)
            (make-lambda (named-let-variables exp)
                        (named-let-body exp)))
      (cons (named-let-name exp) (named-let-expressions exp)))))
        
(define (let->combination exp)
  (if (named-let? exp)
    (named-let->combination exp)
    (cons (make-lambda (let-variables exp)
                      (let-body exp))
          (let-expressions exp))))

;; (while predicate body)
(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (while->combination exp)
  ;(display (while-body exp)) (newline)
  (sequence->exp
    (list
      (list 'define
        (list 'while-iter)
          (make-if (while-predicate exp)
                   (sequence->exp
                    (list
                      (while-body exp)
                      (list 'while-iter)))
                    'false))
      (list 'while-iter))))


#|  Evaluator Data Structures   |#

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Compound procedures are constructed from parameters, procedure bodies, and environments using the constructor make-procedure:
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


#|  Environments  |#

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; Each frame of an environment is represented as a pair of lists: a list of the variables bound in that frame and a list of the associated values
(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (cons val (mcdr frame))))

;; To extend an environment by a new frame that associates variables with values, we make a frame consisting of the list of variables and the list of values, and we adjoin this to the environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; To look up a variable in an environment, we scan the list of variables in the first frame. If we find the desired variable, we return the corresponding element in the list of values. If we do not find the variable in the current frame, we search the enclosing environment, and so on
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-mcar! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-mcar! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))



#|  Running the Evaluator as a Program  |#

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list '< <)
        (list 'display display) ; for debug
        ;⟨more primitives⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; To apply a primitive procedure, we simply apply the implementation procedure to the arguments, using the underlying Lisp system:
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; ;; For convenience in running the metacircular evaluator, we provide a driver loop that models the read-eval-print loop of the underlying Lisp system
; (define input-prompt ";;; M-Eval input:")
; (define output-prompt ";;; M-Eval value:")
; (define (driver-loop)
;   (prompt-for-input input-prompt)
;   (let ((input (read)))
;     (let ((output (eval input the-global-environment)))
;       (announce-output output-prompt)
;       (user-print output)))
;   (driver-loop))
; (define (prompt-for-input string)
;   (newline) (newline) (display string) (newline))
; (define (announce-output string)
;   (newline) (display string) (newline))

; ;; We use a special printing procedure, user-print, to avoid printing the environment part of a compound procedure, which may be a very long
; (define (user-print object)
;   (if (compound-procedure? object)
;       (display (list 'compound-procedure
;                      (procedure-parameters object)
;                      (procedure-body object)
;                      '<procedure-env>))
;       (display object)))
