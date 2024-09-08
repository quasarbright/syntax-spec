#lang racket

(provide (all-defined-out))

(require syntax-spec
         "compile-with-check.rkt"
         (for-syntax syntax/parse))

;;
;; Core syntax
;;

(syntax-spec
  (binding-class term-variable)
  (binding-class relation-name)
  
  (extension-class term-macro)
  (extension-class goal-macro)

  (nonterminal term
    #:allow-extension term-macro
    
    n:number
    x:term-variable
    ((~literal quote) ())
    ((~literal cons) t1:term t2:term))

  (nonterminal goal
    #:description "miniKanren goal"
    #:allow-extension goal-macro

    succeed
    fail
    
    (== t1:term t2:term)

    (disj2 g1:goal g2:goal)
    (conj2 g1:goal g2:goal)
  
    (fresh1 (x:term-variable) b:goal)
    #:binding (scope (bind x) b)

    (r:relation-name t:term ...+)))


;;
;; Interface macros
;;

(syntax-spec
  (host-interface/definition
    (core-defrel (name:relation-name x:term-variable ...) g:goal)
    #:binding [(export name) (scope (bind x) g)]
   
    #:lhs
    [(record-relation-arity! #'name (length (attribute x)))
     #'name]

    #:rhs
    [#'(compile-defrel (x ...) g)])

  (host-interface/expression
    (core-run n:expr q:term-variable g:goal)
    #:binding (scope (bind q) g)
    #'(compile-run n q g)))


;;
;; Surface syntax
;;

(define-syntax list
  (term-macro
   (syntax-rules ()
     [(list) '()]
     [(list a rest ...) (cons a (list rest ...))])))

(define-syntax disj
  (goal-macro
   (syntax-rules ()
     ((disj) fail)
     ((disj g) g)
     ((disj g0 g ...) (disj2 g0 (disj g ...))))))

(define-syntax conj
  (goal-macro
   (syntax-rules ()
     ((conj) succeed)
     ((conj g) g)
     ((conj g0 g ...) (conj2 g0 (conj g ...))))))

(define-syntax fresh
  (goal-macro
   (syntax-rules ()
     ((fresh () g ...) (conj g ...))
     ((fresh (x0 x ...) g ...)
      (fresh1 (x0)
              (fresh (x ...)
                g ...))))))

(define-syntax conde
  (goal-macro
   (syntax-rules ()
     ((conde (g ...) ...)
      (disj (conj g ...) ...)))))


;;
;; Surface syntax for interface macros
;;

(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (core-defrel (name x ...) (conj g ...))]))

(define-syntax run
  (syntax-rules ()
    [(run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== (list x0 x ...) q) g ...))]
    [(run n q g ...)
     (core-run n q (conj g ...))]))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))