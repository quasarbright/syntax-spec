#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/parse "../../main.rkt")
          scribble/example
          racket/sandbox)
@(define eval (make-base-eval '(require racket racket/stxparam syntax/transformer (for-syntax racket syntax/transformer))))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))
@repl[
#:hidden #t
(require "tests/dsls/miniclass/class.rkt")
]

@title[#:tag "miniclass"]{Advanced Tutorial: Re-interpreting Racket Syntax}

It is possible to create a DSL that re-interprets Racket forms like @racket[define] to behave differently in the context of that DSL. In Racket's @racket[class] DSL, procedure definitions are interpreted as method definitions. syntax-spec makes it easier to create these DSLs, but some special care and knowledge is required. In this tutorial, we'll create a small version of Racket's @racket[class] DSL.

Here is an example of using the language we'll build:

@repl[
(define foo%
  (class
    (field x)
    (define (add y) (+ x y))))
(define foo (new foo% 1))
(send foo add 2)
]

@section[#:tag "miniclass-expander"]{Expander}

Let's start by defining the grammar:

@racketmod[
racket
(require syntax-spec
         racket/stxparam
         syntax/transformer
         (for-syntax racket/list
                     syntax/parse
                     syntax/transformer))

(define-syntax-parameter this
  (make-expression-transformer
   (syntax-parser
     [_ (raise-syntax-error 'this "used outside of a class" this-syntax)])))

(begin-for-syntax
  (define-syntax-class lambda-id
    (pattern (~or (~literal lambda) (~literal #%plain-lambda)))))

(syntax-spec
  (binding-class method-var
    #:description "method name"
    #:reference-compiler method-reference-compiler)
  (binding-class field-var
    #:description "field name"
    #:reference-compiler field-reference-compiler)

  (nonterminal/exporting class-form
    #:allow-extension racket-macro
    (field name:field-var ...)
    #:binding [(export name) ...]
    ((~literal define-values) (m:method-var) (lambda:lambda-id (arg:id ...) body:racket-expr ...))
    #:binding (export m)

    ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
    #:binding (export-syntaxes x ... e)

    ((~literal begin) e:class-form ...)
    #:binding [(re-export e) ...]

    e:racket-body
    #:binding (re-export e)))
]

Our host interface will be called @racket[class] and its body will consist of @racket[class-form]s. A @racket[class-form] is either a field declaration, a method definition, a macro definition, a (splicing) @racket[begin], or a plain old Racket expression.

Based on our production for method definitions, it seems like method definitions will have to look like this:

@racketblock[
(define-values (foo) (lambda (y) (+ x y)))
]

However, since @racket[define] is a macro that expands to a usage of @racket[define-values] and potentially @racket[lambda], we use @racket[#:allow-extension racket-macro] to expand macros like @racket[define] away into forms like @racket[define-values]. In the same sense that our productions are the core forms of our DSL and we can have DSL macros in terms of them, we are using the core forms of Racket and all Racket macros can be used as DSL macros.

@section{Procedural Layer}

Next, let's think about how we can actually implement classes. We'll define two data types:

@racketblock[
(struct class-info [methods constructor])
(struct object [fields class])
]

A @racket[class-info] represents a class itself. @racket[methods] is a hash table mapping method names (symbols) to method implementations. Methods will be implemented as functions whose first argument is @racket[this] and subsequent arguments are the explicit arguments to the method. @racket[constructor] takes in an argument for each field and returns an instance of the class, which is an @racket[object].

A @racket[object] represents an instance of a class. It has its fields as a vector of values (in the same order as the constructor), and the @racket[class-info] containing its methods.

Since methods are class-specific and take in @racket[this] as an argument, we can make one re-usable @racket[class-info] for the class and every instance will get a reference to it. But fields are instance-specific so each @racket[object] needs its own. The reason we use a symbol mapping for methods and a vector for fields is because fields are lexical and resolved hygienically, and methods use (non-hygienic) symbolic equality via @racket[send]. During compilation, fields will be associated with their index whereas methods will just be associated with their symbolic name.

Now that we've defined our data types, we can implement some utilities:

@racketblock[
(define (new cls . fields)
  (apply (class-info-constructor cls) fields))

(define-syntax send
  (syntax-parser
    [(_ obj:expr method-name:id arg:expr ...)
     #'(send-rt obj 'method-name (list arg ...))]
    [(_ obj:expr method-name:id . args)
     #'(send-rt obj 'method-name args)]))

(define (send-rt obj method-name args)
  (let* ([cls (object-class obj)]
         [method (hash-ref (class-info-methods cls) method-name (lambda () (error 'send "unknown method ~a" method-name)))])
    (apply method obj args)))
]

Recall our example:

@repl[
(define foo%
  (class
    (field x)
    (define (add y) (+ x y))))
(define foo (new foo% 1))
(send foo add 2)
]

That would look something like this when compiled:

@repl[
(define foo%
  (class-info (hash 'add (lambda (this-arg y)
                           (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                             (+ (vector-ref (object-fields this) 0) y))))
              (lambda (x) (object (vector x) foo%))))
(define foo (new foo% 1))
(send foo add 2)
]

@section[#:tag "miniclass-compiler"]{Compiler}

Now let's implement this compilation.

@racketblock[
(syntax-spec
  (host-interface/expression
    (class e:class-form ...)
    #:binding (scope (import e) ...)
    (define-values (defns fields exprs) (group-class-decls (splice-begins (attribute e))))
    (compile-class-body defns fields exprs)))
]

First, we splice begins so we get a flat list of top-level forms (field declarations, method definitions, and Racket expressions). Then, we group these top-level forms based on their type. Finally, we compile these forms to Racket.

Here is the code for grouping up top-level forms:

@racketblock[
(begin-for-syntax
  (code:comment2 "splices begins (recursively), returns flattened list of exprs.")
  (define (splice-begins exprs)
    (syntax-parse exprs
      [() this-syntax]
      [(expr . rest-exprs)
       (syntax-parse #'expr
         #:literals (begin)
         [(begin e ...)
          (splice-begins (append (attribute e) #'rest-exprs))]
         [_ (cons this-syntax (splice-begins #'rest-exprs))])]))

  (define (group-class-decls exprs)
    (syntax-parse exprs
      #:literals (define-values define-syntaxes field)
      [((~alt (~and defn (define-values . _))
              (code:comment "ignore because they don't end up in the generated code")
              (~and stx-defn (define-syntaxes . _))
              (~and field-decl (field . _))
              expr)
        ...)
       (values (attribute defn)
               (attribute field-decl)
               (attribute expr))])))
]

It's just straightforward syntax manipulation.

For compilation, we can start with reference compilers:

@racketblock[
#:escape unracket
(begin-for-syntax
  (define-persistent-symbol-table field-index-table)

  (define method-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        #'(lambda args (send this name . args))])))

  (define field-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        (let ([idx (symbol-table-ref field-index-table #'name)])
          #`(vector-ref (object-fields this) #,idx))])
     (syntax-parser
       [(_ name:id rhs)
        (let ([idx (symbol-table-ref field-index-table #'name)])
          #`(vector-set! (object-fields this) #,idx rhs))]))))
]

Inside of the @racket[class] body, if you reference a method directly, it is just a variable that refers to a procedure that invokes the method. And field references access or mutate the object's field vector. We use a global persistent symbol table to map field names to indices for convenience.

Now let's compile top-level forms:

@racketblock[
#:escape unracket
(begin-for-syntax
  (define (compile-class-body defns fields exprs)
    (syntax-parse (list defns fields exprs)
      #:literals (define-values field)
      [(((define-values (method-name:id) (_ (method-arg:id ...) method-body:expr ...)) ...)
        (code:comment "only 1 field definition allowed")
        ((~optional (field field-name:id ...) #:defaults ([(field-name 1) null])))
        (expr ...))
       (check-duplicate-method-names (attribute method-name))
       (for ([field-name (attribute field-name)]
             [field-index (in-naturals)])
         (symbol-table-set! field-index-table field-name field-index))
       #'(letrec ([methods
                   (make-immutable-hash
                    (list
                     (cons 'method-name
                           (lambda (this-arg method-arg ...)
                             (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                               method-body
                               ...)))
                     ...))]
                  [constructor
                   (lambda (field-name ...)
                     (let ([this-val (object (vector field-name ...) cls)])
                       (syntax-parameterize ([this (make-variable-like-transformer #'this-val)])
                         (void)
                         expr
                         ...)
                       this-val))]
                  [cls
                   (class-info methods constructor)])
           cls)]))

  (define (check-duplicate-method-names names)
    (let ([duplicate (check-duplicates names #:key syntax->datum)])
      (when duplicate
        (raise-syntax-error #f "a method with same name has already been defined" duplicate)))))
]

@;TODO host interface
@;TODO this
@;TODO compiler
