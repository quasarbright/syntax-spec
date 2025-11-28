#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/transformer syntax/parse "../../main.rkt")
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

(begin-for-syntax
  (define-syntax-class lambda-id
    (pattern (~or (~literal lambda) (~literal #%plain-lambda)))))

(syntax-spec
  (binding-class method-var
    #:reference-compiler method-reference-compiler)
  (binding-class field-var
    #:reference-compiler field-reference-compiler)

  (nonterminal/exporting class-form
    #:allow-extension racket-macro
    (field name:field-var ...)
    #:binding [(export name) ...]
    ((~literal define-values) (m:method-var)
                              (lambda:lambda-id (arg:racket-var ...) body:racket-body ...))
    #:binding [(export m) (scope (bind arg) ... (import body) ...)]

    ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
    #:binding (export-syntaxes x ... e)

    ((~literal begin) e:class-form ...)
    #:binding [(re-export e) ...]

    e:racket-expr))
]

We create separate binding classes for methods and fields because they will behave differently when used in the class body, hence the different reference compilers.

The @racket[class-form] nonterminal is for forms that will appear in the class body. We support field declarations, method definitions, macro definitions, @racket[begin], and arbitrary Racket expressions. The Racket expressions will run once, in the constructor.

If we wanted to allow arbitrary Racket definitions in the class body as well, we could use @racket[racket-body] instead of @racket[racket-expr] like we do in the body of a method. However, since we have a production for @racket[define-values], that production will commit any time it sees a form starting with @racket[define-values], so any definition will be treated like a method definition. Definitions of non-functions will error as bad syntax.

The key piece that allows us to re-interpret Racket syntax is @racket[#:allow-extension racket-macro]. This means built-in macros like @racket[define] and @racket[define-syntax], which eventually translate to @racket[define-values] and @racket[define-syntaxes] respectively, can be used in our DSL. In fact, any definition forms can be used, even fancy ones like @racket[match-define], as long as they eventually expand down to @racket[define-values]. Our grammar has to match on fully expanded Racket syntax, which is what we're doing here.

As an example, let's think about how a method definition might expand:

@racketblock[
(define (add2 x) (+ x 2))
~>
(define-values (add2) (lambda (x) (+ x 2)))
]

This is exactly what our method production looks for.

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
(send-rt foo 'add (list 2))
]

We'll get into how @racket[this] works soon.

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

First, we splice @racket[begin]s so we get a flat list of top-level forms (field declarations, method definitions, and Racket expressions). Then, we group these top-level forms based on their type. Finally, we compile these forms to Racket.

Here is how we splice @racket[begin]s:

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
         [_ (cons this-syntax (splice-begins #'rest-exprs))])])))
]

We just flatten everything into a list of class-level forms.

Here is the code for grouping up class-level forms:

@racketblock[
(begin-for-syntax
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

It's just straightforward syntax manipulation, taking advantage of @racket[syntax-parse]'s powerful patterns.

For compilation, we can start with the method reference compilers:

@racketblock[
#:escape unracket
(begin-for-syntax
  (define method-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        #'(lambda args (send this name . args))]))))
]

Inside of the @racket[class] body, if you reference a method directly, it is just a variable that refers to a procedure that invokes the method.

@racketblock[
#:escape unracket
(begin-for-syntax
  (define-persistent-symbol-table field-index-table)

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


Field references access or mutate the object's field vector. We use a global persistent symbol table to map field names to indices for convenience. This is safe due to hygiene.

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

This is a lot, so let's go through it step by step.

First, we check for duplicate method names and associate fields with indices.

@racketblock[
(check-duplicate-method-names (attribute method-name))
(for ([field-name (attribute field-name)]
      [field-index (in-naturals)])
(symbol-table-set! field-index-table field-name field-index))
]

Next comes the generated syntax:

@racketblock[
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
    cls)
]

We recursively define the methods, constructor, and the class itself, and return the class when we're done.

The methods are a mapping from method name to procedure. We set up @racket[this] to refer to the first argument, which is the instance of the class, and the rest of the arguments are those passed in from @racket[send].
@;TODO is it safe to use the symbols? Are they actually renamed the right way? Or do you have to gensym.

The constructor takes in values for the fields, creates an instance and binds it to @racket[this], runs the class-level Racket expressions, and finally returns the instance.

Finally, let's define the syntax parameter for @racket[this]:

@racketblock[
(define-syntax-parameter this
  (make-expression-transformer
   (syntax-parser
     [_ (raise-syntax-error 'this "used outside of a class" this-syntax)])))
]

That's it. We now have a simple class DSL. To summarize the key points:

@itemlist[
  @item{We have productions with literals of fully expanded Racket forms to detect definitions and re-interpret them.}
  @item{We use @racket[#:allow-extension racket-macro] to expand any Racket definitions down to @racket[define-values] and @racket[define-syntaxes].}
  @item{We use @racket[racket-body] for Racket definitions or expressions, and @racket[racket-expr] for just expressions.}
  @item{We use a syntax parameter for @racket[this], which gets set in the compiler to refer to the instance. But when used outside of a class body, it is a syntax error.}
]
