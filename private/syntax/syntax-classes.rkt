#lang racket/base

(provide
 ;; Syntax class matching identifiers with a `:`
 ;;
 ;; Attributes:
 ;;   var - part before the `:`
 ;;   ref - part after the `:`
 ref-id

 ;; Syntax class matching identifiers with no `:`
 nonref-id

 ;; Splicing syntax class matching production specs with optional
 ;; #:binding binding specs
 ;;
 ;; General form:
 ;;   (~seq sspec (~optional (~seq #:binding bspec)))
 ;;
 ;; Attributes:
 ;;   sspec
 ;;   bspec
 ;;   form-name - when sspec is an s-expression beginning with a nonref-id,
 ;;                 this attribute is bound to that id.
 production-spec

 extclass-spec
 )


(require
  racket/string
  racket/list
  syntax/parse
  syntax/srcloc
  racket/syntax)

(define-syntax-class ref-id
  #:description "pattern variable with annotation"
  (pattern name:id #:when (has:? #'name)
           #:do [(define-values (var ref) (split: #'name))]
           #:attr var var
           #:attr ref ref))
  
(define-syntax-class nonref-id
  (pattern name:id #:when (not (has:? #'name))))


(define-splicing-syntax-class production-spec
  (pattern (~seq sspec:sspec (~optional (~seq #:binding bspec)))
           #:attr form-name (attribute sspec.form-name)))

(define-syntax-class sspec
  (pattern (form-name:nonref-id . _))
  (pattern _ #:attr form-name #f))

  
(define (has:? id)
  (string-contains?
   (symbol->string (syntax-e id))
   ":"))
  
(define (split: id)
  (define str (symbol->string (syntax-e id)))
  
  (define strs
    (string-split str ":" #:trim? #f))

  (define loc1
    (update-source-location
     id
     #:span (string-length (first strs))))

  (define loc2
    (let ([loc2-offset (+ (string-length (first strs)) 1)])
      (update-source-location
       id
       #:column (+ (syntax-column id) loc2-offset)
       #:position (and (syntax-position id)
                       (+ (syntax-position id)
                          loc2-offset))
       #:span (string-length (second strs)))))
  
  (values (datum->syntax id (string->symbol (first strs)) loc1 id)
          (datum->syntax id (string->symbol (second strs)) loc2 id)))


(define-splicing-syntax-class extclass-spec
  (pattern v:id #:attr [classes 1] (list #'v))
  (pattern (classes:id ...)))