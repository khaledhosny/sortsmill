;; -*- mode: bee; coding: utf-8 -*-

;; Copyright (C) 2012 Barry Schwartz
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(define-module (sortsmillff internal-types))

(use-modules
   (sortsmillff pkg-info)
   (rnrs bytevectors)
   (system foreign)
   (srfi srfi-26)                       ; ‘cut’ and ‘cute’.
   )

;;-------------------------------------------------------------------------
;;
;; FIXME: Put these somewhere re-usable.

(define scm_calloc
   (pointer->procedure '*
      (dynamic-func "scm_calloc" (dynamic-link))
      (list size_t)))

(define free
   (pointer->procedure void
      (dynamic-func "free" (dynamic-link))
      (list '*)))

(define _GC_malloc_error
   (pointer->procedure void
      (dynamic-func "scm_memory_error" (dynamic-link))
      (list '*)))

(define _GC_malloc_error_msg
   (string->pointer "GC_malloc"))

(define (GC-malloc-error)
   (_GC_malloc_error _GC_malloc_error_msg))

(define _GC_malloc
   (pointer->procedure '*
      (dynamic-func "GC_malloc" (dynamic-link))
      (list size_t)))

(define (GC_malloc size)
   (let ((ptr (_GC_malloc size)))
      (when (null-pointer? ptr)
         (GC-malloc-error))
      ptr))

(define GC_free
   (pointer->procedure void
      (dynamic-func "GC_free" (dynamic-link))
      (list '*)))

;;-------------------------------------------------------------------------

(export
   %ff:interface-exported?
   ff:interface-exported?
   with-ff:interface-exported
   define-ff:interface
   )

(eval-when (compile load eval)
   (define %ff:interface-exported? (make-fluid #f))

   (define ff:interface-exported?
      (case-lambda
         (() (fluid-ref %ff:interface-exported?))
         ((v) (fluid-set! %ff:interface-exported? (not (not v))))))

   (define (struct-or-union-identifier? obj)
      (and
       (identifier? obj)
       (or
        (eq? (syntax->datum obj) 'struct)
        (eq? (syntax->datum obj) 'union))))

   (define (struct-type-error-msg tag size obj)
      (cond
         ((not (pair? obj))
          (simple-format #f "~S is not a pair" obj))
         ((not (eq? tag (car obj)))
          (simple-format #f "(car ~S) is not ~S" obj tag))
         ((not (bytevector? (cdr obj)))
          (simple-format #f "(cdr ~S) is not a bytevector" obj))
         ((not (= (bytevector-length (cdr obj)) size))
          "bytevector length is wrong")
         (else #f)))

   (define (type-tag-name type-name)
      (string-append "tag-ff:" type-name))

   (define (build-type-related-symbol constructor syntax-context type-name)
      (datum->syntax syntax-context
         (string->symbol (constructor (syntax->datum type-name)))))

   (define (type-tag syntax-context type-name)
      (build-type-related-symbol
         type-tag-name syntax-context type-name))

   (define (type-sizeof-var syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "sizeof-ff:" <>)
         syntax-context type-name))

   (define (struct?-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> "?")
         syntax-context type-name))

   (define (throw-failed-check-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "throw-failed-check-ff:" <>)
         syntax-context type-name))

   (define (check-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "check-ff:" <>)
         syntax-context type-name))

   (define (wrap-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "pointer->ff:" <>)
         syntax-context type-name))

   (define (unwrap-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> "->pointer")
         syntax-context type-name))

   (define (unchecked-unwrap-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "unchecked-ff:" <> "->pointer")
         syntax-context type-name))

   (define (malloc-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "malloc-ff:" <>)
         syntax-context type-name))

   (define (free-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "free-ff:" <>)
         syntax-context type-name))

   (define (unchecked-free-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "unchecked-free-ff:" <>)
         syntax-context type-name))

   (define (gc-malloc-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "gc-malloc-ff:" <>)
         syntax-context type-name))

   (define (gc-free-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "gc-free-ff:" <>)
         syntax-context type-name))

   (define (unchecked-gc-free-struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "unchecked-gc-free-ff:" <>)
         syntax-context type-name))
   )

(define-syntax with-ff:interface-exported
   (syntax-rules ()
      ((_ #t body body* ...)
       (let-syntax ((old-export
                       (syntax-rules ()
                          ((_) (fluid-ref %ff:interface-exported?)))))
          (fluid-set! %ff:interface-exported? #t)
          body body* ...
          (fluid-set! %ff:interface-exported? (old-export))))

      ((_ #f body body* ...)
       (let-syntax ((old-export
                       (syntax-rules ()
                          ((_) (fluid-ref %ff:interface-exported?)))))
          (fluid-set! %ff:interface-exported? #f)
          body body* ...
          (fluid-set! %ff:interface-exported? (old-export))))

      ((_ body body* ...)
       (with-ff:interface-exported #t body body* ...))))

(define-syntax maybe-export
   (syntax-rules ()
      ((_ id id* ...)
       (if (ff:interface-exported?)
           (export id id* ...)))))

(define-syntax define-ff:interface
   (lambda (x)
      (syntax-case x (struct union sizeof field)

         ;; (expand-internal-type (sizeof "SplineChar" 123))
         ;;    -> (define sizeof-ff::SplineChar 123)
         ((_ (sizeof type-name size))
          #`(begin
               (maybe-export
                  #,(type-sizeof-var x #'type-name))
               (define #,(type-sizeof-var x #'type-name) size)))

         ((_ (struct-or-union type-name size))
          (struct-or-union-identifier? #'struct-or-union)
          (let ((tag (type-tag x #'type-name)))
             #`(begin
                  (maybe-export
                     #,(struct?-func x #'type-name)
                     #,(check-struct-func x #'type-name)
                     #,(wrap-struct-func x #'type-name)
                     #,(unwrap-struct-func x #'type-name)
                     #,(unchecked-unwrap-struct-func x #'type-name)
                     #,(malloc-struct-func x #'type-name)
                     #,(free-struct-func x #'type-name)
                     #,(unchecked-free-struct-func x #'type-name)
                     #,(gc-malloc-struct-func x #'type-name)
                     #,(gc-free-struct-func x #'type-name)
                     #,(unchecked-gc-free-struct-func x #'type-name)
                     )

                  (define #,(struct?-func x #'type-name)
                     (lambda (obj)
                        (cond
                           ((not (pair? obj)) #f)
                           ((not (eq? '#,tag (car obj))) #f)
                           ((not (bytevector? (cdr obj))) #f)
                           ((not (= (bytevector-length (cdr obj)) size)) #f)
                           (else #t))))

                  (define #,(throw-failed-check-struct-func x #'type-name)
                     (lambda (caller err-msg obj)
                        (scm-error 'wrong-type-arg
                           (if (symbol? caller)
                               (symbol->string caller)
                               caller)
                           (if err-msg
                               "Expected ~A API struct of type `ff:~A', but ~A"
                               "Expected ~A API struct of type `ff:~A'")
                           (if err-msg
                               (list pkg-info:package-name type-name err-msg)
                               (list pkg-info:package-name type-name))
                           (list obj))))

                  (define #,(check-struct-func x #'type-name)
                     (lambda (caller obj)
                        (cond
                           ((not (pair? obj))
                            (#,(throw-failed-check-struct-func x #'type-name)
                             caller
                             (simple-format #f "~S is not a pair" obj)
                             obj))
                           ((not (eq? '#,tag (car obj)))
                            (#,(throw-failed-check-struct-func x #'type-name)
                             caller
                             (simple-format #f "(car ~S) is not ~S" obj '#,tag)
                             obj))
                           ((not (bytevector? (cdr obj)))
                            (#,(throw-failed-check-struct-func x #'type-name)
                             caller
                             (simple-format #f "(cdr ~S) is not a bytevector" obj)
                             obj))
                           ((not (= (bytevector-length (cdr obj)) size))
                            (#,(throw-failed-check-struct-func x #'type-name)
                             caller
                             "bytevector length is wrong"
                             obj))
                           (else *unspecified*))))

                  (define #,(wrap-struct-func x #'type-name)
                     (lambda (ptr)
                        (cons '#,tag (pointer->bytevector ptr size))))

                  (define #,(unwrap-struct-func x #'type-name)
                     (lambda (obj)
                        (#,(check-struct-func x #'type-name)
                         '#,(unwrap-struct-func x #'type-name)
                         obj)
                        (bytevector->pointer (cdr obj))))

                  (define #,(unchecked-unwrap-struct-func x #'type-name)
                     (lambda (obj)
                        (bytevector->pointer (cdr obj))))

                  (define #,(malloc-struct-func x #'type-name)
                     (lambda ()
                        (cons '#,tag
                           (pointer->bytevector (scm_calloc size) size))))

                  (define #,(free-struct-func x #'type-name)
                     (lambda (obj)
                        (free (#,(unwrap-struct-func x #'type-name) obj))))

                  (define #,(unchecked-free-struct-func x #'type-name)
                     (lambda (obj)
                        (free (#,(unchecked-unwrap-struct-func x #'type-name) obj))))

                  (define #,(gc-malloc-struct-func x #'type-name)
                     (lambda ()
                        (cons '#,tag
                           (pointer->bytevector (GC_malloc size) size))))

                  (define #,(gc-free-struct-func x #'type-name)
                     (lambda (obj)
                        (GC_free (#,(unwrap-struct-func x #'type-name) obj))))

                  (define #,(unchecked-gc-free-struct-func x #'type-name)
                     (lambda (obj)
                        (GC_free (#,(unchecked-unwrap-struct-func x #'type-name) obj))))

                  ))))))

#!
(with-ff:interface-exported
   (define-ff:interface (sizeof "Foo" 32))
   (define-ff:interface (struct "Foo" 32))
   (define-ff:interface (union "Goo" 32))
   )

(define s (cons 'tag-ff:Foo (make-bytevector 32)))
(write s) (newline)
(write (ff:Foo? s)) (newline)
(check-ff:Foo #f s)
(write (ff:Foo? (pointer->ff:Foo (unchecked-ff:Foo->pointer s)))) (newline)
(write (ff:Foo? (malloc-ff:Foo))) (newline)
(write (free-ff:Foo (malloc-ff:Foo))) (newline)
(write (unchecked-free-ff:Foo (malloc-ff:Foo))) (newline)
(write (ff:Foo? (gc-malloc-ff:Foo))) (newline)
(write (gc-free-ff:Foo (gc-malloc-ff:Foo))) (newline)
(write (unchecked-gc-free-ff:Foo (gc-malloc-ff:Foo))) (newline)
!#

