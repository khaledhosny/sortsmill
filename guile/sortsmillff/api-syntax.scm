;; -*- mode: scheme; coding: utf-8 -*-

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

(define-module (sortsmillff api-syntax))

(use-modules
 (sortsmillff machine)
 (sortsmillff pkg-info)
 (rnrs bytevectors)
 (system foreign)
 (srfi srfi-26)                       ; ‘cut’ and ‘cute’.
 )

;; FIXME: Really we require that floating point numbers be IEEE single
;; precision or double precision. More than likely, we will support
;; only machines for which this is true, but these tests should not
;; hurt.
(eval-when (compile load eval)
           (cond
            ((not (= 4 float-size))
             (error
              (simple-format #f
                             "The size of a C float is required to be 4 bytes, but on this machine it is ~A bytes."
                             (number->string float-size))))
            ((not (= 8 double-size))
             (error
              (simple-format #f
                             "The size of a C double is required to be 8 bytes, but on this machine it is ~A bytes."
                             (number->string double-size))))))

(export
 ;; Fluid for ‘Are API-definitions being exported?’
 %api-exported:--?

 ;; Are API-definitions being exported?
 api-exported:--?

 ;; Export enclosed API-definitions.
 with-api-exported:--

 ;; Follow an API-definition instruction.
 api:--

 ;; Read instructions from a port.
 read-api:--

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

(eval-when (compile load eval)

           (define %api-exported:--? (make-fluid #f))

           (define api-exported:--?
             (case-lambda
               (() (fluid-ref %api-exported:--?))
               ((v) (fluid-set! %api-exported:--? (not (not v))))))

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

           (define build-type-related-symbol
             (case-lambda
               ((constructor x type-name)
                (datum->syntax x
                               (string->symbol (constructor (syntax->datum type-name)))))
               ((constructor x struct-name field-name)
                (datum->syntax x
                               (string->symbol (constructor
                                                (syntax->datum struct-name)
                                                (syntax->datum field-name)))))))

           (define (type-tag x type-name)
             (build-type-related-symbol
              (cute simple-format #f "tag-~A" <>)
              x type-name))

           (define (type-sizeof-var x type-name)
             (build-type-related-symbol
              (cute simple-format #f "sizeof-~A" <>)
              x type-name))

           (define (struct?-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "~A?" <>)
              x type-name))

           (define (throw-failed-check-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "throw-failed-check-~A" <>)
              x type-name))

           (define (check-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "check-~A" <>)
              x type-name))

           (define (pointer->struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "pointer->~A" <>)
              x type-name))

           (define (struct->pointer-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "~A->pointer" <>)
              x type-name))

           (define (unchecked-struct->pointer-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A->pointer" <>)
              x type-name))

           (define (struct-ref-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "~A-ref" <>)
              x type-name))

           (define (unchecked-struct-ref-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A-ref" <>)
              x type-name))

           (define (malloc-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "malloc-~A" <>)
              x type-name))

           (define (free-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "free-~A" <>)
              x type-name))

           (define (unchecked-free-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-free-~A" <>)
              x type-name))

           (define (gc-malloc-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "gc-malloc-~A" <>)
              x type-name))

           (define (gc-free-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "gc-free-~A" <>)
              x type-name))

           (define (unchecked-gc-free-struct-func x type-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-gc-free-~A" <>)
              x type-name))

           (define (field-ref-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "~A:~A-ref" <> <>)
              x struct-name field-name))

           (define (unchecked-field-ref-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A:~A-ref" <> <>)
              x struct-name field-name))

           (define (field-dref-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "~A:~A-dref" <> <>)
              x struct-name field-name))

           (define (unchecked-field-dref-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A:~A-dref" <> <>)
              x struct-name field-name))

           (define (field-set!-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "~A:~A-set!" <> <>)
              x struct-name field-name))

           (define (unchecked-field-set!-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A:~A-set!" <> <>)
              x struct-name field-name))

           (define (field->pointer-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "~A:~A->pointer" <> <>)
              x struct-name field-name))

           (define (unchecked-field->pointer-func x struct-name field-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A:~A->pointer" <> <>)
              x struct-name field-name))

           (define (struct->alist-func x struct-name)
             (build-type-related-symbol
              (cute simple-format #f "~A->alist" <>)
              x struct-name))

           (define (unchecked-struct->alist-func x struct-name)
             (build-type-related-symbol
              (cute simple-format #f "unchecked-~A->alist" <>)
              x struct-name))
           )

(define-syntax with-api-exported:--
  (syntax-rules ()
    ((_ #t body body* ...)
     (let-syntax ((old-export
                   (syntax-rules ()
                     ((_) (fluid-ref %api-exported:--?)))))
       (fluid-set! %api-exported:--? #t)
       body body* ...
       (fluid-set! %api-exported:--? (old-export))))

    ((_ #f body body* ...)
     (let-syntax ((old-export
                   (syntax-rules ()
                     ((_) (fluid-ref %api-exported:--?)))))
       (fluid-set! %api-exported:--? #f)
       body body* ...
       (fluid-set! %api-exported:--? (old-export))))

    ((_ body body* ...)
     (with-api-exported:-- #t body body* ...))))

(define-syntax maybe-export
  (syntax-rules ()
    ((_ id id* ...)
     (if (api-exported:--?)
         (export id id* ...)))))

(define-syntax expand-sizeof
  (lambda (x)
    (syntax-case x ()
      ((_ type-name size)
       #`(begin
           (maybe-export
            ;; Example: sizeof-SplineChar
            #,(type-sizeof-var x #'type-name)
            )
           (define #,(type-sizeof-var x #'type-name) size))))))

(define-syntax expand-struct
  (lambda (x)
    (syntax-case x ()
      ((_ type-name size)
       (let ((tag (type-tag x #'type-name)))
         #`(begin
             (maybe-export
              ;; Example: SplineChar?
              #,(struct?-func x #'type-name)

              ;; Example: check-SplineChar
              #,(check-struct-func x #'type-name)

              ;; Example: pointer->SplineChar
              #,(pointer->struct-func x #'type-name)

              ;; Example: unchecked-SplineChar->pointer
              #,(unchecked-struct->pointer-func x #'type-name)

              ;; Example: SplineChar->pointer
              #,(struct->pointer-func x #'type-name)

              ;; Example: unchecked-SplineChar-ref
              #,(unchecked-struct-ref-func x #'type-name)

              ;; Example: SplineChar-ref
              #,(struct-ref-func x #'type-name)

              ;; Example: malloc-SplineChar
              #,(malloc-struct-func x #'type-name)

              ;; Example: unchecked-free-SplineChar
              #,(unchecked-free-struct-func x #'type-name)

              ;; Example: free-SplineChar
              #,(free-struct-func x #'type-name)

              ;; Example: gc-malloc-SplineChar
              #,(gc-malloc-struct-func x #'type-name)

              ;; Example: unchecked-gc-free-SplineChar
              #,(unchecked-gc-free-struct-func x #'type-name)

              ;; Example: gc-free-SplineChar
              #,(gc-free-struct-func x #'type-name)
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
                                "Expected ~A API struct of type `~A', but ~A"
                                "Expected ~A API struct of type `~A'")
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

             (define #,(pointer->struct-func x #'type-name)
               (lambda (ptr)
                 (cons '#,tag (pointer->bytevector ptr size))))

             (define #,(unchecked-struct->pointer-func x #'type-name)
               (case-lambda
                 ((obj) (bytevector->pointer (cdr obj)))
                 ((obj i)
                  ;; Return a pointer to the ith structure
                  ;; relative to this one in an array.
                  (let ((p (bytevector->pointer (cdr obj))))
                    (make-pointer (+ (pointer-address p) (* i size)))))))

             (define #,(struct->pointer-func x #'type-name)
               (case-lambda
                 ((obj)
                  (#,(check-struct-func x #'type-name)
                   '#,(struct->pointer-func x #'type-name)
                   obj)
                  (bytevector->pointer (cdr obj)))
                 ((obj i)
                  ;; Return a pointer to the ith structure
                  ;; relative to this one in an array.
                  (#,(check-struct-func x #'type-name)
                   '#,(struct->pointer-func x #'type-name)
                   obj)
                  (#,(unchecked-struct->pointer-func x #'type-name)
                   obj i))))

             (define #,(unchecked-struct-ref-func x #'type-name)
               (case-lambda
                 ((obj)
                  ;; This merely copies the tagged
                  ;; bytevector. It exists mainly for
                  ;; consistency with other uses of ‘-ref’ in
                  ;; this module.
                  (#,(pointer->struct-func x #'type-name)
                   (bytevector->pointer (cdr obj))))
                 ((obj i)
                  ;; This gives the ith structure relative to
                  ;; this one in an array.
                  (let ((p (bytevector->pointer (cdr obj))))
                    (#,(pointer->struct-func x #'type-name)
                     (make-pointer (+ (pointer-address p) (* i size))))))))

             (define #,(struct-ref-func x #'type-name)
               (case-lambda
                 ((obj)
                  ;; This merely copies the tagged
                  ;; bytevector. It exists mainly for
                  ;; consistency with other uses of ‘-ref’ in
                  ;; this module.
                  (#,(check-struct-func x #'type-name)
                   '#,(struct->pointer-func x #'type-name)
                   obj)
                  (#,(unchecked-struct-ref-func x #'type-name) obj))
                 ((obj i )
                  ;; This gives the ith structure relative to
                  ;; this one in an array.
                  (#,(check-struct-func x #'type-name)
                   '#,(struct->pointer-func x #'type-name)
                   obj)
                  (#,(unchecked-struct-ref-func x #'type-name) obj i))))

             (define #,(malloc-struct-func x #'type-name)
               (case-lambda
                 (() (cons '#,tag
                           (pointer->bytevector (scm_calloc size) size)))
                 ((n)
                  ;; Allocate a contiguous array of n structs,
                  ;; with the tagged bytevector pointing at the
                  ;; first struct in the array.
                  (unless (<= 1 n)
                    (scm-error 'out-of-range
                               #,(datum->syntax x (simple-format #f "malloc-~A"
                                                                 (syntax->datum #'type-name)))
                               (string-append "the argument must be >= 1, but got "
                                              (number->string n))
                               (list n) (list n)))
                  (cons '#,tag
                        (pointer->bytevector (scm_calloc (* n size)) size)))))

             (define #,(unchecked-free-struct-func x #'type-name)
               (lambda (obj)
                 (free (#,(unchecked-struct->pointer-func x #'type-name) obj))))

             (define #,(free-struct-func x #'type-name)
               (lambda (obj)
                 (free (#,(struct->pointer-func x #'type-name) obj))))

             (define #,(gc-malloc-struct-func x #'type-name)
               (case-lambda
                 (() (cons '#,tag
                           (pointer->bytevector (GC_malloc size) size)))
                 ((n)
                  ;; Allocate a contiguous array of n structs,
                  ;; with the tagged bytevector pointing at the
                  ;; first struct in the array.
                  (unless (<= 1 n)
                    (scm-error 'out-of-range
                               #,(datum->syntax x (simple-format #f "gc-malloc-~A"
                                                                 (syntax->datum #'type-name)))
                               (string-append "the argument must be >= 1, but got "
                                              (number->string n))
                               (list n) (list n)))
                  (cons '#,tag
                        (pointer->bytevector (GC_malloc (* n size)) size)))))

             (define #,(unchecked-gc-free-struct-func x #'type-name)
               (lambda (obj)
                 (GC_free (#,(unchecked-struct->pointer-func x #'type-name) obj))))

             (define #,(gc-free-struct-func x #'type-name)
               (lambda (obj)
                 (GC_free (#,(struct->pointer-func x #'type-name) obj))))

             ))))))

(define-syntax expand-field-without-dereferencing
  (lambda (x)
    (syntax-case x ()
      ((_ field-type struct-name field-name offset size)
       #`(begin
           (expand-pointer-to-field struct-name field-name offset size)
           (expand-ref-set!-without-dereferencing field-type struct-name
                                                  field-name offset size)
           )))))

(define-syntax expand-pointer-to-field
  (lambda (x)
    (syntax-case x ()
      ((_ struct-name field-name offset size)
       #`(begin
           (maybe-export
            ;; Example: unchecked-SplineChar:name->pointer
            #,(unchecked-field->pointer-func x #'struct-name #'field-name)

            ;; Example: SplineChar:name->pointer
            #,(field->pointer-func x #'struct-name #'field-name)
            )
           
           (define #,(unchecked-field->pointer-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                ((field->pointer offset) (cdr obj)))
               ((obj i)
                ((field->pointer (+ offset (* i size))) (cdr obj)))))

           (define #,(field->pointer-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-struct-func x #'struct-name)
                 #,(field->pointer-func x #'struct-name #'field-name)
                 obj)
                ((field->pointer offset) (cdr obj)))
               ((obj i)
                (#,(check-struct-func x #'struct-name)
                 #,(field->pointer-func x #'struct-name #'field-name)
                 obj)
                ((field->pointer (+ offset (* i size))) (cdr obj)))))
           )))))

(define-syntax expand-ref-set!-without-dereferencing
  (lambda (x)
    (syntax-case x ()
      ((_ field-type struct-name field-name offset size)
       #`(begin
           (maybe-export
            ;; Example: unchecked-SplineChar:width-ref
            #,(unchecked-field-ref-func x #'struct-name #'field-name)

            ;; Example: SplineChar:width-ref
            #,(field-ref-func x #'struct-name #'field-name)

            ;; Example: unchecked-SplineChar:width-set!
            #,(unchecked-field-set!-func x #'struct-name #'field-name)

            ;; Example: SplineChar:width-set!
            #,(field-set!-func x #'struct-name #'field-name)
            )
           
           (define #,(unchecked-field-ref-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                ((field-ref field-type offset size) (cdr obj)))
               ((obj i)
                ((field-ref field-type (+ offset (* i size)) size) (cdr obj)))))

           (define #,(field-ref-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-struct-func x #'struct-name)
                 #,(field-ref-func x #'struct-name #'field-name)
                 obj)
                ((field-ref field-type offset size) (cdr obj)))
               ((obj i)
                (#,(check-struct-func x #'struct-name)
                 #,(field-ref-func x #'struct-name #'field-name)
                 obj)
                ((field-ref field-type (+ offset (* i size)) size) (cdr obj)))))

           (define #,(unchecked-field-set!-func x #'struct-name #'field-name)
             (case-lambda
               ((obj v)
                ((field-set! field-type offset size) (cdr obj) v))
               ((obj i v)
                ((field-set! field-type (+ offset (* i size)) size) (cdr obj) v))))

           (define #,(field-set!-func x #'struct-name #'field-name)
             (case-lambda
               ((obj v)
                (#,(check-struct-func x #'struct-name)
                 #,(field-set!-func x #'struct-name #'field-name)
                 obj)
                ((field-set! field-type offset size) (cdr obj) v))
               ((obj i v)
                (#,(check-struct-func x #'struct-name)
                 #,(field-set!-func x #'struct-name #'field-name)
                 obj)
                ((field-set! field-type (+ offset (* i size)) size) (cdr obj) v))))
           )))))

(define-syntax expand-field-dereferencing
  (lambda (x)
    (syntax-case x ()
      ((_ (field-type field-subtype) struct-name field-name offset size)
       #`(begin
           (maybe-export
            ;; Example: unchecked-CharViewBase:next-dref
            #,(unchecked-field-dref-func x #'struct-name #'field-name)

            ;; Example: CharViewBase:next-dref
            #,(field-dref-func x #'struct-name #'field-name)
            )

           (define #,(unchecked-field-dref-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (let ((pointer ((field-ref field-type offset size) (cdr obj))))
                  (#,(pointer->struct-func x #'field-subtype) pointer)))
               ((obj i)
                (let* ((p ((field-ref field-type offset size) (cdr obj)))
                       (struct-size #,(type-sizeof-var x #'field-subtype))
                       (pointer (make-pointer
                                 (+ (pointer-address p) (* i struct-size)))))
                  (#,(pointer->struct-func x #'field-subtype) pointer)))))

           (define #,(field-dref-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-struct-func x #'struct-name)
                 #,(field-dref-func x #'struct-name #'field-name)
                 obj)
                (#,(unchecked-field-dref-func x #'struct-name #'field-name) obj))
               ((obj i)
                (#,(check-struct-func x #'struct-name)
                 #,(field-dref-func x #'struct-name #'field-name)
                 obj)                      
                (#,(unchecked-field-dref-func x #'struct-name #'field-name) obj i))))
           )))))

(define-syntax expand-ref-set!-of-struct
  ;; This is similar to de-referencing a pointer field. The
  ;; difference is that the pointer is the address of the field, not
  ;; its contents.
  (lambda (x)
    (syntax-case x ()
      ((_ (field-type field-subtype) struct-name field-name offset size)
       #`(begin
           (maybe-export
            ;; Example: unchecked-CharViewBase:next-ref
            #,(unchecked-field-ref-func x #'struct-name #'field-name)

            ;; Example: CharViewBase:next-ref
            #,(field-ref-func x #'struct-name #'field-name)
            )

           (define #,(unchecked-field-ref-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (let ((pointer ((field->pointer offset) (cdr obj))))
                  (#,(pointer->struct-func x #'field-subtype) pointer)))
               ((obj i)
                (let ((p ((field->pointer offset) (cdr obj)))
                      (pointer (make-pointer (+ (pointer-address p) (* i size)))))
                  (#,(pointer->struct-func x #'field-subtype) pointer)))))

           (define #,(field-ref-func x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-struct-func x #'struct-name)
                 #,(field-ref-func x #'struct-name #'field-name)
                 obj)
                (#,(unchecked-field-ref-func x #'struct-name #'field-name) obj))
               ((obj i)
                (#,(check-struct-func x #'struct-name)
                 #,(field-ref-func x #'struct-name #'field-name)
                 obj)                      
                (#,(unchecked-field-ref-func x #'struct-name #'field-name) obj i))))
           )))))

(define-syntax expand-struct->
  (lambda (x)
    (syntax-case x ()
      ((_ struct-name (field-name kind field-type offset size) ...)
       #`(begin
           (maybe-export
            ;; Example: unchecked-SplineChar->alist
            #,(unchecked-struct->alist-func x #'struct-name)

            ;; Example: SplineChar->alist
            #,(struct->alist-func x #'struct-name)
            )

           (define #,(unchecked-struct->alist-func x #'struct-name)
             (lambda (obj)
               (list
                ((unchecked-field->alist-entry
                  field-name kind field-type offset size) obj)
                ...)))

           (define #,(struct->alist-func x #'struct-name)
             (lambda (obj)
               (#,(check-struct-func x #'struct-name)
                #,(struct->alist-func x #'struct-name)
                obj)
               (#,(unchecked-struct->alist-func x #'struct-name) obj)))
           )))))

(define-syntax api:--
  (lambda (x)
    (syntax-case x (struct sizeof field * array)
      ((_ (sizeof type-name size)) #'(expand-sizeof type-name size))

      ((_ (struct type-name size)) #'(expand-struct type-name size))

      ((_ (field (field-type field-subtype) struct-name field-name offset size))
       (eq? '* (syntax->datum #'field-type))
       #'(begin
           (expand-field-without-dereferencing field-type struct-name field-name offset size)
           (expand-field-dereferencing (field-type field-subtype) struct-name field-name offset size)))

      ((_ (field (field-type field-subtype) struct-name field-name offset size))
       (or (eq? 'struct (syntax->datum #'field-type))
           (eq? 'array (syntax->datum #'field-type)))
       #'(begin
           (expand-pointer-to-field struct-name field-name offset size)
           (expand-ref-set!-of-struct (field-type field-subtype) struct-name field-name offset size)
           ))

      ((_ (field field-type struct-name field-name offset size))
       (or (eq? 'struct (syntax->datum #'field-type))
           (eq? 'array (syntax->datum #'field-type)))
       #'(expand-pointer-to-field struct-name field-name offset size))

      ((_ (field field-type struct-name field-name offset size))
       #'(expand-field-without-dereferencing field-type struct-name field-name offset size))

      ((_ (struct-> struct-name (field-name kind field-type offset size) ...))
       #'(expand-struct-> struct-name (field-name kind field-type offset size) ...))

      ((_ form form* ...)
       #'(begin (api:-- form) (api:-- form*) ...))

      )))

;;-------------------------------------------------------------------------

(define-syntax field-ref
  (syntax-rules (int uint bool float * struct array)
    ((_ int offset 1) (lambda (bv) (bytevector-s8-ref bv offset)))
    ((_ int offset 2) (lambda (bv) (bytevector-s16-native-ref bv offset)))
    ((_ int offset 4) (lambda (bv) (bytevector-s32-native-ref bv offset)))
    ((_ int offset 8) (lambda (bv) (bytevector-s64-native-ref bv offset)))
    ((_ uint offset 1) (lambda (bv) (bytevector-u8-ref bv offset)))
    ((_ uint offset 2) (lambda (bv) (bytevector-u16-native-ref bv offset)))
    ((_ uint offset 4) (lambda (bv) (bytevector-u32-native-ref bv offset)))
    ((_ uint offset 8) (lambda (bv) (bytevector-u64-native-ref bv offset)))
    ((_ bool offset 1) (lambda (bv) (not (zero? (bytevector-u8-ref bv offset)))))
    ((_ bool offset 2) (lambda (bv) (not (zero? (bytevector-u16-native-ref bv offset)))))
    ((_ bool offset 4) (lambda (bv) (not (zero? (bytevector-u32-native-ref bv offset)))))
    ((_ bool offset 8) (lambda (bv) (not (zero? (bytevector-u64-native-ref bv offset)))))
    ((_ float offset 4) (lambda (bv) (bytevector-ieee-single-native-ref bv offset)))
    ((_ float offset 8) (lambda (bv) (bytevector-ieee-double-native-ref bv offset)))
    ((_ * offset 1) (lambda (bv) (make-pointer (bytevector-u8-ref bv offset))))
    ((_ * offset 2) (lambda (bv) (make-pointer (bytevector-u16-native-ref bv offset))))
    ((_ * offset 4) (lambda (bv) (make-pointer (bytevector-u32-native-ref bv offset))))
    ((_ * offset 8) (lambda (bv) (make-pointer (bytevector-u64-native-ref bv offset))))
    ))

(define-syntax field-set!
  (syntax-rules (int uint bool * struct array)
    ((_ int offset 1) (lambda (bv v) (bytevector-s8-set! bv offset v)))
    ((_ int offset 2) (lambda (bv v) (bytevector-s16-native-set! bv offset v)))
    ((_ int offset 4) (lambda (bv v) (bytevector-s32-native-set! bv offset v)))
    ((_ int offset 8) (lambda (bv v) (bytevector-s64-native-set! bv offset v)))
    ((_ uint offset 1) (lambda (bv v) (bytevector-u8-set! bv offset v)))
    ((_ uint offset 2) (lambda (bv v) (bytevector-u16-native-set! bv offset v)))
    ((_ uint offset 4) (lambda (bv v) (bytevector-u32-native-set! bv offset v)))
    ((_ uint offset 8) (lambda (bv v) (bytevector-u64-native-set! bv offset v)))
    ((_ bool offset 1) (lambda (bv v) (bytevector-u8-set! bv offset (if v 1 0))))
    ((_ bool offset 2) (lambda (bv v) (bytevector-u16-native-set! bv offset (if v 1 0))))
    ((_ bool offset 4) (lambda (bv v) (bytevector-u32-native-set! bv offset (if v 1 0))))
    ((_ bool offset 8) (lambda (bv v) (bytevector-u64-native-set! bv offset (if v 1 0))))
    ((_ float offset 4) (lambda (bv v) (bytevector-ieee-single-native-set! bv offset v)))
    ((_ float offset 8) (lambda (bv v) (bytevector-ieee-double-native-set! bv offset v)))
    ((_ * offset 1) (lambda (bv v) (bytevector-u8-set! bv offset (pointer-address v))))
    ((_ * offset 2) (lambda (bv v) (bytevector-u16-native-set! bv offset (pointer-address v))))
    ((_ * offset 4) (lambda (bv v) (bytevector-u32-native-set! bv offset (pointer-address v))))
    ((_ * offset 8) (lambda (bv v) (bytevector-u64-native-set! bv offset (pointer-address v))))
    ))

(define-syntax field->pointer
  (syntax-rules ()
    ((_ offset) (lambda (bv) (bytevector->pointer bv offset)))))

(define-syntax unchecked-field->alist-entry
  (lambda (x)
    (syntax-case x (field struct-field)
      ((_ field-name field (field-type field-subtype) offset size)
       (eq? '* (syntax->datum #'field-type))
       #`(lambda (obj)
           (cons
            (quote #,(datum->syntax x
                                    (string->symbol (syntax->datum #'field-name))))
            ((field-ref field-type offset size) (cdr obj)))))

      ((_ field-name field (field-type field-subtype) offset size)
       (or (eq? 'struct (syntax->datum #'field-type))
           (eq? 'array (syntax->datum #'field-type)))
       #`(lambda (obj)
           (cons
            (quote #,(datum->syntax x
                                    (string->symbol (syntax->datum #'field-name))))
            ((field->pointer offset) (cdr obj)))))

      ((_ field-name field field-type offset size)
       (or (eq? 'struct (syntax->datum #'field-type))
           (eq? 'array (syntax->datum #'field-type)))
       #`(lambda (obj)
           (cons
            (quote #,(datum->syntax x
                                    (string->symbol (syntax->datum #'field-name))))
            ((field->pointer offset) (cdr obj)))))

      ((_ field-name field field-type offset size)
       #`(lambda (obj)
           (cons
            (quote #,(datum->syntax x
                                    (string->symbol (syntax->datum #'field-name))))
            ((field-ref field-type offset size) (cdr obj)))))
      )))

;;-------------------------------------------------------------------------

(define read-api:--
  (case-lambda
    (() (read-api:-- (current-input-port)))
    ((port)
     (do ((instruction (read port) (read port)))
         ((eof-object? instruction))
       (eval (list 'api:-- instruction)
             (interaction-environment))))))

;;-------------------------------------------------------------------------
