;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Barry Schwartz
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

(import (sortsmillff machine)
        (sortsmillff alloc)
        (sortsmillff pkg-info)
        (rnrs)
        (system foreign)
        (only (srfi :26) cut cute)
        (ice-9 match)
        )

(define-syntax eval-early
  (syntax-rules ()
    ((_ e e* ...) (eval-when (compile load eval) e e* ...))))

;; FIXME: Really we require that floating point numbers be IEEE single
;; precision or double precision. More than likely, we will support
;; only machines for which this is true, but these tests should not
;; hurt.
(eval-early
 (cond
  ((not (= 4 float-size))
   (error
    (format28
     "The size of a C float is required to be 4 bytes, but on this machine it is ~a bytes."
     (number->string float-size))))
  ((not (= 8 double-size))
   (error
    (format28
     #f
     "The size of a C double is required to be 8 bytes, but on this machine it is ~a bytes."
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

(eval-early

 ;; Define ‘format28’ as a format function resembling that of SRFI-28;
 ;; we will use it instead of the native format functions, to help make
 ;; this module more portable.
 ;;
 ;; See http://srfi.schemers.org/srfi-28/srfi-28.html
 ;;
 (define (format28 format-string . objects)
   (apply (cut simple-format #f format-string <...>) objects))

 (define %api-exported:--? (make-fluid #f))

 (define api-exported:--?
   (case-lambda
     (() (fluid-ref %api-exported:--?))
     ((v) (fluid-set! %api-exported:--? (not (not v))))))

 (define (struct-type-error-msg tag size obj)
   (cond
    ((not (pair? obj))
     (format28 "~s is not a pair" obj))
    ((not (eq? tag (car obj)))
     (format28 "(car ~s) is not ~s" obj tag))
    ((not (bytevector? (cdr obj)))
     (format28 "(cdr ~s) is not a bytevector" obj))
    ((not (= (bytevector-length (cdr obj)) size))
     "bytevector length is wrong")
    (else #f)))

 (define (syntax-string->syntax-symbol x s)
   (datum->syntax x (string->symbol (syntax->datum s))))

 (define (build-symbol x constructor . objects)
   (syntax-string->syntax-symbol
    x (apply constructor (map syntax->datum objects))))

 (define (type-tag x type-name)
   (build-symbol x
                 (cute format28 "tag-~a" <>)
                 type-name))

 (define (type-sizeof-var x type-name)
   (build-symbol x
                 (cute format28 "sizeof-~a" <>)
                 type-name))

 (define (<type?> x type-name)
   (build-symbol x
                 (cute format28 "~a?" <>)
                 type-name))

 (define (throw-failed-check-<type> x type-name)
   (build-symbol x
                 (cute format28 "throw-failed-check-~a" <>)
                 type-name))

 (define (check-<type> x type-name)
   (build-symbol x
                 (cute format28 "check-~a" <>)
                 type-name))

 (define (pointer-><type> x type-name)
   (build-symbol x
                 (cute format28 "pointer->~a" <>)
                 type-name))

 (define (<type>->pointer x type-name)
   (build-symbol x
                 (cute format28 "~a->pointer" <>)
                 type-name))

 (define (unchecked-<type>->pointer x type-name)
   (build-symbol x
                 (cute format28 "unchecked-~a->pointer" <>)
                 type-name))

 (define (<type>-ref x type-name)
   (build-symbol x
                 (cute format28 "~a-ref" <>)
                 type-name))

 (define (unchecked-<type>-ref x type-name)
   (build-symbol x
                 (cute format28 "unchecked-~a-ref" <>)
                 type-name))

 (define (malloc-<type> x type-name)
   (build-symbol x
                 (cute format28 "malloc-~a" <>)
                 type-name))

 (define (free-<type> x type-name)
   (build-symbol x
                 (cute format28 "free-~a" <>)
                 type-name))

 (define (unchecked-free-<type> x type-name)
   (build-symbol x
                 (cute format28 "unchecked-free-~a" <>)
                 type-name))

 (define (gc-malloc-<type> x type-name)
   (build-symbol x
                 (cute format28 "gc-malloc-~a" <>)
                 type-name))

 (define (gc-free-<type> x type-name)
   (build-symbol x
                 (cute format28 "gc-free-~a" <>)
                 type-name))

 (define (unchecked-gc-free-<type> x type-name)
   (build-symbol x
                 (cute format28 "unchecked-gc-free-~a" <>)
                 type-name))

 (define (<type>:<field>-ref x struct-name field-name)
   (build-symbol x
                 (cute format28 "~a:~a-ref" <> <>)
                 struct-name field-name))

 (define (unchecked-<type>:<field>-ref x struct-name field-name)
   (build-symbol x
                 (cute format28 "unchecked-~a:~a-ref" <> <>)
                 struct-name field-name))

 (define (<type>:<field>-dref x struct-name field-name)
   (build-symbol x
                 (cute format28 "~a:~a-dref" <> <>)
                 struct-name field-name))

 (define (unchecked-<type>:<field>-dref x struct-name field-name)
   (build-symbol x
                 (cute format28 "unchecked-~a:~a-dref" <> <>)
                 struct-name field-name))

 (define (<type>:<field>-set! x struct-name field-name)
   (build-symbol x
                 (cute format28 "~a:~a-set!" <> <>)
                 struct-name field-name))

 (define (unchecked-<type>:<field>-set! x struct-name field-name)
   (build-symbol x
                 (cute format28 "unchecked-~a:~a-set!" <> <>)
                 struct-name field-name))

 (define (<type>:<field>->pointer x struct-name field-name)
   (build-symbol x
                 (cute format28 "~a:~a->pointer" <> <>)
                 struct-name field-name))

 (define (unchecked-<type>:<field>->pointer x struct-name field-name)
   (build-symbol x
                 (cute format28 "unchecked-~a:~a->pointer" <> <>)
                 struct-name field-name))

 (define (<type>->alist x struct-name)
   (build-symbol x
                 (cute format28 "~a->alist" <>)
                 struct-name))

 (define (unchecked-<type>->alist x struct-name)
   (build-symbol x
                 (cute format28 "unchecked-~a->alist" <>)
                 struct-name))
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
              #,(<type?> x #'type-name)

              ;; Example: check-SplineChar
              #,(check-<type> x #'type-name)

              ;; Example: pointer->SplineChar
              #,(pointer-><type> x #'type-name)

              ;; Example: unchecked-SplineChar->pointer
              #,(unchecked-<type>->pointer x #'type-name)

              ;; Example: SplineChar->pointer
              #,(<type>->pointer x #'type-name)

              ;; Example: unchecked-SplineChar-ref
              #,(unchecked-<type>-ref x #'type-name)

              ;; Example: SplineChar-ref
              #,(<type>-ref x #'type-name)

              ;; Example: malloc-SplineChar
              #,(malloc-<type> x #'type-name)

              ;; Example: unchecked-free-SplineChar
              #,(unchecked-free-<type> x #'type-name)

              ;; Example: free-SplineChar
              #,(free-<type> x #'type-name)

              ;; Example: gc-malloc-SplineChar
              #,(gc-malloc-<type> x #'type-name)

              ;; Example: unchecked-gc-free-SplineChar
              #,(unchecked-gc-free-<type> x #'type-name)

              ;; Example: gc-free-SplineChar
              #,(gc-free-<type> x #'type-name)
              )

             (define #,(<type?> x #'type-name)
               (lambda (obj)
                 (cond
                  ((not (pair? obj)) #f)
                  ((not (eq? '#,tag (car obj))) #f)
                  ((not (bytevector? (cdr obj))) #f)
                  ((not (= (bytevector-length (cdr obj)) size)) #f)
                  (else #t))))

             (define #,(throw-failed-check-<type> x #'type-name)
               (lambda (caller err-msg obj)
                 (scm-error 'wrong-type-arg
                            (if (symbol? caller)
                                (symbol->string caller)
                                caller)
                            (if err-msg
                                "Expected ~a API struct of type `~a', but ~a"
                                "Expected ~a API struct of type `~a'")
                            (if err-msg
                                (list pkg-info:package-name type-name err-msg)
                                (list pkg-info:package-name type-name))
                            (list obj))))

             (define #,(check-<type> x #'type-name)
               (lambda (caller obj)
                 (cond
                  ((not (pair? obj))
                   (#,(throw-failed-check-<type> x #'type-name)
                    caller
                    (format28 "~s is not a pair" obj)
                    obj))
                  ((not (eq? '#,tag (car obj)))
                   (#,(throw-failed-check-<type> x #'type-name)
                    caller
                    (format28 "(car ~s) is not ~s" obj '#,tag)
                    obj))
                  ((not (bytevector? (cdr obj)))
                   (#,(throw-failed-check-<type> x #'type-name)
                    caller
                    (format28 "(cdr ~s) is not a bytevector" obj)
                    obj))
                  ((not (= (bytevector-length (cdr obj)) size))
                   (#,(throw-failed-check-<type> x #'type-name)
                    caller
                    "bytevector length is wrong"
                    obj))
                  (else *unspecified*))))

             (define #,(pointer-><type> x #'type-name)
               (lambda (ptr)
                 (cons '#,tag (pointer->bytevector ptr size))))

             (define #,(unchecked-<type>->pointer x #'type-name)
               (case-lambda
                 ((obj) (bytevector->pointer (cdr obj)))
                 ((obj i)
                  ;; Return a pointer to the ith structure
                  ;; relative to this one in an array.
                  (let ((p (bytevector->pointer (cdr obj))))
                    (make-pointer (+ (pointer-address p) (* i size)))))))

             (define #,(<type>->pointer x #'type-name)
               (case-lambda
                 ((obj)
                  (#,(check-<type> x #'type-name)
                   '#,(<type>->pointer x #'type-name)
                   obj)
                  (bytevector->pointer (cdr obj)))
                 ((obj i)
                  ;; Return a pointer to the ith structure
                  ;; relative to this one in an array.
                  (#,(check-<type> x #'type-name)
                   '#,(<type>->pointer x #'type-name)
                   obj)
                  (#,(unchecked-<type>->pointer x #'type-name)
                   obj i))))

             (define #,(unchecked-<type>-ref x #'type-name)
               (case-lambda
                 ((obj)
                  ;; This merely copies the tagged
                  ;; bytevector. It exists mainly for
                  ;; consistency with other uses of ‘-ref’ in
                  ;; this module.
                  (#,(pointer-><type> x #'type-name)
                   (bytevector->pointer (cdr obj))))
                 ((obj i)
                  ;; This gives the ith structure relative to
                  ;; this one in an array.
                  (let ((p (bytevector->pointer (cdr obj))))
                    (#,(pointer-><type> x #'type-name)
                     (make-pointer (+ (pointer-address p) (* i size))))))))

             (define #,(<type>-ref x #'type-name)
               (case-lambda
                 ((obj)
                  ;; This merely copies the tagged
                  ;; bytevector. It exists mainly for
                  ;; consistency with other uses of ‘-ref’ in
                  ;; this module.
                  (#,(check-<type> x #'type-name)
                   '#,(<type>->pointer x #'type-name)
                   obj)
                  (#,(unchecked-<type>-ref x #'type-name) obj))
                 ((obj i )
                  ;; This gives the ith structure relative to
                  ;; this one in an array.
                  (#,(check-<type> x #'type-name)
                   '#,(<type>->pointer x #'type-name)
                   obj)
                  (#,(unchecked-<type>-ref x #'type-name) obj i))))

             (define #,(malloc-<type> x #'type-name)
               (case-lambda
                 (() (cons '#,tag
                           (pointer->bytevector (c:zalloc size) size)))
                 ((n)
                  ;; Allocate a contiguous array of n structs,
                  ;; with the tagged bytevector pointing at the
                  ;; first struct in the array.
                  (unless (<= 1 n)
                    (scm-error 'out-of-range
                               #,(datum->syntax x (format28 "malloc-~a"
                                                                 (syntax->datum #'type-name)))
                               (string-append "the argument must be >= 1, but got "
                                              (number->string n))
                               (list n) (list n)))
                  (cons '#,tag
                        (pointer->bytevector (c:zalloc (* n size)) size)))))

             (define #,(unchecked-free-<type> x #'type-name)
               (lambda (obj)
                 (c:free (#,(unchecked-<type>->pointer x #'type-name) obj))))

             (define #,(free-<type> x #'type-name)
               (lambda (obj)
                 (c:free (#,(<type>->pointer x #'type-name) obj))))

             (define #,(gc-malloc-<type> x #'type-name)
               (case-lambda
                 (() (cons '#,tag
                           (pointer->bytevector (c:gc-zalloc size) size)))
                 ((n)
                  ;; Allocate a contiguous array of n structs,
                  ;; with the tagged bytevector pointing at the
                  ;; first struct in the array.
                  (unless (<= 1 n)
                    (scm-error 'out-of-range
                               #,(datum->syntax x (format28 "gc-malloc-~a"
                                                                 (syntax->datum #'type-name)))
                               (string-append "the argument must be >= 1, but got "
                                              (number->string n))
                               (list n) (list n)))
                  (cons '#,tag
                        (pointer->bytevector (c:gc-zalloc (* n size)) size)))))

             (define #,(unchecked-gc-free-<type> x #'type-name)
               (lambda (obj)
                 (c:gc-free (#,(unchecked-<type>->pointer x #'type-name) obj))))

             (define #,(gc-free-<type> x #'type-name)
               (lambda (obj)
                 (c:gc-free (#,(<type>->pointer x #'type-name) obj))))

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
            #,(unchecked-<type>:<field>->pointer x #'struct-name #'field-name)

            ;; Example: SplineChar:name->pointer
            #,(<type>:<field>->pointer x #'struct-name #'field-name)
            )
           
           (define #,(unchecked-<type>:<field>->pointer x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                ((field->pointer offset) (cdr obj)))
               ((obj i)
                ((field->pointer (+ offset (* i size))) (cdr obj)))))

           (define #,(<type>:<field>->pointer x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>->pointer x #'struct-name #'field-name)
                 obj)
                ((field->pointer offset) (cdr obj)))
               ((obj i)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>->pointer x #'struct-name #'field-name)
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
            #,(unchecked-<type>:<field>-ref x #'struct-name #'field-name)

            ;; Example: SplineChar:width-ref
            #,(<type>:<field>-ref x #'struct-name #'field-name)

            ;; Example: unchecked-SplineChar:width-set!
            #,(unchecked-<type>:<field>-set! x #'struct-name #'field-name)

            ;; Example: SplineChar:width-set!
            #,(<type>:<field>-set! x #'struct-name #'field-name)
            )
           
           (define #,(unchecked-<type>:<field>-ref x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                ((field-ref field-type offset size) (cdr obj)))
               ((obj i)
                ((field-ref field-type (+ offset (* i size)) size) (cdr obj)))))

           (define #,(<type>:<field>-ref x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-ref x #'struct-name #'field-name)
                 obj)
                ((field-ref field-type offset size) (cdr obj)))
               ((obj i)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-ref x #'struct-name #'field-name)
                 obj)
                ((field-ref field-type (+ offset (* i size)) size) (cdr obj)))))

           (define #,(unchecked-<type>:<field>-set! x #'struct-name #'field-name)
             (case-lambda
               ((obj v)
                ((field-set! field-type offset size) (cdr obj) v))
               ((obj i v)
                ((field-set! field-type (+ offset (* i size)) size) (cdr obj) v))))

           (define #,(<type>:<field>-set! x #'struct-name #'field-name)
             (case-lambda
               ((obj v)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-set! x #'struct-name #'field-name)
                 obj)
                ((field-set! field-type offset size) (cdr obj) v))
               ((obj i v)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-set! x #'struct-name #'field-name)
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
            #,(unchecked-<type>:<field>-dref x #'struct-name #'field-name)

            ;; Example: CharViewBase:next-dref
            #,(<type>:<field>-dref x #'struct-name #'field-name)
            )

           (define #,(unchecked-<type>:<field>-dref x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (let ((pointer ((field-ref field-type offset size) (cdr obj))))
                  (#,(pointer-><type> x #'field-subtype) pointer)))
               ((obj i)
                (let* ((p ((field-ref field-type offset size) (cdr obj)))
                       (struct-size #,(type-sizeof-var x #'field-subtype))
                       (pointer (make-pointer
                                 (+ (pointer-address p) (* i struct-size)))))
                  (#,(pointer-><type> x #'field-subtype) pointer)))))

           (define #,(<type>:<field>-dref x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-dref x #'struct-name #'field-name)
                 obj)
                (#,(unchecked-<type>:<field>-dref x #'struct-name #'field-name) obj))
               ((obj i)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-dref x #'struct-name #'field-name)
                 obj)                      
                (#,(unchecked-<type>:<field>-dref x #'struct-name #'field-name) obj i))))
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
            #,(unchecked-<type>:<field>-ref x #'struct-name #'field-name)

            ;; Example: CharViewBase:next-ref
            #,(<type>:<field>-ref x #'struct-name #'field-name)
            )

           (define #,(unchecked-<type>:<field>-ref x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (let ((pointer ((field->pointer offset) (cdr obj))))
                  (#,(pointer-><type> x #'field-subtype) pointer)))
               ((obj i)
                (let ((p ((field->pointer offset) (cdr obj)))
                      (pointer (make-pointer (+ (pointer-address p) (* i size)))))
                  (#,(pointer-><type> x #'field-subtype) pointer)))))

           (define #,(<type>:<field>-ref x #'struct-name #'field-name)
             (case-lambda
               ((obj)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-ref x #'struct-name #'field-name)
                 obj)
                (#,(unchecked-<type>:<field>-ref x #'struct-name #'field-name) obj))
               ((obj i)
                (#,(check-<type> x #'struct-name)
                 #,(<type>:<field>-ref x #'struct-name #'field-name)
                 obj)                      
                (#,(unchecked-<type>:<field>-ref x #'struct-name #'field-name) obj i))))
           )))))

(define-syntax expand-struct->
  (lambda (x)
    (syntax-case x ()
      ((_ struct-name (field-name kind field-type offset size) ...)
       #`(begin
           (maybe-export
            ;; Example: unchecked-SplineChar->alist
            #,(unchecked-<type>->alist x #'struct-name)

            ;; Example: SplineChar->alist
            #,(<type>->alist x #'struct-name)
            )

           (define #,(unchecked-<type>->alist x #'struct-name)
             (lambda (obj)
               (list
                ((unchecked-field->alist-entry
                  field-name kind field-type offset size) obj)
                ...)))

           (define #,(<type>->alist x #'struct-name)
             (lambda (obj)
               (#,(check-<type> x #'struct-name)
                #,(<type>->alist x #'struct-name)
                obj)
               (#,(unchecked-<type>->alist x #'struct-name) obj)))
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

;;;;;(define-syntax field->pointer
;;;;;  (syntax-rules ()
;;;;;    ((_ offset) (lambda (bv) (bytevector->pointer bv offset)))))

(define-syntax unchecked-field->alist-entry
  (lambda (x)
    (syntax-case x (field struct-field)
      ((_ field-name field (field-type field-subtype) offset size)
       (eq? '* (syntax->datum #'field-type))
       #`(lambda (obj)
           (cons
            '#,(syntax-string->syntax-symbol x #'field-name)
            ((field-ref field-type offset size) (cdr obj)))))

      ((_ field-name field (field-type field-subtype) offset size)
       (or (eq? 'struct (syntax->datum #'field-type))
           (eq? 'array (syntax->datum #'field-type)))
       #`(lambda (obj)
           (cons
            '#,(syntax-string->syntax-symbol x #'field-name)
            ((field->pointer offset) (cdr obj)))))

      ((_ field-name field field-type offset size)
       (or (eq? 'struct (syntax->datum #'field-type))
           (eq? 'array (syntax->datum #'field-type)))
       #`(lambda (obj)
           (cons
            '#,(syntax-string->syntax-symbol x #'field-name)
            ((field->pointer offset) (cdr obj)))))

      ((_ field-name field field-type offset size)
       #`(lambda (obj)
           (cons
            '#,(syntax-string->syntax-symbol x #'field-name)
            ((field-ref field-type offset size) (cdr obj)))))
      )))

;;-------------------------------------------------------------------------

(define-syntax field->pointer
  (syntax-rules ()
    ((_ offset) (lambda (bv) (bytevector->pointer bv offset)))))

(define-syntax field-ref
  (syntax-rules (int uint bool float *)
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
  (syntax-rules (int uint bool float *)
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
;;-------------------------------------------------------------------------
;;-------------------------------------------------------------------------
;;-------------------------------------------------------------------------
;;;
;;; A more easily ported (one hopes) implementation, UNDER DEVELOPMENT.
;;;
;;; The idea here is to keep exports separate from defines, to support
;;; module systems that do not allow a free mixture of exports and
;;; defines, and to avoid the Guile fluids in the earlier
;;; implementation.
;;;

;;;
;;; Note: In the following, the binding ‘x’ generally represents the
;;; syntax context. But we want to get rid of most of those
;;; instances. See the noisy ‘FIXME’ below.
;;;

;;; FIXME FIXME FIXME:
;;; FIXME FIXME FIXME: Use (current-form) and (current-subform) more.
;;; FIXME FIXME FIXME:

(define-syntax define-public-api
  (lambda (x)
    (syntax-case x ()
      ((_ . forms)
       (parameterize ((current-form x)
                      (current-subform #f))
         (let-values (((exports defines)
                       (expand-multiple-forms (syntax->datum #'forms))))
           #`(begin (export #,@exports) #,@defines)))))))

(define-syntax define-private-api
  (lambda (x)
    (syntax-case x ()
      ((_ . forms)
       (parameterize ((current-form x))
         (let-values (((exports<--these-are-ignored defines)
                       (expand-multiple-forms (syntax->datum #'forms))))
           #`(begin #,@defines)))))))

(eval-early

 ;; Dynamically scoped bindings to avoid passing around a lot of
 ;; syntax context.
 (define current-form (make-parameter #f))    ; A syntax object.
 (define current-subform (make-parameter #f)) ; #f or a syntax object.

 (define (combine-expansions-list expansions)
   (fold-left (lambda (prior expans)
                (list (append (car prior) (car expans))
                      (append (cadr prior) (cadr expans))))
              '(() ()) expansions))

 (define (combine-expansions . expansions)
   (combine-expansions-list expansions))

 (define (expand-multiple-forms forms)
   (let ((expansion             ; (list (list exports) (list defines))
          (combine-expansions-list
           (map (cut expand-form <>) forms))))
     (values (car expansion) (cadr expansion))))

 (define (expand-form form)
   (parameterize ((current-form (datum->syntax (current-form) form))
                  (current-subform #f))
     (match form
            (('sizeof (? string? type-name) (? integer? size))
             (expand-<sizeof> type-name size))
            (('struct (? string? type-name) (? integer? size))
             (expand-<struct> type-name size))
            (('field ('* (? symbol? field-subtype))
                     (? string? struct-name) (? string? field-name)
                     (? integer? offset) (? integer? size))
             (combine-expansions
              (expand-<struct>:<field>->pointer struct-name field-name offset size)
              (expand-<struct>:<field>-ref '* struct-name field-name offset size)
              (expand-<struct>:<field>-set! '* struct-name field-name offset size)
              ))
;;;;;;;       (expand-field-dereferencing (field-type field-subtype) struct-name field-name offset size))
            )))

 (define (expand-<sizeof> type-name size)
   (let ((x (current-form)))
     (list
      (list (type-sizeof-var x type-name)) ; Example: sizeof-SplineChar
      (list #`(define #,(type-sizeof-var x type-name) #,size)))))

 (define (expand-<struct> type-name size)
   (let* ((x (current-form))
          (tag (type-tag x type-name)))
     (list
      (list
       (<type?> x type-name)               ; Example: SplineChar?
       (check-<type> x type-name)          ; Example: check-SplineChar
       (pointer-><type> x type-name)       ; Example: pointer->SplineChar
       (unchecked-<type>->pointer x type-name) ; Example: unchecked-SplineChar->pointer
       (<type>->pointer x type-name)       ; Example: SplineChar->pointer
       (unchecked-<type>-ref x type-name)  ; Example: unchecked-SplineChar-ref
       (<type>-ref x type-name)            ; Example: SplineChar-ref
       (malloc-<type> x type-name)         ; Example: malloc-SplineChar
       (unchecked-free-<type> x type-name) ; Example: unchecked-free-SplineChar
       (free-<type> x type-name)           ; Example: free-SplineChar
       (gc-malloc-<type> x type-name)      ; Example: gc-malloc-SplineChar
       (unchecked-gc-free-<type> x type-name) ; Example: unchecked-gc-free-SplineChar
       (gc-free-<type> x type-name)        ; Example: gc-free-SplineChar
       )
      (list
       #`(define #,(<type?> x type-name)
           (lambda (obj)
             (cond
              ((not (pair? obj)) #f)
              ((not (eq? '#,tag (car obj))) #f)
              ((not (bytevector? (cdr obj))) #f)
              ((not (= (bytevector-length (cdr obj)) #,size)) #f)
              (else #t))))

       #`(define #,(throw-failed-check-<type> x type-name)
           (lambda (caller err-msg obj)
             (assertion-violation
              caller
              (if err-msg
                  (format28
                   "Expected ~a API struct of type `~a', but ~a"
                   pkg-info:package-name #,type-name err-msg)
                  (format28
                   "Expected ~a API struct of type `~a'"
                   pkg-info:package-name #,type-name))
              obj)))

       #`(define #,(check-<type> x type-name)
           (lambda (caller obj)
             (cond
              ((not (pair? obj))
               (#,(throw-failed-check-<type> x type-name)
                caller
                (format28 "~s is not a pair" obj)
                obj))
              ((not (eq? '#,tag (car obj)))
               (#,(throw-failed-check-<type> x type-name)
                caller
                (format28 "(car ~s) is not ~s" obj '#,tag)
                obj))
              ((not (bytevector? (cdr obj)))
               (#,(throw-failed-check-<type> x type-name)
                caller
                (format28 "(cdr ~s) is not a bytevector" obj)
                obj))
              ((not (= (bytevector-length (cdr obj)) #,size))
               (#,(throw-failed-check-<type> x type-name)
                caller
                "bytevector length is wrong"
                obj))
              (else *unspecified*))))

       #`(define #,(pointer-><type> x type-name)
           (lambda (ptr)
             (cons '#,tag (pointer->bytevector ptr #,size))))

       #`(define #,(unchecked-<type>->pointer x type-name)
           (case-lambda
             ((obj) (bytevector->pointer (cdr obj)))
             ((obj i)
              ;; Return a pointer to the ith structure
              ;; relative to this one in an array.
              (let ((p (bytevector->pointer (cdr obj))))
                (make-pointer (+ (pointer-address p) (* i #,size)))))))

       #`(define #,(<type>->pointer x type-name)
           (case-lambda
             ((obj)
              (#,(check-<type> x type-name) '#,(<type>->pointer x type-name) obj)
              (bytevector->pointer (cdr obj)))
             ((obj i)
              ;; Return a pointer to the ith structure
              ;; relative to this one in an array.
              (#,(check-<type> x type-name) '#,(<type>->pointer x type-name) obj)
              (#,(unchecked-<type>->pointer x type-name)
               obj i))))

       #`(define #,(unchecked-<type>-ref x type-name)
           (case-lambda
             ((obj)
              ;; This merely copies the tagged
              ;; bytevector. It exists mainly for
              ;; consistency with other uses of ‘-ref’ in
              ;; this module.
              (#,(pointer-><type> x type-name)
               (bytevector->pointer (cdr obj)))) ((obj i)
              ;; This gives the ith structure relative to
              ;; this one in an array.
              (let ((p (bytevector->pointer (cdr obj))))
                (#,(pointer-><type> x type-name)
                 (make-pointer (+ (pointer-address p) (* i #,size))))))))

       #`(define #,(<type>-ref x type-name)
           (case-lambda
             ((obj)
              ;; This merely copies the tagged
              ;; bytevector. It exists mainly for
              ;; consistency with other uses of ‘-ref’ in
              ;; this module.
              (#,(check-<type> x type-name) '#,(<type>->pointer x type-name) obj)
              (#,(unchecked-<type>-ref x type-name) obj))
             ((obj i )
              ;; This gives the ith structure relative to
              ;; this one in an array.
              (#,(check-<type> x type-name) '#,(<type>->pointer x type-name) obj)
              (#,(unchecked-<type>-ref x type-name) obj i))))

       #`(define #,(malloc-<type> x type-name)
           (case-lambda
             (() (cons '#,tag
                       (pointer->bytevector (c:zalloc #,size) #,size)))
             ((n)
              ;; Allocate a contiguous array of n structs,
              ;; with the tagged bytevector pointing at the
              ;; first struct in the array.
              (unless (<= 1 n)
                (assertion-violation #,(gc-malloc-<type> x type-name)
                                     "the argument must be >= 1" n))
              (cons '#,tag
                    (pointer->bytevector (c:zalloc (* n #,size)) #,size)))))

       #`(define #,(unchecked-free-<type> x type-name)
           (lambda (obj)
             (c:free (#,(unchecked-<type>->pointer x type-name) obj))))

       #`(define #,(free-<type> x type-name)
           (lambda (obj)
             (c:free (#,(<type>->pointer x type-name) obj))))

       #`(define #,(gc-malloc-<type> x type-name)
           (case-lambda
             (() (cons '#,tag
                       (pointer->bytevector (c:gc-zalloc #,size) #,size)))
             ((n)
              ;; Allocate a contiguous array of n structs, with the
              ;; tagged bytevector pointing at the first struct in the
              ;; array.
              (unless (<= 1 n)
                (assertion-violation #,(gc-malloc-<type> x type-name)
                                     "the argument must be >= 1" n))
              (cons '#,tag
                    (pointer->bytevector (c:gc-zalloc (* n #,size)) #,size)))))

       #`(define #,(unchecked-gc-free-<type> x type-name)
           (lambda (obj)
             (c:gc-free (#,(unchecked-<type>->pointer x type-name) obj))))

       #`(define #,(gc-free-<type> x type-name)
           (lambda (obj)
             (c:gc-free (#,(<type>->pointer x type-name) obj))))
       ))))

 (define (expand-<struct>:<field>->pointer struct-name field-name offset size)
   (let* ((x (current-form))
          (check (check-<type> x struct-name))
          (unchecked-func (unchecked-<type>:<field>->pointer
                           x struct-name field-name))
          (checked-func (<type>:<field>->pointer x struct-name field-name)))
     (list
      (list
       (unchecked-<type>:<field>->pointer x struct-name field-name) ; Example: unchecked-SplineChar:name->pointer
       (<type>:<field>->pointer x struct-name field-name) ; Example: SplineChar:name->pointer
       )
      (list
       #`(define #,unchecked-func
           (case-lambda
             ((obj) (#,offset->pointer (cdr obj) #,offset))
             ((obj i) (#,offset->pointer (cdr obj) (index->offset #,offset i #,size)))))

       #`(define #,checked-func
           (case-lambda
             ((obj)
              (#,check #,checked-func obj)
              (#,offset->pointer (cdr obj) #,offset))
             ((obj i)
              (#,check #,checked-func obj)
              ;; Get a pointer to an array element.
              (#,offset->pointer (cdr obj) (index->offset #,offset i #,size)))))
   ))))

 (define (expand-<struct>:<field>-ref field-type struct-name field-name offset size)
   (let* ((x (current-form))
          (ref-func (offset-ref field-type size))
          (check (check-<type> x struct-name))
          (unchecked-func (unchecked-<type>:<field>-ref x struct-name field-name))
          (checked-func (<type>:<field>-ref x struct-name field-name)))
     (list
      (list
       (unchecked-<type>:<field>-ref x struct-name field-name) ; Example: unchecked-SplineChar:width-ref
       (<type>:<field>-ref x struct-name field-name) ; Example: SplineChar:width-ref
       )
      (list
       #`(define #,unchecked-func
           (case-lambda
             ((obj) (#,ref-func (cdr obj) #,offset))
             ((obj i) (#,ref-func (cdr obj) (index->offset #,offset i #,size)))))

     #`(define #,checked-func
         (case-lambda
           ((obj)
            (#,check #,checked-func obj)
            (#,ref-func (cdr obj) #,offset))
           ((obj i)
            (#,check #,checked-func obj)
            ;; Get the contents of an array element.
            (#,ref-func (cdr obj) (index->offset #,offset i #,size)))))
     ))))

 (define (expand-<struct>:<field>-set! field-type struct-name field-name offset size)
   (let* ((x (current-form))
          (set!-func (offset-set! field-type size))
          (check (check-<type> x struct-name))
          (unchecked-func (unchecked-<type>:<field>-set! x struct-name field-name))
          (checked-func (<type>:<field>-set! x struct-name field-name)))
     (list
      (list
       (unchecked-<type>:<field>-set! x struct-name field-name) ; Example: unchecked-SplineChar:width-set!
       (<type>:<field>-set! x struct-name field-name) ; Example: SplineChar:width-set!
       )
      (list
       #`(define #,unchecked-func
           (case-lambda
             ((obj v) (#,set!-func (cdr obj) #,offset v))
             ((obj i v) (#,set!-func (cdr obj) (index->offset #,offset i #,size) v))))

     #`(define #,checked-func
         (case-lambda
           ((obj v)
            (#,check #,checked-func obj)
            (#,set!-func (cdr obj) #,offset v))
           ((obj i v)
            (#,check #,checked-func obj)
            ;; Get the contents of an array element.
            (#,set!-func (cdr obj) (index->offset #,offset i #,size) v))))
     ))))

 (define offset->pointer
   #'(lambda (bv offset) (bytevector->pointer bv offset)))

 (define (offset-ref field-type size)
   (match
    (list field-type size)
    (('int 1) #'(lambda (bv offset) (bytevector-s8-ref bv offset)))
    (('int 2) #'(lambda (bv offset) (bytevector-s16-native-ref bv offset)))
    (('int 4) #'(lambda (bv offset) (bytevector-s32-native-ref bv offset)))
    (('int 8) #'(lambda (bv offset) (bytevector-s64-native-ref bv offset)))
    (('uint 1) #'(lambda (bv offset) (bytevector-u8-ref bv offset)))
    (('uint 2) #'(lambda (bv offset) (bytevector-u16-native-ref bv offset)))
    (('uint 4) #'(lambda (bv offset) (bytevector-u32-native-ref bv offset)))
    (('uint 8) #'(lambda (bv offset) (bytevector-u64-native-ref bv offset)))
    (('bool 1) #'(lambda (bv offset) (not (zero? (bytevector-u8-ref bv offset)))))
    (('bool 2) #'(lambda (bv offset) (not (zero? (bytevector-u16-native-ref bv offset)))))
    (('bool 4) #'(lambda (bv offset) (not (zero? (bytevector-u32-native-ref bv offset)))))
    (('bool 8) #'(lambda (bv offset) (not (zero? (bytevector-u64-native-ref bv offset)))))
    (('float 4) #'(lambda (bv offset) (bytevector-ieee-single-native-ref bv offset)))
    (('float 8) #'(lambda (bv offset) (bytevector-ieee-double-native-ref bv offset)))
    (('* 1) #'(lambda (bv offset) (make-pointer (bytevector-u8-ref bv offset))))
    (('* 2) #'(lambda (bv offset) (make-pointer (bytevector-u16-native-ref bv offset))))
    (('* 4) #'(lambda (bv offset) (make-pointer (bytevector-u32-native-ref bv offset))))
    (('* 8) #'(lambda (bv offset) (make-pointer (bytevector-u64-native-ref bv offset))))
    (else (syntax-violation #f "illegal field type or size in API instruction"
                            (syntax->datum (current-form))
                            (syntax->datum (current-subform))))
    ))

 (define (offset-set! field-type size)
   (match
    (list field-type size)
    (('int 1) #'(lambda (bv offset v) (bytevector-s8-set! bv offset v)))
    (('int 2) #'(lambda (bv offset v) (bytevector-s16-native-set! bv offset v)))
    (('int 4) #'(lambda (bv offset v) (bytevector-s32-native-set! bv offset v)))
    (('int 8) #'(lambda (bv offset v) (bytevector-s64-native-set! bv offset v)))
    (('uint 1) #'(lambda (bv offset v) (bytevector-u8-set! bv offset v)))
    (('uint 2) #'(lambda (bv offset v) (bytevector-u16-native-set! bv offset v)))
    (('uint 4) #'(lambda (bv offset v) (bytevector-u32-native-set! bv offset v)))
    (('uint 8) #'(lambda (bv offset v) (bytevector-u64-native-set! bv offset v)))
    (('bool 1) #'(lambda (bv offset v) (not (zero? (bytevector-u8-set! bv offset v)))))
    (('bool 2) #'(lambda (bv offset v) (not (zero? (bytevector-u16-native-set! bv offset v)))))
    (('bool 4) #'(lambda (bv offset v) (not (zero? (bytevector-u32-native-set! bv offset v)))))
    (('bool 8) #'(lambda (bv offset v) (not (zero? (bytevector-u64-native-set! bv offset v)))))
    (('float 4) #'(lambda (bv offset v) (bytevector-ieee-single-native-set! bv offset v)))
    (('float 8) #'(lambda (bv offset v) (bytevector-ieee-double-native-set! bv offset v)))
    (('* 1) #'(lambda (bv offset v) (make-pointer (bytevector-u8-set! bv offset v))))
    (('* 2) #'(lambda (bv offset v) (make-pointer (bytevector-u16-native-set! bv offset v))))
    (('* 4) #'(lambda (bv offset v) (make-pointer (bytevector-u32-native-set! bv offset v))))
    (('* 8) #'(lambda (bv offset v) (make-pointer (bytevector-u64-native-set! bv offset v))))
    (else (syntax-violation #f "illegal field type or size in API instruction"
                            (syntax->datum (current-form))
                            (syntax->datum (current-subform))))
    ))

) ;; end of eval-early

;;-------------------------------------------------------------------------

(define-syntax index->offset
  (syntax-rules ()
    ((_ offset i size) (+ offset (* i size)))))

(define-syntax index->pointer
  (syntax-rules ()
    ((_ p i size) (make-pointer (+ (pointer-address p) (* i size))))))

;;-------------------------------------------------------------------------

#!
(define-public-api
  (sizeof "barf" 1234)
  (struct "barf" 1234)
  (field (* int) "barf" "fld1" 20 4)
  (field (* int) "barf" "fld2" 24 1)
  )

(write sizeof-barf (current-error-port))
(write barf? (current-error-port))
(write gc-free-barf (current-error-port))
(write (gc-malloc-barf 30) (current-error-port))
;;(barf-ref 3)
(write unchecked-barf:fld1->pointer (current-error-port))
(write barf:fld1->pointer (current-error-port))
(write unchecked-barf:fld1-ref (current-error-port))
(write barf:fld1-ref (current-error-port))
(write barf:fld2-ref (current-error-port))
(write barf:fld2-set! (current-error-port))
!#

;;-------------------------------------------------------------------------
