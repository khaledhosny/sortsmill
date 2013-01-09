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

(export define-public-api
        define-private-api)

(import (sortsmillff machine)
        (sortsmillff alloc)
        (sortsmillff pkg-info)
        (rnrs)
        (system foreign)
        (only (srfi :26) cut cute)
        (ice-9 match) ;; Alex Shinn’s public-domain matcher.
        )

(define-syntax eval-early
  (syntax-rules ()
    ((_ e e* ...) (eval-when (compile load eval) e e* ...))))

(eval-early

 ;; Define ‘format28’ as a format function resembling that of SRFI-28;
 ;; we will use it instead of the native format functions, to help make
 ;; this module more portable.
 ;;
 ;; See http://srfi.schemers.org/srfi-28/srfi-28.html
 ;;
 (define (format28 format-string . objects)
   (apply (cut simple-format #f format-string <...>) objects))

 ) ;; end of eval-early

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

;;-------------------------------------------------------------------------
;;;
;;; Note: In the following, the binding ‘x’ generally represents the
;;; syntax context. But we want to get rid of most of those
;;; instances. See the noisy ‘FIXME’ below.
;;;

;;; FIXME FIXME FIXME:
;;; FIXME FIXME FIXME: Use (current-form) and (current-subform) more.
;;; FIXME FIXME FIXME:

;;; FIXME FIXME FIXME:
;;; FIXME FIXME FIXME: Have API instructions be made with symbols instead of strings.
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
       (parameterize ((current-form x)
                      (current-subform #f))
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
              (expand-<struct>:<field>-dref '* field-subtype struct-name field-name
                                            offset size)))

            (('field ((and (or 'struct 'array) field-type)
                      (? symbol? field-subtype))
                     (? string? struct-name) (? string? field-name)
                     (? integer? offset) (? integer? size))
             (combine-expansions
              (expand-<struct>:<field>->pointer struct-name field-name offset size)
              (expand-<struct>:<field>-ref field-type field-subtype struct-name
                                           field-name offset size)))

            (('field (and (or 'struct 'array) field-type)
                     (? string? struct-name) (? string? field-name)
                     (? integer? offset) (? integer? size))
             (expand-<struct>:<field>->pointer struct-name field-name offset size))

            (('field (? symbol? field-type)
                     (? string? struct-name) (? string? field-name)
                     (? integer? offset) (? integer? size))
             (combine-expansions
              (expand-<struct>:<field>->pointer struct-name field-name offset size)
              (expand-<struct>:<field>-ref field-type struct-name field-name offset size)
              (expand-<struct>:<field>-set! field-type struct-name field-name offset size)))

            (('struct-> (? string? struct-name)
                        ((? string? field-name) (? symbol? kind)
                         field-type
                         (? integer? offset) (? integer? size)) ...)
             ;; For now, ignore struct-> instructions. They have many
             ;; potential uses, however.
             '(() ()))

            (else (syntax-violation #f "illegal API instruction"
                                    (syntax->datum (current-form))
                                    (syntax->datum (current-subform))))
            )))

 (define (expand-<sizeof> type-name size)
   (let ((sizeof-value (type-sizeof-var (current-form) type-name)))
     (list
      (list sizeof-value)               ; Example: sizeof-SplineChar
      (list #`(define #,sizeof-value #,size)))))

;;;
;;; FIXME: Neaten up expand-<struct> like its brethren.
;;;
 (define (expand-<struct> type-name size)
   (let ((tag (type-tag (current-form) type-name)))
     (list
      (list
       (<type?> (current-form) type-name)               ; Example: SplineChar?
       (check-<type> (current-form) type-name)          ; Example: check-SplineChar
       (pointer-><type> (current-form) type-name)       ; Example: pointer->SplineChar
       (unchecked-<type>->pointer (current-form) type-name) ; Example: unchecked-SplineChar->pointer
       (<type>->pointer (current-form) type-name)       ; Example: SplineChar->pointer
       (unchecked-<type>-ref (current-form) type-name)  ; Example: unchecked-SplineChar-ref
       (<type>-ref (current-form) type-name)            ; Example: SplineChar-ref
       (malloc-<type> (current-form) type-name)         ; Example: malloc-SplineChar
       (unchecked-free-<type> (current-form) type-name) ; Example: unchecked-free-SplineChar
       (free-<type> (current-form) type-name)           ; Example: free-SplineChar
       (gc-malloc-<type> (current-form) type-name)      ; Example: gc-malloc-SplineChar
       (unchecked-gc-free-<type> (current-form) type-name) ; Example: unchecked-gc-free-SplineChar
       (gc-free-<type> (current-form) type-name)        ; Example: gc-free-SplineChar
       )
      (list
       #`(define #,(<type?> (current-form) type-name)
           (lambda (obj)
             (cond
              ((not (pair? obj)) #f)
              ((not (eq? '#,tag (car obj))) #f)
              ((not (bytevector? (cdr obj))) #f)
              ((not (= (bytevector-length (cdr obj)) #,size)) #f)
              (else #t))))

       #`(define #,(throw-failed-check-<type> (current-form) type-name)
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

       #`(define #,(check-<type> (current-form) type-name)
           (lambda (caller obj)
             (cond
              ((not (pair? obj))
               (#,(throw-failed-check-<type> (current-form) type-name)
                caller
                (format28 "~s is not a pair" obj)
                obj))
              ((not (eq? '#,tag (car obj)))
               (#,(throw-failed-check-<type> (current-form) type-name)
                caller
                (format28 "(car ~s) is not ~s" obj '#,tag)
                obj))
              ((not (bytevector? (cdr obj)))
               (#,(throw-failed-check-<type> (current-form) type-name)
                caller
                (format28 "(cdr ~s) is not a bytevector" obj)
                obj))
              ((not (= (bytevector-length (cdr obj)) #,size))
               (#,(throw-failed-check-<type> (current-form) type-name)
                caller
                "bytevector length is wrong"
                obj))
              (else *unspecified*))))

       #`(define #,(pointer-><type> (current-form) type-name)
           (lambda (ptr)
             (cons '#,tag (pointer->bytevector ptr #,size))))

       #`(define #,(unchecked-<type>->pointer (current-form) type-name)
           (case-lambda
             ((obj) (bytevector->pointer (cdr obj)))
             ((obj i)
              ;; Return a pointer to the ith structure relative to
              ;; this one in an array.
              (let ((p (bytevector->pointer (cdr obj))))
                (make-pointer (+ (pointer-address p) (* i #,size)))))))

       #`(define #,(<type>->pointer (current-form) type-name)
           (case-lambda
             ((obj)
              (#,(check-<type> (current-form) type-name) '#,(<type>->pointer (current-form) type-name) obj)
              (bytevector->pointer (cdr obj)))
             ((obj i)
              ;; Return a pointer to the ith structure relative to
              ;; this one in an array.
              (#,(check-<type> (current-form) type-name) '#,(<type>->pointer (current-form) type-name) obj)
              (#,(unchecked-<type>->pointer (current-form) type-name)
               obj i))))

       #`(define #,(unchecked-<type>-ref (current-form) type-name)
           (case-lambda
             ((obj)
              ;; This merely copies the tagged bytevector. It exists
              ;; mainly for consistency with other uses of ‘-ref’ in
              ;; this module.
              (#,(pointer-><type> (current-form) type-name)
               (bytevector->pointer (cdr obj))))
             ((obj i)
              ;; This gives the ith structure relative to this one in
              ;; an array.
              (let ((p (bytevector->pointer (cdr obj))))
                (#,(pointer-><type> (current-form) type-name)
                 (make-pointer (+ (pointer-address p) (* i #,size))))))))

       #`(define #,(<type>-ref (current-form) type-name)
           (case-lambda
             ((obj)
              ;; This merely copies the tagged bytevector. It exists
              ;; mainly for consistency with other uses of ‘-ref’ in
              ;; this module.
              (#,(check-<type> (current-form) type-name) '#,(<type>->pointer (current-form) type-name) obj)
              (#,(unchecked-<type>-ref (current-form) type-name) obj))
             ((obj i )
              ;; This gives the ith structure relative to this one in
              ;; an array.
              (#,(check-<type> (current-form) type-name) '#,(<type>->pointer (current-form) type-name) obj)
              (#,(unchecked-<type>-ref (current-form) type-name) obj i))))

       #`(define #,(malloc-<type> (current-form) type-name)
           (case-lambda
             (() (cons '#,tag
                       (pointer->bytevector (c:zalloc #,size) #,size)))
             ((n)
              ;; Allocate a contiguous array of n structs, with the
              ;; tagged bytevector pointing at the first struct in the
              ;; array.
              (unless (<= 1 n)
                (assertion-violation #,(gc-malloc-<type> (current-form) type-name)
                                     "the argument must be >= 1" n))
              (cons '#,tag
                    (pointer->bytevector (c:zalloc (* n #,size)) #,size)))))

       #`(define #,(unchecked-free-<type> (current-form) type-name)
           (lambda (obj)
             (c:free (#,(unchecked-<type>->pointer (current-form) type-name) obj))))

       #`(define #,(free-<type> (current-form) type-name)
           (lambda (obj)
             (c:free (#,(<type>->pointer (current-form) type-name) obj))))

       #`(define #,(gc-malloc-<type> (current-form) type-name)
           (case-lambda
             (() (cons '#,tag
                       (pointer->bytevector (c:gc-zalloc #,size) #,size)))
             ((n)
              ;; Allocate a contiguous array of n structs, with the
              ;; tagged bytevector pointing at the first struct in the
              ;; array.
              (unless (<= 1 n)
                (assertion-violation #,(gc-malloc-<type> (current-form) type-name)
                                     "the argument must be >= 1" n))
              (cons '#,tag
                    (pointer->bytevector (c:gc-zalloc (* n #,size)) #,size)))))

       #`(define #,(unchecked-gc-free-<type> (current-form) type-name)
           (lambda (obj)
             (c:gc-free (#,(unchecked-<type>->pointer (current-form) type-name) obj))))

       #`(define #,(gc-free-<type> (current-form) type-name)
           (lambda (obj)
             (c:gc-free (#,(<type>->pointer (current-form) type-name) obj))))
       ))))

 (define (expand-<struct>:<field>->pointer struct-name field-name offset size)
   (let ((check (check-<type> (current-form) struct-name))
         (unchecked-func (unchecked-<type>:<field>->pointer (current-form) struct-name field-name))
         (checked-func (<type>:<field>->pointer (current-form) struct-name field-name)))
     (list
      (list unchecked-func ; Example: unchecked-SplineChar:name->pointer
            checked-func   ; Example: SplineChar:name->pointer
            )
      (list
       #`(define #,unchecked-func
           (case-lambda
             ((obj) (#,bv-offset->pointer (cdr obj) #,offset))
             ((obj i) (#,bv-offset->pointer (cdr obj) (index->offset #,offset i #,size)))))

       #`(define #,checked-func
           (case-lambda
             ((obj)
              (#,check #,checked-func obj)
              (#,bv-offset->pointer (cdr obj) #,offset))
             ((obj i)
              (#,check #,checked-func obj)
              ;; Get a pointer to an array element.
              (#,bv-offset->pointer (cdr obj) (index->offset #,offset i #,size)))))
       ))))

 (define expand-<struct>:<field>-ref
   (case-lambda
     ((field-type struct-name field-name offset size)
      ;;
      ;; Functions to get the value of a primitive type.
      ;;
      (let ((ref-func (bv-offset-ref field-type size))
            (check (check-<type> (current-form) struct-name))
            (unchecked-func (unchecked-<type>:<field>-ref (current-form) struct-name field-name))
            (checked-func (<type>:<field>-ref (current-form) struct-name field-name)))
        (list
         (list unchecked-func   ; Example: unchecked-SplineChar:width-ref
               checked-func     ; Example: SplineChar:width-ref
               )
         (list
          #`(define #,unchecked-func
              (case-lambda
                ((obj) (#,ref-func (cdr obj) #,offset))
;;; FIXME: This will not work, because TIG computes ‘size’ as the size
;;; of the entire array. TIG cannot infer the size of a single array
;;; element.
;;;;;                ((obj i) (#,ref-func (cdr obj) (index->offset #,offset i #,size)))
                ))

          #`(define #,checked-func
              (case-lambda
                ((obj)
                 (#,check #,checked-func obj)
                 (#,ref-func (cdr obj) #,offset))
;;; FIXME: This will not work, because TIG computes ‘size’ as the size
;;; of the entire array. TIG cannot infer the size of a single array
;;; element.
;;;;;                ((obj i)
;;;;;                 (#,check #,checked-func obj)
;;;;;                 ;; Get the contents of an array element.
;;;;;                 (#,ref-func (cdr obj) (index->offset #,offset i #,size)))
                ))
          ))))

     ((field-type field-subtype struct-name field-name offset size)
      ;;
      ;; Functions to get the ‘value’ of a sub-structure.
      ;;
      (let ((to-subtype (pointer-><type> (current-form) field-subtype))
            (subtype-size (type-sizeof-var (current-form) field-subtype))
            (check (check-<type> (current-form) struct-name))
            (unchecked-func (unchecked-<type>:<field>-ref (current-form) struct-name field-name))
            (checked-func (<type>:<field>-ref (current-form) struct-name field-name)))
        (list
         (list unchecked-func   ; Example: unchecked-SplineChar:width-ref
               checked-func     ; Example: SplineChar:width-ref
               )
         (list
          #`(define #,unchecked-func
              (case-lambda
                ((obj) (#,to-subtype (#,bv-offset->pointer (cdr obj) #,offset)))
                ((obj i) (#,to-subtype (index->pointer
                                        (#,bv-offset->pointer (cdr obj) #,offset)
                                        i #,subtype-size)))))

          #`(define #,checked-func
              (case-lambda
                ((obj)
                 (#,check #,checked-func obj)
                 (#,unchecked-func obj))
                ((obj i)
                 (#,check #,checked-func obj)
                 (#,unchecked-func obj i))))
          ))))
     ))

 (define (expand-<struct>:<field>-set! field-type struct-name field-name offset size)
   (let ((set!-func (bv-offset-set! field-type size))
         (check (check-<type> (current-form) struct-name))
         (unchecked-func (unchecked-<type>:<field>-set! (current-form) struct-name field-name))
         (checked-func (<type>:<field>-set! (current-form) struct-name field-name)))
     (list
      (list unchecked-func  ; Example: unchecked-SplineChar:width-set!
            checked-func    ; Example: SplineChar:width-set!
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

 (define (expand-<struct>:<field>-dref field-type field-subtype
                                       struct-name field-name offset size)
   (let ((ref-func (bv-offset-ref field-type size))
         (to-subtype (pointer-><type> (current-form) field-subtype))
         (subtype-size (type-sizeof-var (current-form) field-subtype))
         (check (check-<type> (current-form) struct-name))
         (unchecked-func (unchecked-<type>:<field>-dref (current-form) struct-name field-name))
         (checked-func (<type>:<field>-dref (current-form) struct-name field-name)))
     (list
      (list unchecked-func ; Example: unchecked-CharViewBase:next-dref
            checked-func   ; Example: CharViewBase:next-dref
            )
      (list
       #`(define #,unchecked-func
           (case-lambda
             ((obj) (#,to-subtype (#,ref-func (cdr obj) #,offset)))
             ((obj i) (#,to-subtype (index->pointer
                                     (#,ref-func (cdr obj) #,offset)
                                     i #,subtype-size)))))

       #`(define #,checked-func
           (case-lambda
             ((obj)
              (#,check #,checked-func obj)
              (#,unchecked-func obj))
             ((obj i)
              (#,check #,checked-func obj)
              (#,unchecked-func obj i))))
       ))))

 (define bv-offset->pointer
   #'(lambda (bv offset) (bytevector->pointer bv offset)))

 (define (bv-offset-ref field-type size)
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

 (define (bv-offset-set! field-type size)
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
    (('bool 1) #'(lambda (bv offset v) (bytevector-u8-set! bv offset (if v 1 0))))
    (('bool 2) #'(lambda (bv offset v) (bytevector-u16-native-set! bv offset (if v 1 0))))
    (('bool 4) #'(lambda (bv offset v) (bytevector-u32-native-set! bv offset (if v 1 0))))
    (('bool 8) #'(lambda (bv offset v) (bytevector-u64-native-set! bv offset (if v 1 0))))
    (('float 4) #'(lambda (bv offset v) (bytevector-ieee-single-native-set! bv offset v)))
    (('float 8) #'(lambda (bv offset v) (bytevector-ieee-double-native-set! bv offset v)))
    (('* 1) #'(lambda (bv offset v) (bytevector-u8-set! bv offset (pointer-address v))))
    (('* 2) #'(lambda (bv offset v) (bytevector-u16-native-set! bv offset (pointer-address v))))
    (('* 4) #'(lambda (bv offset v) (bytevector-u32-native-set! bv offset (pointer-address v))))
    (('* 8) #'(lambda (bv offset v) (bytevector-u64-native-set! bv offset (pointer-address v))))
    (else (syntax-violation #f "illegal field type or size in API instruction"
                            (syntax->datum (current-form))
                            (syntax->datum (current-subform))))
    ))

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

 ) ;; end of eval-early

;;-------------------------------------------------------------------------

(define-syntax index->offset
  (syntax-rules ()
    ((_ offset i size) (+ offset (* i size)))))

(define-syntax index->pointer
  (syntax-rules ()
    ((_ p i size) (make-pointer (+ (pointer-address p) (* i size))))))

;;-------------------------------------------------------------------------
