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

(define-module (sortsmillff internal-types-syntax))

(use-modules
   (sortsmillff pkg-info)
   (rnrs bytevectors)
   (system foreign)
   (ice-9 match)
   (srfi srfi-26)                       ; ‘cut’ and ‘cute’.
   )

(export
   ;; Fluid for ‘Are API-definitions being exported?’
   %ff:interface-exported?

   ;; Are API-definitions being exported?
   ff:interface-exported?

   ;; Export enclosed API-definitions.
   with-ff:interface-exported

   ;; Follow an API-definition instruction.
   define-ff:interface

   ;; Read instructions from a port.
   read-define-ff:interface
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

   (define %ff:interface-exported? (make-fluid #f))

   (define ff:interface-exported?
      (case-lambda
         (() (fluid-ref %ff:interface-exported?))
         ((v) (fluid-set! %ff:interface-exported? (not (not v))))))

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

   (define build-type-related-symbol
      (case-lambda
         ((constructor syntax-context type-name)
          (datum->syntax syntax-context
             (string->symbol (constructor (syntax->datum type-name)))))
         ((constructor syntax-context struct-name field-name)
          (datum->syntax syntax-context
             (string->symbol (constructor
                                (syntax->datum struct-name)
                                (syntax->datum field-name)))))))

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

   (define (pointer->struct-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "pointer->ff:" <>)
         syntax-context type-name))

   (define (struct->pointer-func syntax-context type-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> "->pointer")
         syntax-context type-name))

   (define (unchecked-struct->pointer-func syntax-context type-name)
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

   (define (struct-field-ref-func syntax-context struct-name field-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> ":" <> "-ref")
         syntax-context struct-name field-name))

   (define (unchecked-struct-field-ref-func syntax-context struct-name field-name)
      (build-type-related-symbol
         (cute string-append "unchecked-ff:" <> ":" <> "-ref")
         syntax-context struct-name field-name))

   (define (struct-field-set!-func syntax-context struct-name field-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> ":" <> "-set!")
         syntax-context struct-name field-name))

   (define (unchecked-struct-field-set!-func syntax-context struct-name field-name)
      (build-type-related-symbol
         (cute string-append "unchecked-ff:" <> ":" <> "-set!")
         syntax-context struct-name field-name))

   (define (struct-field->pointer-func syntax-context struct-name field-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> ":" <> "->pointer")
         syntax-context struct-name field-name))

   (define (unchecked-struct-field->pointer-func syntax-context struct-name field-name)
      (build-type-related-symbol
         (cute string-append "unchecked-ff:" <> ":" <> "->pointer")
         syntax-context struct-name field-name))

   (define (struct->alist-func syntax-context struct-name)
      (build-type-related-symbol
         (cute string-append "ff:" <> "->alist")
         syntax-context struct-name))

   (define (unchecked-struct->alist-func syntax-context struct-name)
      (build-type-related-symbol
         (cute string-append "unchecked-ff:" <> "->alist")
         syntax-context struct-name))
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
      (syntax-case x (struct sizeof field)

         ((_ (sizeof type-name size))
          #`(begin
               (maybe-export
                  ;; Example: sizeof-ff:SplineChar
                  #,(type-sizeof-var x #'type-name))
               (define #,(type-sizeof-var x #'type-name) size)))

         ((_ (struct type-name size))
          (let ((tag (type-tag x #'type-name)))
             #`(begin
                  (maybe-export
                     ;; Example: ff:SplineChar?
                     #,(struct?-func x #'type-name)

                     ;; Example: check-ff:SplineChar
                     #,(check-struct-func x #'type-name)

                     ;; Example: pointer->ff:SplineChar
                     #,(pointer->struct-func x #'type-name)

                     ;; Example: ff:SplineChar->pointer
                     #,(struct->pointer-func x #'type-name)

                     ;; Example: unchecked-ff:SplineChar->pointer
                     #,(unchecked-struct->pointer-func x #'type-name)

                     ;; Example: malloc-ff:SplineChar
                     #,(malloc-struct-func x #'type-name)

                     ;; Example: free-ff:SplineChar
                     #,(free-struct-func x #'type-name)

                     ;; Example: unchecked-free-ff:SplineChar
                     #,(unchecked-free-struct-func x #'type-name)

                     ;; Example: gc-malloc-ff:SplineChar
                     #,(gc-malloc-struct-func x #'type-name)

                     ;; Example: gc-free-ff:SplineChar
                     #,(gc-free-struct-func x #'type-name)

                     ;; Example: unchecked-gc-free-ff:SplineChar
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

                  (define #,(pointer->struct-func x #'type-name)
                     (lambda (ptr)
                        (cons '#,tag (pointer->bytevector ptr size))))

                  (define #,(struct->pointer-func x #'type-name)
                     (lambda (obj)
                        (#,(check-struct-func x #'type-name)
                         '#,(struct->pointer-func x #'type-name)
                         obj)
                        (bytevector->pointer (cdr obj))))

                  (define #,(unchecked-struct->pointer-func x #'type-name)
                     (lambda (obj)
                        (bytevector->pointer (cdr obj))))

                  (define #,(malloc-struct-func x #'type-name)
                     (lambda ()
                        (cons '#,tag
                           (pointer->bytevector (scm_calloc size) size))))

                  (define #,(free-struct-func x #'type-name)
                     (lambda (obj)
                        (free (#,(struct->pointer-func x #'type-name) obj))))

                  (define #,(unchecked-free-struct-func x #'type-name)
                     (lambda (obj)
                        (free (#,(unchecked-struct->pointer-func x #'type-name) obj))))

                  (define #,(gc-malloc-struct-func x #'type-name)
                     (lambda ()
                        (cons '#,tag
                           (pointer->bytevector (GC_malloc size) size))))

                  (define #,(gc-free-struct-func x #'type-name)
                     (lambda (obj)
                        (GC_free (#,(struct->pointer-func x #'type-name) obj))))

                  (define #,(unchecked-gc-free-struct-func x #'type-name)
                     (lambda (obj)
                        (GC_free (#,(unchecked-struct->pointer-func x #'type-name) obj))))

                  )))

         ((_ (field field-type struct-name field-name offset size))
          #`(begin
               (maybe-export
                  ;; Example: unchecked-ff:SplineChar:name-ref
                  #,(unchecked-struct-field-ref-func x #'struct-name #'field-name)

                  ;; Example: ff:SplineChar:name-ref
                  #,(struct-field-ref-func x #'struct-name #'field-name)

                  ;; Example: unchecked-ff:SplineChar:name-set!
                  #,(unchecked-struct-field-set!-func x #'struct-name #'field-name)

                  ;; Example: ff:SplineChar:name-set!
                  #,(struct-field-set!-func x #'struct-name #'field-name)

                  ;; Example: unchecked-ff:SplineChar:name->pointer
                  #,(unchecked-struct-field->pointer-func x #'struct-name #'field-name)

                  ;; Example: ff:SplineChar:name->pointer
                  #,(struct-field->pointer-func x #'struct-name #'field-name)
                  )
               
               (define #,(unchecked-struct-field-ref-func x #'struct-name #'field-name)
                  (lambda (obj)
                     ((ff:struct-field-ref field-type offset size) obj)))

               (define #,(struct-field-ref-func x #'struct-name #'field-name)
                  (lambda (obj)
                     (#,(check-struct-func x #'struct-name)
                      #,(struct-field-ref-func x #'struct-name #'field-name)
                      obj)
                     ((ff:struct-field-ref field-type offset size) obj)))

               (define #,(unchecked-struct-field-set!-func x #'struct-name #'field-name)
                  (lambda (obj v)
                     ((ff:struct-field-set! field-type offset size) obj v)))

               (define #,(struct-field-set!-func x #'struct-name #'field-name)
                  (lambda (obj v)
                     (#,(check-struct-func x #'struct-name)
                      #,(struct-field-set!-func x #'struct-name #'field-name)
                      obj)
                     ((ff:struct-field-set! field-type offset size) obj v)))

               (define #,(unchecked-struct-field->pointer-func x #'struct-name #'field-name)
                  (lambda (obj)
                     ((ff:struct-field->pointer offset) obj)))

               (define #,(struct-field->pointer-func x #'struct-name #'field-name)
                  (lambda (obj)
                     (#,(check-struct-func x #'struct-name)
                      #,(struct-field->pointer-func x #'struct-name #'field-name)
                      obj)
                     ((ff:struct-field->pointer offset) obj)))
               ))

         ((_ (struct-field struct-name field-name offset size))
          #`(begin
               (maybe-export
                  ;; Example: unchecked-ff:SplineChar:name->pointer
                  #,(unchecked-struct-field->pointer-func x #'struct-name #'field-name)

                  ;; Example: ff:SplineChar:name->pointer
                  #,(struct-field->pointer-func x #'struct-name #'field-name)
                  )
               
               (define #,(unchecked-struct-field->pointer-func x #'struct-name #'field-name)
                  (lambda (obj)
                     ((ff:struct-field->pointer offset) obj)))

               (define #,(struct-field->pointer-func x #'struct-name #'field-name)
                  (lambda (obj)
                     (#,(check-struct-func x #'struct-name)
                      #,(struct-field->pointer-func x #'struct-name #'field-name)
                      obj)
                     ((ff:struct-field->pointer offset) obj)))
               ))

         ((_ (struct-> struct-name (field-name kind field-type offset size) ...))
          #`(begin
               (maybe-export
                  ;; Example: unchecked-ff:SplineChar->alist
                  #,(unchecked-struct->alist-func x #'struct-name)

                  ;; Example: ff:SplineChar->alist
                  #,(struct->alist-func x #'struct-name)
                  )

               (define #,(unchecked-struct->alist-func x #'struct-name)
                  (lambda (obj)
                     (list
                        ((unchecked-ff:struct-field->alist-entry
                            field-name kind field-type offset size) obj)
                        ...)))

               (define #,(struct->alist-func x #'struct-name)
                  (lambda (obj)
                     (#,(check-struct-func x #'struct-name)
                      #,(struct->alist-func x #'struct-name)
                      obj)
                     (#,(unchecked-struct->alist-func x #'struct-name) obj)))
               ))
         )))

;;-------------------------------------------------------------------------

(define-syntax ff:struct-field-ref
   (syntax-rules (int uint bool *)
      ((_ int offset 1) (lambda (obj) (bytevector-s8-ref (cdr obj) offset)))
      ((_ int offset 2) (lambda (obj) (bytevector-s16-native-ref (cdr obj) offset)))
      ((_ int offset 4) (lambda (obj) (bytevector-s32-native-ref (cdr obj) offset)))
      ((_ int offset 8) (lambda (obj) (bytevector-s64-native-ref (cdr obj) offset)))
      ((_ uint offset 1) (lambda (obj) (bytevector-u8-ref (cdr obj) offset)))
      ((_ uint offset 2) (lambda (obj) (bytevector-u16-native-ref (cdr obj) offset)))
      ((_ uint offset 4) (lambda (obj) (bytevector-u32-native-ref (cdr obj) offset)))
      ((_ uint offset 8) (lambda (obj) (bytevector-u64-native-ref (cdr obj) offset)))
      ((_ bool offset 1) (lambda (obj) (not (zero? (bytevector-u8-ref (cdr obj) offset)))))
      ((_ bool offset 2) (lambda (obj) (not (zero? (bytevector-u16-native-ref (cdr obj) offset)))))
      ((_ bool offset 4) (lambda (obj) (not (zero? (bytevector-u32-native-ref (cdr obj) offset)))))
      ((_ bool offset 8) (lambda (obj) (not (zero? (bytevector-u64-native-ref (cdr obj) offset)))))
      ((_ * offset 1) (lambda (obj) (make-pointer (bytevector-u8-ref (cdr obj) offset))))
      ((_ * offset 2) (lambda (obj) (make-pointer (bytevector-u16-native-ref (cdr obj) offset))))
      ((_ * offset 4) (lambda (obj) (make-pointer (bytevector-u32-native-ref (cdr obj) offset))))
      ((_ * offset 8) (lambda (obj) (make-pointer (bytevector-u64-native-ref (cdr obj) offset))))
      ))

(define-syntax ff:struct-field-set!
   (syntax-rules (int uint bool *)
      ((_ int offset 1) (lambda (obj v) (bytevector-s8-set! (cdr obj) offset v)))
      ((_ int offset 2) (lambda (obj v) (bytevector-s16-native-set! (cdr obj) offset v)))
      ((_ int offset 4) (lambda (obj v) (bytevector-s32-native-set! (cdr obj) offset v)))
      ((_ int offset 8) (lambda (obj v) (bytevector-s64-native-set! (cdr obj) offset v)))
      ((_ uint offset 1) (lambda (obj v) (bytevector-u8-set! (cdr obj) offset v)))
      ((_ uint offset 2) (lambda (obj v) (bytevector-u16-native-set! (cdr obj) offset v)))
      ((_ uint offset 4) (lambda (obj v) (bytevector-u32-native-set! (cdr obj) offset v)))
      ((_ uint offset 8) (lambda (obj v) (bytevector-u64-native-set! (cdr obj) offset v)))
      ((_ bool offset 1) (lambda (obj v) (bytevector-u8-set! (cdr obj) offset (if v 1 0))))
      ((_ bool offset 2) (lambda (obj v) (bytevector-u16-native-set! (cdr obj) offset (if v 1 0))))
      ((_ bool offset 4) (lambda (obj v) (bytevector-u32-native-set! (cdr obj) offset (if v 1 0))))
      ((_ bool offset 8) (lambda (obj v) (bytevector-u64-native-set! (cdr obj) offset (if v 1 0))))
      ((_ * offset 1) (lambda (obj v) (bytevector-u8-set! (cdr obj) offset (pointer-address v))))
      ((_ * offset 2) (lambda (obj v) (bytevector-u16-native-set! (cdr obj) offset (pointer-address v))))
      ((_ * offset 4) (lambda (obj v) (bytevector-u32-native-set! (cdr obj) offset (pointer-address v))))
      ((_ * offset 8) (lambda (obj v) (bytevector-u64-native-set! (cdr obj) offset (pointer-address v))))
      ))

(define-syntax ff:struct-field->pointer
   (syntax-rules ()
      ((_ offset) (lambda (obj) (bytevector->pointer (cdr obj) offset)))))

(define-syntax unchecked-ff:struct-field->alist-entry
   (lambda (x)
      (syntax-case x (field struct-field)
         ((_ field-name field field-type offset size)
          #`(lambda (obj)
               (cons
                  (quote #,(datum->syntax x
                              (string->symbol (syntax->datum #'field-name))))
                  ((ff:struct-field-ref field-type offset size) obj))))
         ((_ field-name struct-field _ offset _)
           #`(lambda (obj)
                (cons
                   (quote #,(datum->syntax x
                               (string->symbol (syntax->datum #'field-name))))
                   ((ff:struct-field->pointer offset) obj)))))))

;;-------------------------------------------------------------------------

(define read-define-ff:interface
   (case-lambda
      (() (read-define-ff:interface (current-input-port)))
      ((port)
       (do ((instruction (read port) (read port)))
           ((eof-object? instruction))
           (eval (list 'define-ff:interface instruction)
              (interaction-environment))))))

;;-------------------------------------------------------------------------
