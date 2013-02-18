;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Barry Schwartz
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

(library (sortsmill options)

  (export option-remaining

          ;; FIXME: Maybe come up with Schemey synonyms for some of
          ;; these.
          option-arg-none
          option-arg-string
          option-arg-int
          option-arg-callback
          option-arg-filename
          option-arg-string-array
          option-arg-filename-array
          option-arg-double
          option-arg-int64

          option-flag-hidden
          option-flag-in-main
          option-flag-reverse
          option-flag-no-arg
          option-flag-filename
          option-flag-optional-arg
          option-flag-noalias

          option-error-unknown-option
          option-error-bad-value
          option-error-failed
          )

  (import (sortsmill dynlink)
          (sortsmill kwargs)
          (sortsmill pkg-info)
          (sortsmill api-syntax)
          (sortsmill strings)
          (sortsmill argv)
          (sortsmill alloc)
          (sortsmill machine)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 format)
          (ice-9 match))

  (sortsmill-dynlink-declarations "#include <glib.h>")

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_options"))

  ;;-----------------------------------------------------------------------
  ;;
  ;; FIXME: These should go in a reusable glib support module, which
  ;; also should define the domains and codes. Some of these items may
  ;; not be needed, and the remainder may need to be redesigned
  ;; somewhat.

  (define-wrapped-pointer-type gerror
    gerror?
    pointer->gerror gerror->pointer
    [lambda (obj port)
      (format port "#<gerror ~a 0x~x>"
              (gerror->string obj)
              (pointer-address (gerror->pointer obj)))] )

;;;;;  (define gerror-size (sizeof `(,uint32 ,int *)))

  (define gerror-finalizer (sortsmill-dynlink-func "g_error_free"))

  (define (pointer->grabbed-gerror p)
    "Grab control of a GError structure that was allocated by
g_error_new()."
    (pointer->gerror (make-pointer (pointer-address p) gerror-finalizer)))

  ;; What the string this creates looks like may change from time to
  ;; time.
  (define (gerror->string e)
    (assert (or (list? e) (gerror? e)))
    (if (gerror? e)
        (gerror->string (gerror->list e))
        (match e
          [(domain code message)
           (format #f "(~a ~a ~a)" domain code
                   (if (null-pointer? message)
                       %null-pointer
                       (format #f "~s" (pointer->string message))))] )))

  (define/kwargs (make-gerror [domain 0]
                              [code 0]
                              [message #f])
    "Create a gerror object. Do not try to run g_error_free() on this
object!"
    (pointer->gerror
     (make-c-struct `[,uint32 ,int *]
                    `[,domain
                      ,code
                      ,(if message %null-pointer
                           (string->pointer message))] )))

  (define (gerror->list e)
    (match (parse-c-struct (gerror->pointer e) `(,uint32 ,int *))
      [(domain code message)
       (list domain code (if (null-pointer? message) #f
                             (pointer->string message)))] ))

  ;;-----------------------------------------------------------------------

  (define option-remaining "")

  (define-wrapped-pointer-type option-context
    option-context?
    pointer->option-context option-context->pointer
    [lambda (obj port)
      (format port "#<option-context 0x~x>"
              (pointer-address (option-context->pointer obj)))] )

  (define-syntax define-arg-data-union
    (lambda (stx)
      (let ([size (datum->syntax stx (max (sizeof '*)
                                          (sizeof int)
                                          (sizeof int64)
                                          (sizeof double)))]
            [*-size (datum->syntax stx (sizeof '*))]
            [int-size (datum->syntax stx (sizeof int))]
            [int64-size (datum->syntax stx (sizeof int64))]
            [double-size (datum->syntax stx (sizeof double))])
        (syntax-case stx ()
          [(_)
           #`(define-private-api
               (struct arg-data-union #,size)
               (sizeof arg-data-union #,size)
               (field bool  arg-data-union bool 0 #,int-size)
               (field int   arg-data-union int 0 #,int-size)
               (field int   arg-data-union int64 0 #,int64-size)
               (field float arg-data-union float 0 #,double-size)
               (field *     arg-data-union string 0 #,*-size)
               (field *     arg-data-union filename 0 #,*-size)
               (field *     arg-data-union string-array 0 #,*-size)
               (field *     arg-data-union filename-array 0 #,*-size)
               (field *     arg-data-union callback 0 #,*-size))] ))))

  (define-arg-data-union)

  (define-record-type option-arg-value-record
    [fields (immutable value option-arg-value)
            (immutable storage)]
    [protocol
     (lambda (new)
       (lambda (value-fetcher)
         (new value-fetcher (gc-malloc-arg-data-union))))] )

  (define-record-type option-arg-callback-record
    [parent option-arg-value-record]
    [sealed #t]
    [opaque #t]
    [protocol
     (lambda (value-fetcher->new)
       (lambda (callback-procedure!)
         (let ([new-record (value-fetcher->new
                            (lambda args
                              (assertion-violation
                               'option-arg-callback-record
                               "options callback objects do not return values"
                               value-fetcher->new callback-procedure!)))])
           (arg-data-union:callback-set!
            (option-arg-value-record-storage new-record)
            (procedure->GOptionArgFunc-pointer callback-procedure!))
           new-record)))] )

  (define-syntax define-option-arg-<type>-value
    (syntax-rules ()
      [(_ name ref)
       (define-record-type name
         [parent option-arg-value-record]
         [sealed #t]
         [opaque #t]
         [protocol
          (let ([value-fetcher
                 (compose ref option-arg-value-record-storage)])
            (lambda (value-fetcher->new)
              (value-fetcher->new value-fetcher)))] )] ))

  (define-option-arg-<type>-value option-arg-bool-value
    arg-data-union:bool-ref)

  (define-option-arg-<type>-value option-arg-int-value
    arg-data-union:int-ref)

  (define-option-arg-<type>-value option-arg-int64-value
    arg-data-union:int64-ref)

  (define-option-arg-<type>-value option-arg-float-value
    arg-data-union:float-ref)

  (define-option-arg-<type>-value option-arg-string-value
    (compose pointer->grabbed-string arg-data-union:string-ref))

  (define-option-arg-<type>-value option-arg-filename-value
    (compose pointer->grabbed-string arg-data-union:filename-ref))

  (define-option-arg-<type>-value option-arg-string-array-value
    (compose pointer->grabbed-string-list arg-data-union:string-ref))

  (define-option-arg-<type>-value option-arg-filename-array-value
    (compose pointer->grabbed-string-list arg-data-union:filename-ref))

  (define/kwargs (option-arg-data-object arg [callback #f])
    (assert (eq? (= arg option-arg-callback) (not (not callback))))
    (cond [(= arg option-arg-none)     (make-option-arg-bool-value)]
          [(= arg option-arg-int)      (make-option-arg-int-value)]
          [(= arg option-arg-int64)    (make-option-arg-int64-value)]
          [(= arg option-arg-double)   (make-option-arg-float-value)]
          [(= arg option-arg-callback) (make-option-arg-callback-record callback)]
          [(= arg option-arg-string)   (make-option-arg-string-value)]
          [(= arg option-arg-filename) (make-option-arg-filename-value)]
          [(= arg option-arg-string-array)   (make-option-arg-string-array-value)]
          [(= arg option-arg-filename-array) (make-option-arg-filename-array-value)]
          [(assertion-violation 'option-arg-data-object "illegal argument" arg)] ))

  (define/kwargs (option-entry long-name
                               [short-name #\nul]
                               [flags 0]
                               [arg #f]
                               [callback #f]
                               [description #f]
                               [arg-description #f])
    (assert (or arg callback))
    (assert (if (and arg callback) (= arg option-arg-callback) #t))
    (assert (if callback (procedure? callback) #t))
    (let ([arg-data (option-arg-data-object arg callback)])
      (assert (string? long-name))
      (assert (char? short-name))
      (assert (integer? flags))
      (assert (integer? arg))
      (assert (when description (string? description)))
      (assert (when arg-description (string? arg-description)))
      (assert (<= 0 flags))
      (assert (<= flags (+ option-flag-hidden
                           option-flag-in-main
                           option-flag-reverse
                           option-flag-no-arg
                           option-flag-filename
                           option-flag-optional-arg
                           option-flag-noalias)))
      (assert (member arg (list
                           option-arg-none
                           option-arg-string
                           option-arg-int
                           option-arg-callback
                           option-arg-filename
                           option-arg-string-array
                           option-arg-filename-array
                           option-arg-double
                           option-arg-int64)))
      (list long-name short-name flags arg arg-data description arg-description)))

  (define (option-entries entries)
    (map (cut apply option-entry <>) entries))

  (define (list-with-keywords->GOptionEntry-array lst)
    (list->GOptionEntry-array
     (map (cut apply option-entry-fields->c-data <>)
          (option-entries lst))))

  (define (option-entry-fields->c-data long-name short-name flags arg arg-data
                                       description arg-description)
    (list long-name short-name flags arg
          (arg-data-union->pointer (option-arg-value-record-storage arg-data))
          description arg-description))

  (define option-context-new
    (let ([proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "g_option_context_new")
                 '(*))])
      (compose pointer->option-context proc string->pointer)))

  (define option-context-free
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_free")
                 '(*))])
      (compose proc option-context->pointer)))

  (define option-context-add-main-entries
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_add_main_entries")
                 '(* * *))])
      (lambda* (context entries #:optional
                        [translation-domain pkg-info:textdomain])
        (proc (option-context->pointer context)
              (bytevector->pointer
               (list-with-keywords->GOptionEntry-array entries))
              (string->pointer translation-domain)))))

  (define option-context-set-summary
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_set_summary")
                 '(* *))])
      (lambda (context summary)
        (proc (option-context->pointer context)
              (string->pointer summary)))))
  
  (define option-context-set-description
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_set_description")
                 '(* *))])
      (lambda (context description)
        (proc (option-context->pointer context)
              (string->pointer description)))))

  (define (procedure->GOptionArgFunc-pointer proc!)
    "(proc! option-name:string value:string data:pointer) should
return #t on success; #f, a @code{gerror} object, or an ‘error list’
on failure. It should perform whatever side-effects are needed for
the kind of option it handles."
    (let* ([set-error (pointer->procedure
                       void
                       (sortsmill-dynlink-func "g_set_error_literal")
                       `(* ,uint32 ,int *))]
           [wrapped-proc!
            (lambda (option-name value data error)
              (let ([errval (proc! (pointer->string option-name)
                                   (pointer->string value)
                                   data)])
                (cond [(eq? #t errval) 1]
                      [(eq? #f errval) 0]
                      [(or (gerror? errval) (list? errval))
                       (match (if (gerror? errval)
                                  (gerror->list errval)
                                  errval)
                         [(domain code message)
                          (assert (integer? domain))
                          (assert (integer? code))
                          (assert (string? message))
                          (set-error error domain code
                                     (string->pointer message))
                          0]
                         [_ (assert #f)])] ; FIXME: Better violation checking.
                      [else (assert #f)] ) ; FIXME: Better violation checking.
                ))] )
      (procedure->pointer int wrapped-proc! '(* * * *))))

  (define option-context-parse
    (let ([parse (pointer->procedure
                  int
                  (sortsmill-dynlink-func "g_option_context_parse")
                  '(* * * *))])
      (lambda (context args)
        (let-values ([(argv argc) (string-list->argv-and-argc args)])
          (let ([argc-bv (make-bytevector (sizeof int))]
                [argv-bv (make-bytevector (sizeof '*))]
                [error-bv (make-bytevector (sizeof '*) 0)])
            (bytevector-sint-set! argc-bv 0 argc (native-endianness)
                                  (sizeof int))
            (set-pointer! argv-bv (bytevector->pointer argv))
            (let* ([success?-int (parse (option-context->pointer context)
                                        (bytevector->pointer argc-bv)
                                        (bytevector->pointer argv-bv)
                                        (bytevector->pointer error-bv))]
                   [success? (not (= 0 success?-int))])
              (if success?
                  [values
                   (argv/argc->string-list
                    (get-pointer argv-bv)
                    (bytevector-sint-ref argc-bv 0 (native-endianness)
                                         (sizeof int)))
                   #f]
                  [values
                   #f
                   (gerror->list
                    (pointer->grabbed-gerror (get-pointer error-bv)))] )))))))


  (define ctx  (option-context-new "sksksksskks"))
  (write  ctx)
  (write  (option-context-set-summary ctx "summary"))
  (write  (option-context-set-description ctx "description"))
  (write  (option-context-add-main-entries
           ctx
           (list [list #:long-name "foo"
                       #:arg option-arg-none]
                 [list #:long-name "bar"
                       #:arg option-arg-double]
;;;                                                  [list #:long-name "cb"
;;;                                                        #:arg option-arg-callback
;;;                                                        #:callback (lambda () 1)]
                 )))
  (format #t "\n\n>>>>>>>>>>>>>>>>>>>>>>>>> ~s\n\n\n"  (option-context-parse ctx (list "cmd" "--foox" "a" "b" "c")))
  (write (option-context-free ctx))

  ) ;; end of library.
