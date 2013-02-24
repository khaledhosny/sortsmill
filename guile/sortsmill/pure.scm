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

;;;
;;; To use the Pure API in sortsmill-editor, put stuff like
;;;
;;;     (use-modules (sortsmill pure)
;;;                  (sortsmill usermenu pure))
;;;     (use-pure-api)
;;;
;;; in your local-init.scm and/or user-init.scm.
;;;

((@ (sortsmill strings hash-guillemet) enable-hash-guillemet-strings))

(library (sortsmill pure)

  (export use-pure-api

          pure-op-infix
          pure-op-infixl
          pure-op-infixr
          pure-op-prefix
          pure-op-postfix
          pure-op-nullary
          pure-prec-max
          pure-nprec-max
          pure-fixity-max

          string->symbol-pure-expr   ; Creates the symbol if necessary.
          string->symbol-pure-expr-or-f  ; Returns #f if the symbol
                                        ; does not exist.
          symbol-pure-expr->string
          pure-closing-symbol
          pure-fixity
          pure-eval-symbol

          pure-expr?
          pointer->pure-expr
          pure-expr->pointer

          number->pure-expr
          pure-expr->number
          pure-expr-is-number?

          integer->pure-expr
          small-integer->pure-expr
          big-integer->pure-expr
          pure-expr->integer
          pure-expr->small-integer
          pure-expr->big-integer
          pure-expr->small-integer-or-f
          pure-expr->big-integer-or-f
          pure-expr-is-integer?
          pure-expr-is-small-integer?
          pure-expr-is-big-integer?

          rational->pure-expr
          pure-expr->rational
          pure-expr->rational-or-f
          pure-expr-is-rational?

          inexact->pure-expr
          flonum->pure-expr
          pure-expr->inexact
          pure-expr->flonum
          pure-expr->inexact-or-f
          pure-expr->flonum-or-f
          pure-expr-is-inexact?
          pure-expr-is-flonum?

          complex->pure-expr
          pure-expr->complex
          pure-expr->complex-or-f
          pure-expr-is-complex?

          pointer->pointer-pure-expr
          pointer-pure-expr->pointer
          pointer-pure-expr->pointer-or-f
          pure-expr-is-pointer?

          string->pure-expr
          pure-expr->string
          pure-expr->string-or-f
          pure-expr-is-string?

          pure-lasterr
          pure-clear-lasterr
          pure-lasterr-pos
          pure-val
          pure-eval
          pure-evalcmd
          pure-interp-compile

          pure-apply

          pure-str                       ; Returns a Guile string.
          pure-str-pure-expr             ; Returns a Pure string.

          pure-pointer-tag
          pure-pointer-type
          pure-pointer-cast

          pure-interp?
          pointer->pure-interp
          pure-interp->pointer
          pure-create-interp
          pure-delete-interp
          pure-switch-interp
          pure-current-interp

          pure-finalize

          ;; Reëxported from (sortsmill strings ...).
          enable-hash-guillemet-strings
          disable-hash-guillemet-strings
          lines-begin-with)

  (import (sortsmill i18n)
          (sortsmill dynlink)
          (sortsmill editor finalization)
          (only (sortsmill strings hash-guillemet)
                enable-hash-guillemet-strings
                disable-hash-guillemet-strings)
          (only (sortsmill strings) lines-begin-with)
          (sortsmill argv)
          (rnrs)
          (except (guile) error)
          (except (srfi :1) map)
          (ice-9 match)
          (ice-9 format)
          (system foreign))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_pure"))

  (sortsmill-dynlink-declarations "#include <pure/runtime.h>")

  (define pure-op-infix 0)
  (define pure-op-infixl 1)
  (define pure-op-infixr 2)
  (define pure-op-prefix 3)
  (define pure-op-postfix 4)
  (define pure-op-nullary 5)
  (define pure-prec-max 16777216)
  (define pure-nprec-max 167772160)
  (define-syntax pure-fixity-max
    (identifier-syntax pure-nprec-max))

  (define string->symbol-pure-expr
    (let ((proc (pointer->procedure
                 int32
                 (sortsmill-dynlink-func "pure_sym")
                 `(*))))
      (lambda (s)
        (pure-quoted-symbol (proc (string->pointer s "UTF-8"))))))

  (define string->symbol-pure-expr-or-f
    (let ((proc (pointer->procedure
                 int32
                 (sortsmill-dynlink-func "pure_getsym")
                 `(*))))
      (lambda (s)
        (let ((sym (proc (string->pointer s "UTF-8"))))
          (if (zero? sym) #f (pure-quoted-symbol sym))))))

  (define symbol-pure-expr->small-integer
    (lambda (caller x)
      (cond
       ((symbol-pure-expr->small-integer-or-f x))
       (else
        (error caller
               (_ "the Pure expression is not a symbol")
               x)))))
  
  (define symbol-pure-expr->string
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_sym_pname")
                 `(,int32))))
      (lambda (x)
        (let ((sym (symbol-pure-expr->small-integer
                    'symbol-pure-expr->string x)))
          (pointer->string (proc sym) -1 "UTF-8")))))
  
  (define pure-closing-symbol
    (let ((proc (pointer->procedure
                 int32
                 (sortsmill-dynlink-func "pure_sym_other")
                 `(,int32))))
      (lambda (x)
        (let* ((sym (symbol-pure-expr->small-integer
                     'pure-closing-symbol x))
               (other (proc sym)))
          (if (zero? other) #f (pure-quoted-symbol other))))))

  (define pure-fixity
    (let ((proc (pointer->procedure
                 int32
                 (sortsmill-dynlink-func "pure_sym_nprec")
                 `(,int32))))
      (lambda (x)
        (let ((sym (symbol-pure-expr->small-integer 'pure-fixity x)))
          (proc sym)))))

  (define pure-eval-symbol
    (lambda (string)
      (let ((result-pair (private:eval-pure-symbol string)))
        (match result-pair
          (#f (error 'eval-pure-symbol
                     (_ "the Pure symbol does not exist")
                     string))
          ([result . pure-exception]
           (if pure-exception
               ;; FIXME: Consider raising a ‘&pure-exception’ R6RS
               ;; condition instead.
               (throw 'pure-exception pure-exception)
               result))))))

  (define-wrapped-pointer-type pure-expr
    pure-expr?
    private:pointer->pure-expr pure-expr->pointer
    (lambda (expr port)
      (format port "#<pure-expr ~s 0x~x>"
              (pure-str expr)
              (pointer-address (pure-expr->pointer expr)))))

  (define pointer->pure-expr
    ;; @var{pointer->pure-expr} sets up reference counting for the Pure
    ;; garbage collector, then wraps the pointer as a @var{pure-expr}
    ;; object.
    (lambda (p)
      (private:pointer->pure-expr (scm-pure-new p))))

  (define procedure:pure-expr->pointer
    ;; Wrap the syntax transformer @var{pure-expr->pointer} in a
    ;; procedure, so C code can use it.
    (lambda (x) (pure-expr->pointer x)))

  (define number->pure-expr
    (lambda (n)
      (cond
       ((inexact? n) (if (real? n)
                         (inexact->pure-expr n)
                         (complex->pure-expr n)))
       ((integer? n) (integer->pure-expr n))
       ((rational? n) (rational->pure-expr n))
       (else (error 'number->pure-expr
                    (_ "not a number") n)))))

  (define pure-expr->number
    (lambda (x)
      (cond
       ((pure-expr->small-integer-or-f x))
       ((pure-expr->inexact-or-f x))
       ((pure-expr->big-integer-or-f x))
       ((pure-expr->rational-or-f x))
       ((pure-expr->complex-or-f x))
       (else
        (error 'pure-expr->number
               (_ "the Pure expression is not a number")
               x)))))

  (define pure-expr-is-number?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr-is-integer? x) #t)
       ((pure-expr-is-inexact? x) #t)
       ((pure-expr-is-rational? x) #t)
       ((pure-expr-is-complex? x) #t)
       (else #f))))

  (define integer->pure-expr
    (lambda (n)
      (if (and (<= -2147483648 n) (<= n 2147483647))
          (small-integer->pure-expr n)
          (big-integer->pure-expr n))))

  (define small-integer->pure-expr
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_int")
                 `(,int32))))
      (compose pointer->pure-expr proc)))

  (define pure-expr->integer
    (lambda (x)
      (cond
       ((pure-expr->small-integer-or-f x))
       ((pure-expr->big-integer-or-f x))
       (else
        (error
         'pure-expr->integer
         (_ "the Pure expression is not an integer")
         x)))))

  (define pure-expr->small-integer
    (lambda (x)
      (cond
       ((pure-expr->small-integer-or-f x))
       (else
        (error
         'pure-expr->small-integer
         (_ "the Pure expression is not an integer in the range -2147483648 to 2147483647")
         x)))))

  (define pure-expr->big-integer
    (lambda (x)
      (cond
       ((pure-expr->big-integer-or-f x))
       (else
        (error
         'pure-expr->big-integer
         (_ "the Pure expression is not an integer outside the range -2147483648 to 2147483647")
         x)))))

  (define pure-expr-is-integer?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr-is-small-integer? x) #t)
       ((pure-expr-is-big-integer? x) #t)
       (else #f))))

  (define pure-expr-is-small-integer?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr->small-integer-or-f x) #t)
       (else #f))))

  (define pure-expr-is-big-integer?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr->big-integer-or-f x) #t)
       (else #f))))

  (define pure-expr->rational
    (lambda (x)
      (cond
       ((pure-expr->rational-or-f x))
       (else
        (error
         'pure-expr->rational
         (_ "the Pure expression is not a rational number")
         x)))))

  (define pure-expr-is-rational?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr->rational-or-f x) #t)
       (else #f))))

  (define inexact->pure-expr
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_double")
                 `(,double))))
      (compose pointer->pure-expr proc)))

  (define-syntax flonum->pure-expr
    (identifier-syntax inexact->pure-expr))

  (define pure-expr->inexact
    (lambda (x)
      (cond
       ((pure-expr->inexact-or-f x))
       (else
        (error
         'pure-expr->inexact
         (_ "the Pure expression is not a floating point number")
         x)))))

  (define-syntax pure-expr->flonum
    (identifier-syntax pure-expr->inexact))

  (define-syntax pure-expr->flonum-or-f
    (identifier-syntax pure-expr->inexact-or-f))

  (define pure-expr-is-inexact?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr->inexact-or-f x) #t)
       (else #f))))

  (define-syntax pure-expr-is-flonum?
    (identifier-syntax pure-expr-is-inexact?))

  (define pure-expr->complex
    (lambda (x)
      (cond
       ((pure-expr->complex-or-f x))
       (else
        (error 'pure-expr->complex
               (_ "the Pure expression is not a complex number")
               x)))))

  (define pure-expr-is-complex?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr->complex-or-f x) #t)
       (else #f))))

  (define pointer->pointer-pure-expr
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_pointer")
                 `(*))))
      (compose pointer->pure-expr proc)))

  (define pointer-pure-expr->pointer
    (lambda (x)
      (cond
       ((pointer-pure-expr->pointer-or-f x))
       (else
        (error 'pure-expr->complex
               (_ "the Pure expression is not a pointer")
               x)))))

  (define pure-expr-is-pointer?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pointer-pure-expr->pointer-or-f x) #t)
       (else #f))))

  (define string->pure-expr
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_string_dup")
                 `(*))))
      (lambda (s)
        (pointer->pure-expr (proc (string->pointer s "UTF-8"))))))

  (define pure-expr->string
    (lambda (x)
      (cond
       ((pure-expr->string-or-f x))
       (else
        (error 'pure-expr->complex
               (_ "the Pure expression is not a string")
               x)))))

  (define pure-expr-is-string?
    (lambda (x)
      (cond
       ((not (pure-expr? x)) #f)
       ((pure-expr->string-or-f x) #t)
       (else #f))))

  (define pure-lasterr
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "lasterr") `())))
      (lambda () (pointer->string (proc) -1 "UTF-8"))))

  (define pure-clear-lasterr
    (pointer->procedure
     void
     (sortsmill-dynlink-func "clear_lasterr")
     `()))

  (define pure-lasterr-pos
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "lasterrpos")
                 `())))
      (lambda () (pointer->pure-expr (proc)))))

  (define pure-val
    ;; Converts Pure syntax from string to pure-expr. Returns #f and
    ;; sets (pure-lasterr) if the string is not legal.
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_val")
                 `(*))))
      (lambda (s)
        (let ((result (proc (string->pointer s "UTF-8"))))
          (if (null-pointer? result)
              #f (pointer->pure-expr result))))))

  (define pure-eval
    (let ((eval-proc (pointer->procedure
                      '*
                      (sortsmill-dynlink-func "pure_eval")
                      `(*))))
      (lambda (x)
        (let ((result-pair
               (cond
                ([string? x]             ; Evaluate a string.
                 (let ((value (eval-proc (string->pointer x "UTF-8"))))
                   (if (null-pointer? value) #f
                       (cons (pointer->pure-expr value) #f))))
                ([pure-expr? x]         ; Evaluate a quoted expression.
                 (private:pure-eval x))
                (else (assertion-violation
                       'pure-eval "expected a string or pure-expr"
                       x)))))
          (match result-pair
            (#f #f)
            ([result . pure-exception]
             (if pure-exception
                 (throw 'pure-exception pure-exception)
                 result)))))))

  (define pure-evalcmd
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_evalcmd")
                 `(*))))
      (lambda (s)
        (let ((output (proc (string->pointer s "UTF-8"))))
          (if (null-pointer? output) #f
              (pointer->string output -1 "UTF-8"))))))

  (define pure-pointer-tag
    (let ((proc (pointer->procedure
                 int
                 (sortsmill-dynlink-func "pure_pointer_tag")
                 `(*))))
      (lambda (s)
        (cond
         ((string? s) (proc (string->pointer s)))
         ((pure-expr-is-string? s) (proc (string->pointer
                                          (pure-expr->string s))))
         (else (assertion-violation
                'pure-pointer-tag
                (_ "expected a string or a string pure-expr")
                s))))))

  (define pure-pointer-cast
    (let* ((proc (pointer->procedure
                  '*
                  (sortsmill-dynlink-func "pure_pointer_cast")
                  `(,int *)))
           (cast (lambda (tag x)
                   (if (pure-pointer-type tag)
                       [pointer->pure-expr
                        (proc tag (pure-expr->pointer x))]
                       [assertion-violation
                        'pure-pointer-cast
                        (_ "expected a valid Pure pointer tag")
                        tag]))))
      (lambda (tag x)
        (unless (pure-expr-is-pointer? x)
          (assertion-violation
           'pure-pointer-cast (_ "expected a pointer pure-expr") x))
        (if (or (string? tag) (pure-expr-is-string? tag))
            (cast (pure-pointer-tag tag) x)
            (cast tag x)))))

  (define pure-interp-compile
    (let ((proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "pure_interp_compile")
                 `(* ,int32))))
      (case-lambda
        [() (pure-interp-compile (pure-current-interp) #f)]
        [(interp-or-function-symbol)
         (cond
          [(pure-interp? interp-or-function-symbol)
           ;; Compile @emph{everything}.
           (pure-interp-compile interp-or-function-symbol #f)]
          [else
           (pure-interp-compile (pure-current-interp)
                                interp-or-function-symbol)])]
        [(interp function-symbol)
         (if function-symbol
             (proc (pure-interp->pointer interp)
                   (symbol-pure-expr->small-integer
                    'pure-interp-compile function-symbol))
             (proc (pure-interp->pointer interp) 0))])))

  (define pure-apply
    ;; Application of a curried Pure function, as if it were an
    ;; uncurried (that is, multiple-argument) Scheme function.
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_app")
                 `(* *))))
      (case-lambda
        [(f arg)  ; Treat this as a special case for (perhaps) speed.
         (pointer->pure-expr (proc (pure-expr->pointer f)
                                   (pure-expr->pointer arg)))]
        [(f arg . rest)
         (let ((apply-pure-func (lambda (g x) (proc g x))))
           ;; Repeatedly derive and apply new Pure functions, until we
           ;; reach the result.
           (pointer->pure-expr
            (fold-left apply-pure-func (pure-expr->pointer f)
                       (map pure-expr->pointer (cons arg rest)))))])))

  (define-wrapped-pointer-type pure-interp
    pure-interp? pointer->pure-interp pure-interp->pointer
    (lambda (interp port)
      (format port "#<pure-interp 0x~x>"
              (pointer-address (pure-interp->pointer interp)))))

  (define pure-create-interp
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_create_interp")
                 `(,int *))))
      (lambda (args)
        (let-values ([(argv argc) (string-list->argv-and-argc args)])
          (pointer->pure-interp
           (proc argc (bytevector->pointer argv)))))))

  (define pure-delete-interp
    (let ((proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "pure_delete_interp")
                 `(*))))
      (compose proc pure-interp->pointer)))

  (define pure-switch-interp
    (let ((proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "pure_switch_interp")
                 `(*))))
      (compose proc pure-interp->pointer)))

  (define pure-current-interp
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_current_interp")
                 `())))
      (compose pointer->pure-interp proc)))

  (define pure-quoted-symbol
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_quoted_symbol")
                 `(,int32))))
      (compose pointer->pure-expr proc)))

  (define pure-str-pure-expr
    (let ((proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "pure_str")
                 `(*))))
      (lambda (x)
        (let ((result (proc (pure-expr->pointer x))))
          (if (null-pointer? result) #f
              (pointer->pure-expr result))))))

  (define pure-finalize
    (pointer->procedure
     void
     (sortsmill-dynlink-func "pure_finalize")
     `()))

  (define (use-pure-api)
    (pure-create-interp '("sortsmill-editor"))
    (register-finalizer "Pure interpreter" pure-finalize))

  ) ;; end of library.
