;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
;; This file is part of the Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

;;;
;;; To use the Python API in sortsmill-editor, put
;;;
;;;     (use-modules (sortsmill python)
;;;                  (sortsmill usermenu python))
;;;     (use-python-api)
;;;     (load-user_init.py)
;;;
;;; in your user-init.scm.
;;;

((@ (sortsmill strings hash-guillemet) enable-hash-guillemet-strings))

(library (sortsmill python)

  (export python-dll

          use-python-api

          py-initialized?
          py-initialize
          force-py-initialized
          py-finalize
          force-py-finalized

          pyobject?
          pointer->pyobject
          borrowed-pointer->pyobject
          pyobject->pointer

          ;; ‘pyguile’ is an opaque representation of Guile objects in
          ;; Python.
          scm->pyguile
          pyguile->scm
          pyguile?

          ;; scm->pyobject is like scm->pyguile, but passes pyobjects
          ;; unaltered. Other objects are converted to pyguile.
          scm->pyobject

          pair->pytuple
          list->pytuple
          list->pylist
          alist->python-alist
          python-alist->pydict
          alist->pydict
          make-pytuple
          make-pylist
          make-pydict

          pytuple->list
          pylist->list
          pysequence->list
          pyiterable->pyiterator
          pyiterator->!procedure
          pyiterable->!procedure

          pytuple?
          pylist?
          pydict?
          pysequence?
          pyiterable?
          pygenerator?

          pytuple-map
          pylist-map
          
          pyindexed-ref
          pyindexed-set!

          py-none ;; This is a function, not a constant.
          pynone?

          py-false ;; This is a function, not a constant.
          py-true  ;; This is a function, not a constant.
          py-not
          py-not-not ;; Use this to convert a Python value to a pybool.
          boolean->pybool
          pybool->boolean
          pybool?

          integer->pyint
          integer->pylong
          integer->pympz
          pyint->integer
          pylong->integer
          pympz->integer
          pylong->pympz
          pympz->pylong
          pyint?
          pylong?
          pympz?

          rational->pympq
          pympq->rational
          pympq?

          inexact->pyfloat
          pyfloat->inexact
          flonum->pyfloat ;; The same as inexact->pyfloat.
          pyfloat->flonum ;; The same as pyfloat->inexact.
          pyfloat?

          complex->pycomplex
          pycomplex->complex
          pycomplex?

          number->pyobject
          pyobject->number

          pointer->pylong ;; Big integers to represent addresses.
          pylong->pointer

          string->pystring ;; Converts to a unicode object.
          pystring->string ;; Converts from a unicode or UTF-8-encoded bytes object.
          pyunicode?
          pybytes?
          pystring?

          py-repr
          py-str
          py-name
          py-dict

          py-builtins
          py-locals
          py-globals

          procedure->pycallable
          pycallable->procedure
          pycallable?

          pyapply

          pyexec
          pyexec-in
          pyexec-in-main

          pyexec-file-name
          pyexec-file-name-in
          pyexec-file-name-in-main

          pyeval
          pyeval-in
          pyeval-in-main

          ;; Bind an object to a Python identifier.
          pydef-in
          pydef

          ;; Import a module and bind it to a Python identifier.
          pydefimp-in
          pydefimp

          ;; Define a function and bind it to a Python identifier.
          pydefun-in
          pydefun

          python-module
          current-python-module
          in-python-module
          pyimport
          pymodule?

          py-incref ;; Should not be needed very often.
          py-decref ;; Should not be needed very often.

          ;; Reëxported from (sortsmill strings).
          enable-hash-guillemet-strings
          disable-hash-guillemet-strings
          lines-begin-with)

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (sortsmill pkg-info)
          (sortsmill ffcompat)
          (sortsmill editor finalization)
          (only (sortsmill strings)
                enable-hash-guillemet-strings
                disable-hash-guillemet-strings
                lines-begin-with)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 match)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_python"))

  (define flonum->pyfloat inexact->pyfloat)
  (define pyfloat->flonum pyfloat->inexact)

  (define py-initialized?
    (let ([proc (pointer->procedure
                 int (sortsmill-dynlink-func "Py_IsInitialized" "")
                 '())])
      (lambda () (not (fxzero? (proc))))))

  (define py-initialize
    (pointer->procedure
     void (sortsmill-dynlink-func "Py_Initialize" "") `()))

  (define (force-py-initialized)
    (unless (py-initialized?) (py-initialize)))

  (define py-finalize
    (pointer->procedure
     void (sortsmill-dynlink-func "Py_Finalize" "") `()))

  (define (force-py-finalized)
    (when (py-initialized?) (py-finalize)))

  (define py-incref
    (let ([proc (pointer->procedure
                 void (sortsmill-dynlink-func "Py_IncRef" "")
                 '(*))])
      (lambda (obj)
        (assert (pyobject? obj))
        (proc (pyobject->pointer obj)))))

  (define py-decref
    (let ([proc (pointer->procedure
                 void (sortsmill-dynlink-func "Py_DecRef" "")
                 '(*))])
      (lambda (obj)
        (assert (pyobject? obj))
        (proc (pyobject->pointer obj)))))

  (define-wrapped-pointer-type pyobject
    pyobject?
    private:pointer->pyobject private:pyobject->pointer
    (lambda (obj port)
      (let ([repr (pystring->string (py-repr obj))]
            [str  (pystring->string (py-str obj))])
        (if (string=? repr str)
            (format port "#<pyobject repr:~a 0x~x>" repr
                    (pointer-address (pyobject->pointer obj)))
            (format port "#<pyobject repr:~a str:~s 0x~x>" repr str
                    (pointer-address (pyobject->pointer obj)))))))

  (define (pointer->pyobject p)
    (private:pointer->pyobject (grab-pyref p)))
  
  (define (borrowed-pointer->pyobject p)
    (private:pointer->pyobject (grab-borrowed-pyref p)))

  (define (pyobject->pointer obj)
    (unless (pyobject? obj)
      (assertion-violation 'pyobject->pointer (_ "expected a Python object") obj))
    (private:pyobject->pointer obj))

  (define procedure:pyobject?
    ;; Wrap the syntax transformer @code{pyobject?} in a procedure, so
    ;; C code can use it.
    (lambda (obj) (pyobject? obj)))

  (define (pair->pytuple pair)
    (list->pytuple (list (car pair) (cdr pair))))

  (define (make-pytuple . elements)
    (list->pytuple elements))

  (define (make-pylist . elements)
    (list->pylist elements))

  (define (make-pydict . key-value-pairs)
    (alist->pydict key-value-pairs))

  ;; These could ‘easily’ be re-implemented in C if necessary for
  ;; performance.
  (define pytuple-map (compose list->pytuple map pytuple->list))
  (define pylist-map (compose list->pylist map pylist->list))

  ;;--------------------------------------------------------------------------

  (eval-when (compile eval load)
    (define guile-support-dll (dynamic-link "libguile-sortsmill_cython")))

  (eval-when (compile eval load)
    (define (guile-support-func func-name)
      (dynamic-func func-name guile-support-dll)))

  (eval-when (compile eval load)
    (load-extension "libguile-sortsmill_cython"
                    (pkg-info:pyinit-function-name "libguile_sortsmill_cython")))

  (define-syntax define-guile-support-procedure
    (lambda (x)
      (syntax-case x ()
        [(_ return-type c-func arg-types)
         (let ([my-proc
                (datum->syntax
                 x (string->symbol (syntax->datum #'c-func)))])
           #`(eval-when (compile eval load)
               (define #,my-proc
                 (pointer->procedure
                  return-type (guile-support-func c-func) arg-types))))] )))

  (define-syntax define-py-to-py-filter
    (lambda (x)
      (syntax-case x ()
        [(_ filter-name c-func)
         #`(define (filter-name obj)
             (cond
              [(pyobject? obj)
               (let ([result (c-func (pyobject->pointer obj))])
                 (if (null-pointer? result)
                     (py-failure (quote c-func) (list obj))
                     (pointer->pyobject result)))]
              [else
               (assertion-violation (quote filter-name)
                                    (_ "expected a Python object") obj)] ))]
        )))

  (define-guile-support-procedure '* "__c_pyguile_make" '(*))
  (define-guile-support-procedure '* "__c_pyguile_address" '(*))
  (define-guile-support-procedure '* "__pyguile_check" '(*))
  (define-guile-support-procedure '* "__exec_python" '(*))
  (define-guile-support-procedure '* "__exec_python_in_module" '(*))
  (define-guile-support-procedure '* "__exec_python_file_name" '(*))
  (define-guile-support-procedure '* "__exec_python_file_name_in_module" '(*))
  (define-guile-support-procedure '* "__eval_python" '(*))
  (define-guile-support-procedure '* "__eval_python_in_module" '(*))
  (define-guile-support-procedure '* "__c_py_wrap_function" '(*))
  (define-guile-support-procedure '* "__apply_python_callable" '(* * *))
  (define-guile-support-procedure '* "__py_raise_guile_exception" '(*))
  (define-guile-support-procedure '* "__py_exception_description" '(*))
  (define-guile-support-procedure '* "__python_module" '(*))
  (define-guile-support-procedure '* "__python_import" '(*))
  (define-guile-support-procedure '* "__pylong_to_pympz" '(*))
  (define-guile-support-procedure '* "__pympz_to_pylong" '(*))
  (define-guile-support-procedure '* "__py_repr" '(*))
  (define-guile-support-procedure '* "__py_str" '(*))
  (define-guile-support-procedure '* "__py_name" '(*))
  (define-guile-support-procedure '* "__py_dict" '(*))
  (define-guile-support-procedure '* "__pyindexed_ref" '(*))
  (define-guile-support-procedure '* "__pyindexed_set" '(*))
  (define-guile-support-procedure '* "__python_alist_to_pydict" '(*))
  (define-guile-support-procedure '* "__make_pyiterator" '(*))
  (define-guile-support-procedure '* "__pyiterator_next" '(*))

  (define-py-to-py-filter pyguile?-base __pyguile_check)

  (define-py-to-py-filter pyexec-base __exec_python)
  (define-py-to-py-filter pyexec-in-module-base __exec_python_in_module)
  (define-py-to-py-filter pyexec-file-name-base __exec_python_file_name)
  (define-py-to-py-filter pyexec-file-name-in-module-base __exec_python_file_name_in_module)
  (define-py-to-py-filter pyeval-base __eval_python)
  (define-py-to-py-filter pyeval-in-module-base __eval_python_in_module)

  (define-py-to-py-filter py-raise-guile-exception __py_raise_guile_exception)
  (define-py-to-py-filter py-exception-description-base __py_exception_description)

  (define-py-to-py-filter python-module-base __python_module)
  (define-py-to-py-filter python-import-base __python_import)

  (define-py-to-py-filter pylong->pympz __pylong_to_pympz)
  (define-py-to-py-filter pympz->pylong __pympz_to_pylong)

  (define-py-to-py-filter py-repr __py_repr)
  (define-py-to-py-filter py-str __py_str)
  (define-py-to-py-filter py-name __py_name)
  (define-py-to-py-filter py-dict __py_dict)

  (define-py-to-py-filter pyindexed-ref-core __pyindexed_ref)
  (define-py-to-py-filter pyindexed-set!-core __pyindexed_set)
  (define-py-to-py-filter python-alist->pydict __python_alist_to_pydict)
  (define-py-to-py-filter pyiterable->pyiterator __make_pyiterator)

  (define (procedure->pycallable proc)
    (let* ([wrapped-proc
            (lambda (args)
              (pyobject->pointer (apply proc (pytuple->list
                                              (pointer->pyobject args)))))]
           [func (procedure->pointer '* wrapped-proc '(*))])
      (pointer->pyobject (__c_py_wrap_function func))))

  (define (pycallable-catch-handler key . args)
    (let ([py-key (string->pystring (symbol->string key))]
          [py-args (list->pytuple (map scm->pyguile args))])
      (py-raise-guile-exception (make-pytuple py-key py-args))))

  (define (pycallable->procedure obj)
    (assert (pycallable? obj))
    (lambda (. args) (pyapply obj args)))

  (define pyapply
    (case-lambda
      [(func args) (pyapply func args (py-none))]
      [(func args keyword-args)
       (cond
        [(not (pyobject? args)) (pyapply func (list->pytuple args) keyword-args)]
        [(not (pyobject? keyword-args)) (pyapply func args
                                                 (alist->pydict keyword-args))]
        [else
         (unless (pycallable? func)
           (assertion-violation 'pyapply (_ "expected a Python callable") func))
         (let ([retval
                (__apply_python_callable (pyobject->pointer func)
                                         (pyobject->pointer args) 
                                         (pyobject->pointer keyword-args))])
           (assert (not (null-pointer? retval)))
           (pointer->pyobject retval))] )] ))

  (define (scm->pyguile obj)
    (pointer->pyobject (__c_pyguile_make (scm->pointer obj))))

  (define (scm->pyobject obj)
    (cond [(pyobject? obj) obj]
          [else (pointer->pyobject (__c_pyguile_make (scm->pointer obj)))]))

  (define (pyguile->scm obj)
    (cond [(pyguile? obj)
           (pointer->scm (__c_pyguile_address (pyobject->pointer obj)))]
          [else (assertion-violation 'pyguile->scm
                                     (_ "expected a pyguile object")
                                     obj)]))

  (define (pyguile? obj)
    (if (pyobject? obj) (pyguile?-base obj) #f))

  (set-exception-printer!
   'python-exception
   (lambda (port key args default-printer)
     (match args
       [[who exc-info . rest]
        (when who (format port "In procedure ~a:\n" who))
        (format port "~a" (py-exception-description exc-info))]
       [_ (default-printer)] )))

  (define (py-exception-description exc-info)
    (apply string-append
           "ERROR: Python exception:\n"
           (map pystring->string
                (pylist->list (py-exception-description-base exc-info)))))

  (define (pyexec python-code)
    (pyexec-in-module-base
     (make-pytuple (current-python-module)
                   (force-pystring 'pyexec python-code))))

  (define (pyexec-in module python-code)
    (pyexec-in-module-base
     (make-pytuple (force-pymodule 'pyexec-in module)
                   (force-pystring 'pyexec-in python-code))))

  (define (pyexec-in-main python-code)
    (pyexec-base (force-pystring 'pyexec-in-main python-code)))

  (define (pyexec-file-name file-name)
    (pyexec-file-name-in-module-base
     (make-pytuple (current-python-module)
                   (force-pystring 'pyeval-file-name-in file-name))))

  (define (pyexec-file-name-in module file-name)
    (pyexec-file-name-in-module-base
     (make-pytuple (force-pymodule 'pyeval-file-name-in module)
                   (force-pystring 'pyeval-file-name-in file-name))))

  (define (pyexec-file-name-in-main file-name)
    (pyexec-file-name-base
     (force-pystring 'pyexec-file-name-in-main file-name)))

  (define (pyeval python-code)
    (pyeval-in-module-base
     (make-pytuple (current-python-module)
                   (force-pystring 'pyeval python-code))))

  (define (pyeval-in module python-code)
    (pyeval-in-module-base
     (make-pytuple (force-pymodule 'pyeval-in module)
                   (force-pystring 'pyeval-in python-code))))

  (define (pyeval-in-main python-code)
    (pyeval-base (force-pystring 'pyeval-in-main python-code)))

  (define integer->pylong (compose pympz->pylong integer->pympz))
  (define pylong->integer (compose pympz->integer pylong->pympz))

  (define (pyindexed-ref obj i)
    (assert (pyobject? obj))
    (pyindexed-ref-core (list->pytuple (list obj i))))

  (define (pyindexed-set! obj i v)
    (assert (pyobject? obj))
    (pyindexed-set!-core (list->pytuple (list obj i v))))

  (define (string-or-pystring-failure who obj)
    (assertion-violation who (_ "expected a string or Python string") obj))

  (define (python-module module-name)
    (pointer->pyobject
     (__python_module (pyobject->pointer
                       (force-pystring 'python-module module-name)))))

  (define* (pyimport module-name #:key
                     [globals (py-dict (current-python-module))]
                     [locals #f]
                     [from '()]
                     [level 0])
    (pointer->pyobject
     (__python_import (pyobject->pointer
                       (make-pytuple (force-pystring 'pyimport module-name)
                                     globals
                                     (if locals locals globals)
                                     (list->pylist from)
                                     (integer->pyint level))))))

  (define %current-python-module-fluid (make-fluid #f))

  (define (current-python-module)
    (let ([pymod (fluid-ref %current-python-module-fluid)])
      (if pymod pymod
          (begin
            (force-py-initialized)
            (let ([main (python-module "__main__")])
              (fluid-set! %current-python-module-fluid main)
              main)))))

  (define-syntax in-python-module
    (syntax-rules ()
      [(_ module body body* ...)
       (begin
         (force-py-initialized)
         (with-fluids ([%current-python-module-fluid
                        (force-pymodule 'in-python-module module)])
           body body* ...))]))

  (define (alist->python-alist alist)
    (list->pylist (map pair->pytuple alist)))

  (define alist->pydict
    (compose python-alist->pydict alist->python-alist))

  (define pyiterator->!procedure
    (lambda (obj)
      (let ([next! __pyiterator_next]
            [obj-ptr (pyobject->pointer obj)])
        (lambda ()
          (let ([next-value (next! obj-ptr)])
            (cond [(null-pointer? next-value) #f]
                  [else (pointer->pyobject next-value)]))))))

  (define pyiterable->!procedure
    (compose pyiterator->!procedure pyiterable->pyiterator))

  ;;--------------------------------------------------------------------------

  (define (pydef-base caller container name obj)
    (when (and container name)
      (pyindexed-set! (if (pydict? container) container (py-dict container))
                      (force-pystring caller name)
                      obj))
    obj)

  (define (pydefimp-base caller container name module)
    (let ([module^ (begin (pyimport module)
                          (python-module module))])
      (match name
        [#t (pydef-base caller container module module^)]
        [s  (pydef-base caller container s module^)])))

  (define (pydefun-base caller container name func)
    (pydef-base caller container name
                (match func
                  [(? pycallable? callable) callable]
                  [(? procedure? proc) (procedure->pycallable proc)]
                  [else (assertion-violation
                         caller
                         (_ "expected a procedure or pycallable")
                         func)] )))

  (define (pydef name obj)
    (pydef-base 'pydef (current-python-module) name obj))

  (define (pydef-in container name obj)
    (pydef-base 'pydef-in container name obj))

  (define (pydefimp name module)
    (pydefimp-base 'pydefimp (current-python-module) name module))

  (define (pydefimp-in container name module)
    (pydefimp-base 'pydefimp-in container name module))

  (define (pydefun name func)
    (pydefun-base 'pydefun (current-python-module) name func))

  (define (pydefun-in container name func)
    (pydefun-base 'pydefun-in container name func))

  ;;--------------------------------------------------------------------------

  (define force-string
    (case-lambda
      [(caller obj)
       (force-string caller obj (_ "expected a string, pystring, or symbol"))]
      [(caller obj message)
       (match obj
         [(? pystring? s) (pystring->string s)]
         [(? string? s) s]
         [(? symbol? s) (symbol->string s)]
         [other (assertion-violation caller message other)] )] ))

  (define force-pystring
    (case-lambda
      [(caller obj)
       (force-pystring caller obj (_ "expected a string, pystring, or symbol"))]
      [(caller obj message)
       (match obj
         [(? pystring? s) s]
         [(? string? s) (string->pystring s)]
         [(? symbol? s) (string->pystring (symbol->string s))]
         [other (assertion-violation caller message other)] )] ))

  (define force-pymodule
    (case-lambda
      [(caller obj)
       (force-pymodule caller obj
                       (_ "expected a pymodule, string, pystring, or symbol"))]
      [(caller obj message)
       (match obj
         [(? pymodule? m) m]
         [s (python-module (force-pystring caller obj message))] )] ))

  (define (use-python-api)
    (force-py-initialized)
    (register-finalizer "Python interpreter" force-py-finalized)
    (pyimport "sortsmill.ffcompat")
    (when pkg-info:python-compatibility?
      (pyimport "fontforge"))
    (no_windowing_ui-set! #f)
    (running_script-set! #f)
    (pyimport "sortsmill.psMat")
    (when pkg-info:python-compatibility?
      (pyimport "psMat"))
    (pyimport "ffContrib.excepthook"))

  ) ;; end of library.
