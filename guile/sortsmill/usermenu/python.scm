;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-

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

(library (sortsmill usermenu python)

  (export load-user_init.py

          python-menu-entry-callable->procedure

          ;; FIXME: Get rid of this, make it private, or revise
          ;; it for the menu design we end up with.
          register-python-menu-entry

          ;; Legacy Python support.
          registerMenuItem)

  (import (sortsmill python)
          (sortsmill usermenu)
          (sortsmill fontforge-api)
          (sortsmill gdraw-api)
          (sortsmill machine)
          (sortsmill fonts views)
          (sortsmill dynlink)
          (sortsmill pkg-info)
          (sortsmill notices)
          (rnrs)
          (except (guile) error)
          (system foreign))

  (sortsmill-dynlink-declarations "#include <activeinui.h>")
  (sortsmill-dynlink-declarations "#include <ffpython.h>")

  (define view->python-view
    (lambda (v)
      (assert (view? v))
      (let* ([views (python-module "sortsmill.views")]
             [views-dict (py-dict views)]
             [view-class-name (cond [(glyph-view? v) "glyph_view"]
                                    [(font-view? v)  "font_view"]
                                    [else (assert #f)])]
             [view-class (pyindexed-ref views-dict (string->pystring view-class-name))])
        ((pycallable->procedure view-class) (pointer->pylong (view->pointer v))))))

  (define python-menu-entry-callable->procedure
    (lambda (f)
      "Wrap either a Python callable to serve as an ‘action’ or
‘enabled’ function. The Guile return value of the wrapped function is
always a boolean."
      (let ([f-wrapped (pycallable->procedure f)])
        [lambda (view)
          (let ([result (f-wrapped (view->python-view view))])
            (pybool->boolean (py-not-not result)))] )))

  (define no-windowing-ui?
    (let ([proc (pointer->procedure
                 _Bool
                 (sortsmill-dynlink-func "get_no_windowing_ui")
                 '())])
      (lambda () (not (fxzero? (proc))))))

  (define (register-python-menu-entry window menu-path action enabled shortcut)
    (unless (no-windowing-ui?)
      (let ([window^    (if (pyobject? window)
                            window
                            (borrowed-pointer->pyobject window))]
            [menu-path^ (if (pyobject? menu-path)
                            menu-path
                            (borrowed-pointer->pyobject menu-path))]
            [action^    (if (pyobject? action)
                            action
                            (borrowed-pointer->pyobject action))]
            [enabled^   (if (pyobject? enabled)
                            enabled
                            (borrowed-pointer->pyobject enabled))]
            [shortcut^  (if (pyobject? shortcut)
                            shortcut
                            (borrowed-pointer->pyobject shortcut))])
        (let ([window-®    (string->symbol (string-downcase (pystring->string window^)))]
              [menu-path-® (if (pystring? menu-path^)
                               (list (pystring->string menu-path^))
                               (map pystring->string (pysequence->list menu-path^)))]
              [action-®    (python-menu-entry-callable->procedure action^)]
              [enabled-®   (if (not (pynone? enabled^))
                               (python-menu-entry-callable->procedure enabled^)
                               (lambda (view) #t))]
              [shortcut-®  (if (not (pynone? shortcut^))
                               (pystring->string shortcut^)
                               #f)])
          (register-fontforge-menu-entry #:window window-®
                                         #:menu-path menu-path-®
                                         #:action action-®
                                         #:enabled enabled-®
                                         #:shortcut shortcut-®)))))

  (define fv-active-in-ui (pointer->bytevector
                           (sortsmill-dynlink-pointer "fv_active_in_ui")
                           (sizeof '*)))

  (define sc-active-in-ui (pointer->bytevector
                           (sortsmill-dynlink-pointer "sc_active_in_ui")
                           (sizeof '*)))

  (define layer-active-in-ui (pointer->bytevector
                              (sortsmill-dynlink-pointer "layer_active_in_ui")
                              (sizeof int)))

  (define SC->PySC
    (pointer->procedure '*
                        (sortsmill-dynlink-func "PySC_From_SC")
                        '(*)))

  (define FV->PyFV
    (pointer->procedure '*
                        (sortsmill-dynlink-func "PyFV_From_FV")
                        '(*)))

  (define CVLayer
    (pointer->procedure int
                        (sortsmill-dynlink-func "CVLayer")
                        '(*)))

  (define prepare-python-fv-object
    (pointer->procedure void
                        (sortsmill-dynlink-func "prepare_python_fv_object")
                        '(*)))

  (define ly-fore 1)
  
  (define (view->legacy-python-view! v)
    (assert (view? v))
    (cond [(glyph-view? v)
           (let* ([cvb   (glyph-view->CharViewBase v)]
                  [sc    (glyph-view->SplineChar v)]
                  [layer (CVLayer (CharViewBase->pointer cvb))]
                  [fvb   (view->FontViewBase v)])

             ;; ULTRA-SUPER-MEGA-WARNING: SIDE EFFECTS!
             (set-pointer! sc-active-in-ui (SplineChar->pointer sc))
             (bytevector-sint-set! layer-active-in-ui 0 layer
                                   (native-endianness) (sizeof int))
             (prepare-python-fv-object (FontViewBase->pointer fvb))
             (set-pointer! fv-active-in-ui (FontViewBase->pointer fvb))

             (borrowed-pointer->pyobject (SC->PySC (SplineChar->pointer sc))))]

          [(font-view? v)
           (let* ([fvb   (view->FontViewBase v)]
                  [layer (FontViewBase:active-layer-ref fvb)])

             ;; ULTRA-SUPER-MEGA-WARNING: SIDE EFFECTS!
             (prepare-python-fv-object (FontViewBase->pointer fvb))
             (set-pointer! fv-active-in-ui (FontViewBase->pointer fvb))
             (bytevector-sint-set! layer-active-in-ui 0 layer
                                   (native-endianness) (sizeof int))

             (borrowed-pointer->pyobject
              (FV->PyFV (FontViewBase->pointer fvb))))] ))

  (define (reset-legacy-python-globals! v)
    (assert (view? v))
    (cond [(glyph-view? v)
           (set-pointer! sc-active-in-ui %null-pointer)
           (bytevector-sint-set! layer-active-in-ui 0 ly-fore
                                 (native-endianness) (sizeof int))
           (set-pointer! fv-active-in-ui %null-pointer)]
          [(font-view? v)
           (set-pointer! fv-active-in-ui %null-pointer)]))

  (define (legacy-python-callable->procedure data callable)
    (assert (pyobject? data))
    (assert (pycallable? callable))
    (let ([proc (pycallable->procedure callable)])
      (lambda (v)
        (let ([retval (proc data (view->legacy-python-view! v))])
          (reset-legacy-python-globals! v)
          (pybool->boolean (py-not-not retval))))))

  (define (registerMenuItem-one-window action enabled data window shortcut menu-path)
    (unless (no-windowing-ui?)
      (let ([window^    (if (pyobject? window)
                            window
                            (borrowed-pointer->pyobject window))]
            [menu-path^ (if (pyobject? menu-path)
                            menu-path
                            (borrowed-pointer->pyobject menu-path))]
            [action^    (if (pyobject? action)
                            action
                            (borrowed-pointer->pyobject action))]
            [enabled^   (if (pyobject? enabled)
                            enabled
                            (borrowed-pointer->pyobject enabled))]
            [shortcut^  (if (pyobject? shortcut)
                            shortcut
                            (borrowed-pointer->pyobject shortcut))]
            [data^      (if (pyobject? data)
                            data
                            (borrowed-pointer->pyobject data))])
        (let ([window-®    (string->symbol (string-downcase (pystring->string window^)))]
              [menu-path-® (map pystring->string (pysequence->list menu-path^))]
              [action-®    (legacy-python-callable->procedure data^ action^)]
              [enabled-®   (if (not (pynone? enabled^))
                               (legacy-python-callable->procedure data^ enabled^)
                               (lambda (view) #t))]
              [shortcut-®  (cond [(pynone? shortcut^) #f]
                                 [(string=? (pystring->string shortcut^) "None") #f]
                                 [else (pystring->string shortcut^)])])
          (register-fontforge-menu-entry #:window window-®
                                         #:menu-path menu-path-®
                                         #:action action-®
                                         #:enabled enabled-®
                                         #:shortcut shortcut-®)))))

  (define (registerMenuItem action enabled data windows shortcut menu-path)
    (cond [(pysequence? windows)
           (let ([scm-windows (map (compose string-downcase pystring->string)
                                   (pysequence->list windows))])
             (when (or (member "glyph" scm-windows) (member "char" scm-windows))
               (registerMenuItem-one-window action enabled data
                                            (string->pystring "glyph")
                                            shortcut menu-path))
             (when (member "font" scm-windows)
               (registerMenuItem-one-window action enabled data
                                            (string->pystring "font")
                                            shortcut menu-path)))]
          [else
           (registerMenuItem action enabled data
                             (borrowed-pointer->pyobject windows)
                             shortcut menu-path)]))

  (define (user-config-directory)
    (let* ([xdg-config-home (getenv "XDG_CONFIG_HOME")]
           [home (getenv "HOME")])
      (cond [xdg-config-home]
            [home (string-append (getenv "HOME") "/.config")]
            [else #f] )))

  (define (load-user_init.py)
    ;;
    ;; Load user_init.py, which typically is in
    ;; ${HOME}/.config/sortsmill-tools/
    ;;
    (let ([user-config-dir (user-config-directory)])
      (if user-config-dir
          (let ([user-init (format #f "~a/~a/user_init.py"
                                   user-config-dir pkg-info:package)])
            (if (file-exists? user-init)
                (fontforge-call-with-error-handling
                 "user_init.py"
                 (lambda () (pyexec-file-name-in-main user-init))))))))

  #|
  (define python-scripts-subdirectory
    (make-fluid
     (lambda ()
       (let ([user-config-dir (user-config-directory)])
         (if user-config-dir
             (let ([scripts-dir (format #f "~a/python" user-config-dir)])
               

  (define (load-python-scripts-subdirectory)
    ;;
    ;; Load the scripts in the python scripts subdirectory, which
    ;; typically is ${HOME}/.config/sortsmill-tools/python
    ;;
    3)
  |#

  ) ;; end of library.
