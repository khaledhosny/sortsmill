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

  (export python-menu-entry-callable->procedure

          ;; FIXME: Get rid of this, make it private, or revise
          ;; it for the menu design we end up with.
          register-python-menu-entry
          )

  (import (sortsmill python)
          (sortsmill usermenu)
          (rename (sortsmill usermenu)
                  (action-entry action-entry^))
          (sortsmill fontforge-api)
          (sortsmill gdraw-api)
          (sortsmill machine)
          (sortsmill views)
          (rnrs)
          (except (guile) error)
          (system foreign))

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

  (define python-dll (dynamic-link "libguile-sortsmill_fontforgeexe"))

  (define no-windowing-ui?
    (let ([proc (pointer->procedure
                 _Bool (dynamic-func "get_no_windowing_ui" python-dll) '())])
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
              [menu-path-® (map pystring->string (pysequence->list menu-path^))]
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
                           (dynamic-pointer "fv_active_in_ui" python-dll)
                           (sizeof '*)))

  (define sc-active-in-ui (pointer->bytevector
                           (dynamic-pointer "sc_active_in_ui" python-dll)
                           (sizeof '*)))

  (define layer-active-in-ui (pointer->bytevector
                              (dynamic-pointer "layer_active_in_ui" python-dll)
                              (sizeof int)))

  (define SC->PySC
    (pointer->procedure '* (dynamic-func "PySC_From_SC" python-dll) '(*)))

  (define FV->PyFV
    (pointer->procedure '* (dynamic-func "PyFV_From_FV" python-dll) '(*)))

  (define CVLayer
    (pointer->procedure int (dynamic-func "CVLayer" python-dll) '(*)))

  (define ly-fore 1)
  
  (define (view->legacy-python-view! v)
    (assert (view? v))
    (cond [(glyph-view? v)
           (let* ([cvb   (glyph-view->CharViewBase v)]
                  [sc    (CharViewBase:sc-dref cvb)]
                  [layer (CVLayer (CharViewBase->pointer cvb))])

             ;; ULTRA-SUPER-MEGA-WARNING: SIDE EFFECTS!
             (set-pointer! sc-active-in-ui (SplineChar->pointer sc))
             (bytevector-sint-set! layer-active-in-ui 0 layer
                                   (native-endianness) (sizeof int))

             (borrowed-pointer->pyobject
              (SC->PySC (SplineChar->pointer sc))))]

          [(font-view? v)
           (let* ([fvb   (font-view->FontViewBase v)]
                  [layer (FontViewBase:active-layer-ref fvb)])

             ;; ULTRA-SUPER-MEGA-WARNING: SIDE EFFECTS!
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
                                 (native-endianness) (sizeof int))]
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

  ) ;; end of library.
