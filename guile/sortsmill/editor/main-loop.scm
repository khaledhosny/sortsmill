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

(library (sortsmill editor main-loop)

  (export exit-main-loop
          main-loop)

  (import (sortsmill dynlink)
          (sortsmill kwargs)
          (rnrs)
          (except (guile) error)
          (system foreign))

  (define %main-loop-exit-prompt (make-prompt-tag))

  (define/kwargs (exit-main-loop (exit-status 0))
    (abort-to-prompt %main-loop-exit-prompt
                     `[(exit-status . ,exit-status)] ))

  (define main-loop-thunk
    (let ([GDrawEventLoop
           (pointer->procedure
            void
            (sortsmill-dynlink-func "GDrawEventLoop"
                                    "#include <gdraw.h>")
            '(*))])
      [lambda ()
        (GDrawEventLoop %null-pointer)
        (exit-main-loop)] ))

  (define (main-loop-exit-handler continuation alist)
    alist)

  (define (main-loop)
    (call-with-prompt %main-loop-exit-prompt
                      main-loop-thunk
                      main-loop-exit-handler))

  ) ;; end of library.
