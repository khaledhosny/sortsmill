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

  (export exit-editor-main-loop
          editor-main-loop
          load-site-init)

  (import (sortsmill editor finalization)
          (sortsmill dynlink)
          (sortsmill kwargs)
          (rnrs)
          (except (guile) error)
          (system foreign))

  (define %editor-main-loop-exit-prompt (make-prompt-tag))

  (define/kwargs (exit-editor-main-loop (exit-status 0)
                                        (alist '()))
    (abort-to-prompt %editor-main-loop-exit-prompt
                     `[(exit-status . ,exit-status)
                       ,@alist] ))

  (define editor-main-loop-thunk
    (let ([GDrawEventLoop
           (pointer->procedure
            void
            (sortsmill-dynlink-func "GDrawEventLoop"
                                    "#include <gdraw.h>")
            '(*))])
      [lambda ()
        (GDrawEventLoop %null-pointer)
        (exit-editor-main-loop)] ))

  (define (editor-main-loop-exit-handler continuation alist)
    alist)

  (define (load-site-init site-init)
    (if (file-exists? site-init)
        (primitive-load site-init)))

  (define (editor-main-loop)
    (let ([retval (call-with-prompt %editor-main-loop-exit-prompt
                                    editor-main-loop-thunk
                                    editor-main-loop-exit-handler)])
      (run-and-clear-all-finalizers)
      retval))

  ) ;; end of library.
