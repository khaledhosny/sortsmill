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

(import (sortsmill usermenu)
        (sortsmill notices)
        (sortsmill pkg-info))

(if (not (getenv "HOME"))
    (let ([rnrs-error (@ (rnrs) error)]
          [_          (@ (sortsmill i18n) _)])
      (rnrs-error
       "site-init.scm"
       (_ "the HOME environment variable is not set; please set it"))))

;;
;; Try to ensure access to Sorts Mill Tools’s Guile modules.
;;
;; FIXME: Try to ensure access to LTDL modules, too.
;;
(eval-when (compile load eval)
  (unless (member pkg-info:guilemoduledir %load-path)
    (add-to-load-path pkg-info:guilemoduledir)))
(eval-when (compile load eval)
  (unless (member pkg-info:guileobjmoduledir %load-compiled-path)
    (set! %load-compiled-path
          (cons pkg-info:guileobjmoduledir %load-compiled-path))))

(fontforge-call-with-error-handling
 "site-init.scm"
 (lambda () ;; start of thunk.

   ;;
   ;; Install a more pleasant (one hopes) GSL error handler.
   ;;
   ((@ (sortsmill math gsl) gsl:set-error-handler-raise-error))

   ;;
   ;; Load local-init.scm, which typically is in /etc/sortsmill-tools/
   ;;
   (let ([local-init (format #f "~a/~a/local-init.scm"
                             pkg-info:sysconfdir pkg-info:package)])
     (if (file-exists? local-init)
         (fontforge-call-with-error-handling
          "local-init.scm"
          (lambda () (primitive-load local-init)))))

   ;;
   ;; Load user-init.scm, which typically is in
   ;; ${HOME}/.config/sortsmill-tools/
   ;;
   (let* ([xdg-config-home (getenv "XDG_CONFIG_HOME")]
          [home (getenv "HOME")]
          [user-config-dir
           (cond [xdg-config-home]
                 [home (format #f "~a/.config" (getenv "HOME"))]
                 [else #f] )] )
     (if user-config-dir
         (let ([user-init (format #f "~a/~a/user-init.scm"
                                  user-config-dir pkg-info:package)])
           (if (file-exists? user-init)
               (fontforge-call-with-error-handling
                "user-init.scm"
                (lambda () (primitive-load user-init)))))))

   ;; Load the ‘Tools’ menus. FIXME: One _should_ be able to change
   ;; menu items after this, but for now we require the menus be
   ;; loaded from a ‘template tree’ this one time.
   (activate-gui-tools)

   ) ;; end of thunk.
 )   ;; end of fontforge-call-with-error-handling.
