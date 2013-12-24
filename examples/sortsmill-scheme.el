;;; sortsmill-scheme.el

;; Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

;;
;; Put this in your .emacs file:
;;
;;   (require 'sortsmill-scheme)
;;   (add-hook 'scheme-mode-hook 'sortsmill-scheme-mode-hook)
;;

(require 'scheme)

(defun sortsmill-scheme-mode-hook ()
  "Scheme mode hook for Sorts Mill programs."
  (interactive)
  (make-local-variable 'scheme-indent-function)
  (put 'in-python-module 'scheme-indent-function 1)
  (put 'pydefun 'scheme-indent-function 1)
  (put 'pydefun-in 'scheme-indent-function 1)
  (put 'lambda-scm-procedure 'scheme-indent-function 1)
  (put 'lambda/kwargs 'scheme-indent-function 1)
  (put 'call-with-uninm-names-db 'scheme-indent-function 1)
  (put 'call-with-po-file 'scheme-indent-function 1)

  ;;; FIXME: Maybe we can let these be handled by Scheme mode itself,
  ;;; someday.
  (put 'library 'scheme-indent-function 1)
  (put 'eval-when 'scheme-indent-function 1)
  (put 'lambda* 'scheme-indent-function 1)
  (put 'rec 'scheme-indent-function 1)
  (put 'sxml-match 'scheme-indent-function 1)
  )

(provide 'sortsmill-scheme)

;;; sortsmill-scheme.el ends here
