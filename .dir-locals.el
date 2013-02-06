((python-mode . ((python-indent . 2)))
 (cython-mode . ((python-indent . 2)))
 (makefile-mode . ((tab-width . 4)))
 (makefile-automake-mode . ((tab-width . 4)))
 (c-mode . ((c-file-style  . "gnu")
            (comment-start . "//")
            (comment-end   . "")))
 (scheme-mode . ((geiser-scheme-implementation . guile)))
 )

;;;
;;; Notes:
;;;
;;;   * You can do something like this: (c-mode . ((mode . c++)))
;;;
;;;     See
;;;     http://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode
;;;
