;; -*- mode: scheme; coding: utf-8 -*-

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

(library (sortsmill mac-encodings)

  (export
   ;; (mac-encoded-string->string bv mac-encoding-int mac-language-int) → string
   mac-encoded-string->string

   ;; (string->mac-encoded-string string mac-encoding-int mac-language-int) → bytevector
   string->mac-encoded-string

   ;; (mac-encoding-from-mac-language integer) → integer
   mac-encoding-from-mac-language

   ;; (windows-language-from-mac-language integer) → integer
   windows-language-from-mac-language

   ;; (mac-language-from-windows-language integer) → integer
   mac-language-from-windows-language

   ;; (can-encode-for-mac-given-windows-language? integer) → boolean
   can-encode-for-mac-given-windows-language?

   ;; (mac-language-code-from-locale) → integer
   mac-language-code-from-locale

   ;; (mac-language-from-code integer) → string
   mac-language-from-code
   )

  (import (sortsmill dynlink)
          (only (guile) eval-when))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_fontforge_macenc_guile"))

  ) ;; end of library.
