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

(library (sortsmill pkg-info directory-layout)

  (export pkg-info:pixmapsdir
          pkg-info:cursorsdir
          pkg-info:htdocsdir
          pkg-info:pkgconfigdir
          pkg-info:pkgguiledatadir
          pkg-info:guilemoduledir
          pkg-info:guileobjmoduledir
          pkg-info:cythonincludedir
          pkg-info:fcmoduleincludedir
          pkg-info:pkgdatadir
          pkg-info:localedir
          pkg-info:sysconfdir)

  (import (sortsmill dynlink)
          (only (guile) eval-when))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_directory_layout"))

  ) ;; end of library.
