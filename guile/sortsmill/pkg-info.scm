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

(library (sortsmill pkg-info)

  (export

   ;; Reëxported from (sortsmill pkg-info package).
   pkg-info:package
   pkg-info:package-bugreport
   pkg-info:package-name
   pkg-info:package-string
   pkg-info:package-tarname
   pkg-info:package-url
   pkg-info:package-version

   ;; Reëxported from (sortsmill pkg-info version).
   pkg-info:version-major
   pkg-info:version-minor
   pkg-info:version-patch
   pkg-info:version-extra
   pkg-info:version-extra-short

   ;; Reëxported from (sortsmill pkg-info pure).
   pkg-info:have-pure-api?

   ;; Reëxported from (sortsmill pkg-info python).
   pkg-info:have-python-api?
   pkg-info:python-compatibility?
   pkg-info:py-major-version
   pkg-info:py-minor-version
   pkg-info:py-micro-version
   pkg-info:py-release-level
   pkg-info:py-release-serial
   pkg-info:py-version
   pkg-info:py-version-hex
   pkg-info:pyinit-function-name

   ;; Reëxported from (sortsmill pkg-info i18n).
   pkg-info:textdomain

   ;; Reëxported from (sortsmill pkg-info directory-layout).
   pkg-info:pixmapsdir
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

  (import (sortsmill pkg-info package)
          (sortsmill pkg-info version)
          (sortsmill pkg-info pure)
          (sortsmill pkg-info python)
          (sortsmill pkg-info i18n)
          (sortsmill pkg-info directory-layout))

  ) ;; end of library.
