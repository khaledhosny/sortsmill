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

(library (sortsmill fonts anchors)

  (export glyph-view-anchor-points)

  (import (sortsmill fonts views)
          (sortsmill fontforge-api)
          (rnrs)
          (except (guile) error))

  (define (glyph-view-anchor-points gv)
    (let ([sc (glyph-view->SplineChar gv)])
      (format #f "Not yet implemented; but, in the meanwhile, have a look at this SplineChar: ~s\n" sc)))

  ) ;; end of library.
