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

(library (sortsmill fonts glyphs)

  (export
   ;; (glyph&layer->glyph-view glyph-view) → glyph-view
   ;; (glyph&layer->glyph-view (list glyph-view layer)) → glyph-view
   glyph&layer->glyph-view

   ;; (glyph&layer->layer glyph-view) → editable-layer
   ;; (glyph&layer->layer (list glyph-view layer)) → layer
   glyph&layer->layer

   ;; (glyph&layer->layer glyph-view) → editable-layer
   ;; (glyph&layer->glyph-view-and-layer (list glyph-view layer)) → glyph-view layer
   glyph&layer->glyph-view-and-layer

   ;; (glyph&layer:update-changed glyph-view) → *unspecified*
   ;; (glyph&layer:update-changed (list glyph-view layer)) → *unspecified*
   glyph&layer:update-changed

   ;; (view:update-layer-palette view) → *unspecified*
   view:update-layer-palette

   ;; (view:active-layer view) → layer
   ;; (view:active-layer-set! view layer) → *unspecified*
   view:active-layer
   view:active-layer-set!

   ;; Find out which layer currently is ‘editable’ in the glyph view:
   ;;
   ;; (glyph-view:editable-layer gv) → layer
   ;; (glyph-view:editable-layer-set! gv layer) → *unspecified*
   glyph-view:editable-layer
   glyph-view:editable-layer-set!

   ;; (view:layer-names view) → list-of-strings
   view:layer-names

   ;; (glyph&layer:preserve-as-undo glyph&layer) → *unspecified*
   ;; (glyph&layer:preserve-as-undo glyph&layer #:hints? boolean) → *unspecified*
   glyph&layer:preserve-as-undo

   ;; (glyph-view:transform-by-psmat glyph-view psmat) → *unspecified*
   ;; (glyph-view:transform-by-psmat glyph-view psmat flags) → *unspecified*
   glyph-view:transform-by-psmat

   ;; (glyph-view:width glyph-view) → integer
   ;; (glyph-view:width-set! glyph-view integer) → *unspecified*
   glyph-view:width
   glyph-view:width-set!

   ;; (view:glyph-count view) → non-negative integer
   ;; (view:glyphs view) → vector of glyph-view entries
   view:glyph-count
   view:glyphs

   ;; Conversion between the Guile representation of layers and the
   ;; integer representation used internally in the C code as a legacy
   ;; from the original FontForge. These procedures are liable to
   ;; change or go away without warning.
   ;;
   ;; (layer->integer layer) → integer
   ;; (integer->layer integer) → layer
   ;;
   ;; where layer is a non-negative integer, string, 'all, 'guide, or
   ;; #f.
   layer->integer
   integer->layer
   )

  (import (sortsmill fonts views)
          (sortsmill dynlink)
          (sortsmill fontforge-api)
          (sortsmill i18n)
          (sortsmill __internals__ glyphs)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_glyphs"))

  (define (raise-not-a-glyph&layer who obj)
    (assertion-violation who (_ "expected a glyph&layer") obj))

  (define (glyph&layer->glyph-view gl)
    (match gl
      [(? glyph-view? gv) gv]
      [((? glyph-view? gv) layer) gv]
      [_ (raise-not-a-glyph&layer 'glyph&layer->glyph-view gl)]))

  (define (glyph&layer->layer gl)
    (match gl
      [(or (? glyph-view? gv) ((? glyph-view? gv) 'editable))
       (glyph-view:editable-layer gv)]
      [((? glyph-view? gv) 'active) (view:active-layer gv)]
      [((? glyph-view? gv) layer) layer]
      [_ (raise-not-a-glyph&layer 'glyph&layer->layer gl)]))

  (define (glyph&layer->glyph-view-and-layer gl)
    (match gl
      [(or (? glyph-view? gv) ((? glyph-view? gv) 'editable))
       (values gv (glyph-view:editable-layer gv))]
      [((? glyph-view? gv) 'active) (values gv (view:active-layer gv))]
      [((? glyph-view? gv) layer) (values gv layer)]
      [_ (raise-not-a-glyph&layer 'glyph&layer->glyph-view-and-layer gl)]))

  (define (glyph&layer:update-changed gl)
    (let-values ([(gv layer) (glyph&layer->glyph-view-and-layer gl)])
      (update-changed-SplineChar
       (SplineChar->pointer (glyph-view->SplineChar gv)) layer)))

  (define (view:active-layer view)
    (integer->layer
     (FontViewBase:active-layer-ref (view->FontViewBase view))))

  (define (view:active-layer-set! view layer)
    (FontViewBase:active-layer-set! (view->FontViewBase view)
                                    (layer->integer layer)))

  (define* (glyph&layer:preserve-as-undo gl #:key [hints? #f])
    (let-values ([(gv layer) (glyph&layer->glyph-view-and-layer gl)])
      (let ([sc (glyph-view->SplineChar gv)])
        (match layer
          ['guide (private:view:preserve-guide-layer-as-undo gv)]
          ['all (for-each
                 (lambda (layer^)
                   (glyph&layer:preserve-as-undo `(,gv ,layer^) #:hints? hints?))
                 (view:layer-names gv))]
          [_ (private:glyph&layer:preserve-nonguide-layer-as-undo
              'glyph&layer:preserve-as-undo
              gl (view:layer-names gv) hints?)] ))))

  ) ;; end of library.
