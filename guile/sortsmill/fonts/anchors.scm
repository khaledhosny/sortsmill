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

  (export
   ;; (glyph-view-anchor-classes view) → list of alists
   view-anchor-classes

   ;; (glyph-view-anchor-points gv) → list of alists
   ;;
   ;; alist keys:
   ;;   'type → 'mark 'base 'ligature 'base-mark 'entry 'exit 'unrecognized
   ;;   'name → string
   ;;   'coords → (list x y)
   ;;   'selected? → #f or #t
   ;;   'ligature-index → integer  (present if type is 'ligature)
   glyph-view-anchor-points

   ;; (glyph-view-anchor-points-set! gv list-of-alists) → unspecified
   ;; (glyph-view-anchor-points-add! gv alist) → unspecified
   glyph-view-anchor-points-set!
   glyph-view-anchor-points-add!

   ;; Get anchor point field values.
   ;;
   ;; (anchor-point-name alist) → string
   ;; (anchor-point-type alist) → symbol
   ;; (anchor-point-coords alist) → (x y)
   ;; (anchor-point-selected? alist) → boolean
   ;; (anchor-point-ligature-index alist) → integer
   ;;
   ;; These may also raise an assertion-violation, because we want the
   ;; programmer to have provided us with a good anchor point
   ;; specification, by the time these procedures are called.
   ;;
   ;; We do let programmers add their own fields, _without_ having the
   ;; presence of extra fields raise assertion violations. Probably
   ;; one should use keys that start with ‘x-’.
   anchor-point-name
   anchor-point-type
   anchor-point-coords
   anchor-point-selected?
   anchor-point-ligature-index          ; Assumes type is 'ligature.

   ;; Set or replace anchor point field values. The ‘value’ arguments
   ;; are not checked.
   ;;
   ;; (anchor-point-with-name alist value) → alist
   ;; (anchor-point-with-type alist value) → alist
   ;; (anchor-point-with-coords alist value) → alist
   ;; (anchor-point-with-selected? alist value) → alist
   ;; (anchor-point-with-ligature-index alist value) → alist
   ;; (anchor-point-with-field alist key value) → alist
   anchor-point-with-name
   anchor-point-with-type
   anchor-point-with-coords
   anchor-point-with-selected?
   anchor-point-with-ligature-index
   anchor-point-with-field
   )

  (import (sortsmill fonts views)
          (sortsmill fontforge-api)
          (sortsmill i18n)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) alist-cons unfold)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 match)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fontforge_internals"))

  (define (view-anchor-classes view)
    (let ([ac-list (view->AnchorClass-list view)])
      (map AnchorClass->alist ac-list)))

  (define (glyph-view-anchor-points gv)
    (let ([ap-list (glyph-view->AnchorPoint-list gv)])
      (map AnchorPoint->alist ap-list)))

  (define (glyph-view-anchor-points-set! gv anchor-points)
;;;;; FIXME: First remove duplicates from the anchor-points list.
;;;;;
;;;;; FIXME: Then sort the anchor points.
    (let ([ap-ptr (anchor-points->AnchorPoint-linked-list
                   gv anchor-points 'set-glyph-view-anchor-points!)]
          [sc (glyph-view->SplineChar gv)])
      (SplineChar:anchor-points-set! sc ap-ptr)
      (update-changed-SplineChar (SplineChar->pointer sc) #f)))

  (define (glyph-view-anchor-points-add! gv alist)
    (let ([anchor-points (glyph-view-anchor-points gv)])
      (assertion-violation 'glyph-view-anchor-points-add!
                           "not yet implemented")))

  (define (anchor-point-name alist)
    (match (assq 'name alist)
      [(k . (? string? v)) v]
      [(k . v) (raise:expected-string 'anchor-point-name 'name v alist)]
      [#f (raise:missing-anchor-point-field 'anchor-point-name 'name alist)]))

  (define (anchor-point-type alist)
    (match (assq 'type alist)
      [(k . (? symbol? v))
       (check-AnchorPoint-type-symbol 'anchor-point-type v)
       (when (eq? v 'ligature)
         (unless (assq 'ligature-index alist)
           (assertion-violation
            'anchor-point-type
            (_ "expected 'ligature-index field in anchor point of type 'ligature")
            alist)))
       v]
      [(k . v) (raise:expected-symbol 'anchor-point-type 'type v alist)]
      [#f (raise:missing-anchor-point-field 'anchor-point-type 'type alist)]))

  (define (anchor-point-coords alist)
    (match (assq 'coords alist)
      [(k . (? (match-lambda [((? real? x) (? real? y)) #t] [_ #f]) v)) v]
      [(k . v) (raise:expected-real-coordinates 'anchor-point-coords
                                                'coords v alist)]
      [#f (raise:missing-anchor-point-field 'anchor-point-coords
                                            'coords alist)]))

  (define (anchor-point-selected? alist)
    (match (assq 'selected? alist)
      [(k . (? boolean? v)) v]
      [(k . v) (raise:expected-boolean 'anchor-point-selected?
                                       'selected? v alist)]

      ;; The 'selected? field may be left out, and defaults to #f.
      [#f #f]
      ))

  (define (anchor-point-ligature-index alist)
    (match (assq 'ligature-index alist)
      [(k . (? integer? v)) v] ;; FIXME: Should we check more thoroughly?
      [(k . v) (raise:expected-integer 'anchor-point-ligature-index
                                       'ligature-index v alist)]
      [#f (raise:missing-anchor-point-field 'anchor-point-ligature-index
                                            'ligature-index alist)]))

  (define (anchor-point-with-field alist key value)
    (alist-cons key value (remp (lambda (k.v) (eq? (car k.v) key)) alist)))

  (define (anchor-point-with-name alist value)
    (anchor-point-with-field alist 'name value))

  (define (anchor-point-with-type alist value)
    (anchor-point-with-field alist 'type value))

  (define (anchor-point-with-coords alist value)
    (anchor-point-with-field alist 'coords value))

  (define (anchor-point-with-selected? alist value)
    (anchor-point-with-field alist 'selected? value))

  (define (anchor-point-with-ligature-index alist value)
    (anchor-point-with-field alist 'ligature-index value))

  (define (glyph-view->AnchorPoint-list gv)
    (let ([sc (glyph-view->SplineChar gv)])
      (unfold null-pointer?
              pointer->AnchorPoint
              (compose AnchorPoint:next-ref pointer->AnchorPoint)
              (SplineChar:anchor-points-ref sc))))

  (define (AnchorPoint->alist ap)
    (let ([my-coords (AnchorPoint:coords-ref ap)]
          [ac (AnchorPoint:anchor-class-dref ap)])
      (let ([type (AnchorPoint->type-symbol ap)]
            [name (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8")]
            [x    (BasePoint:x-ref my-coords)]
            [y    (BasePoint:y-ref my-coords)]
            [selected? (AnchorPoint:selected-ref ap)]
            [lig-index (AnchorPoint:lig-index-ref ap)])
        `([type      . ,type]
          [name      . ,name]
          [coords    . ,(list x y)]
          [selected? . ,selected?]
          ,@[if (eq? type 'ligature)
                `([ligature-index . ,lig-index])
                '()])
        )))

  (define (AnchorPoint-type->type-symbol type)
    (cond [(= type anchor-type:mark) 'mark]
          [(= type anchor-type:base) 'base]
          [(= type anchor-type:ligature) 'ligature]
          [(= type anchor-type:base-mark) 'base-mark]
          [(= type anchor-type:entry) 'entry]
          [(= type anchor-type:exit) 'exit]
          [else 'unrecognized]))

  (define (AnchorPoint->type-symbol ap)
    (AnchorPoint-type->type-symbol (AnchorPoint:type-ref ap)))

  (define* (type-symbol->AnchorPoint-type symb #:optional who)
    (match symb
      ['mark      anchor-type:mark]
      ['base      anchor-type:base]
      ['ligature  anchor-type:ligature]
      ['base-mark anchor-type:base-mark]
      ['entry     anchor-type:entry]
      ['exit      anchor-type:exit]
      [_ (assertion-violation
          (if who who 'type-symbol->AnchorPoint-type)
          (_ "unrecognized anchor point type") symb)] ))

  (define (check-AnchorPoint-type-symbol who symb)
    (type-symbol->AnchorPoint-type symb who))

  (define* (anchor-points->AnchorPoint-linked-list gv anchor-points #:optional who)
    (match anchor-points
      [() %null-pointer]
      [(h . t)
       (let ([tail (anchor-points->AnchorPoint-linked-list gv t who)])
         (catch #t
           [lambda ()
             (let ([ap (glyph-view-and-alist->AnchorPoint gv h who)])
               (AnchorPoint:next-set! ap tail)
               (AnchorPoint->pointer ap))]
           [lambda args
             (free-AnchorPoint-linked-list tail)
             (apply throw args)] ))] ))

  (define* (glyph-view-and-alist->AnchorPoint gv alist #:optional who)
    (alist->AnchorPoint (glyph-view->AnchorClass-linked-list gv) alist who))

  (define* (alist->AnchorPoint ac-ptr alist #:optional who)
    (let ([name (anchor-point-name alist)]
          [type (anchor-point-type alist)]
          [coords (anchor-point-coords alist)]
          [selected? (anchor-point-selected? alist)])
      (let ([ac (find-AnchorClass ac-ptr name)])
        (unless ac
          (error (if who who 'alist->AnchorPoint)
                 (format #f (_ "anchor class `~a' not found") name)
                 alist))
        (check-conformability-with-AnchorClass who ac type alist)
        (let ([ap (malloc-AnchorPoint)])
          (catch #t
            [lambda ()
              (AnchorPoint:anchor-class-set! ap (AnchorClass->pointer ac))
              (AnchorPoint:type-set! ap (type-symbol->AnchorPoint-type type))
              (let ([bp (AnchorPoint:coords-ref ap)])
                (BasePoint:x-set! bp (car coords))
                (BasePoint:y-set! bp (cadr coords)))
              (AnchorPoint:selected-set! ap selected?)
              (when (eq? type 'ligature)
                (AnchorPoint:lig-index-set! ap (anchor-point-ligature-index alist)))]
            [lambda args
              (free-AnchorPoint ap)
              (apply throw args)] )
          ap))))

  (define (check-conformability-with-AnchorClass who ac type alist)
;;;;;;    (let ([ac-type (AnchorClass:type-ref ac)])
    (let ([ac-type (OTLookup:type-ref
                    (LookupSubtable:lookup-dref (AnchorClass:subtable-dref ac)))])
      (cond
       [(= ac-type lookup-type:gpos-mark-to-base)
        (unless (or (eq? type 'mark) (eq? type 'base))
          (assertion-violation
           who
           (format #f (_ "anchor class `~a' allows only 'mark and 'base anchors")
                   (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
           alist))]
       [(= ac-type lookup-type:gpos-mark-to-mark)
        (unless (or (eq? type 'mark) (eq? type 'base-mark))
          (assertion-violation
           who
           (format #f (_ "anchor class `~a' allows only 'mark and 'base-mark anchors")
                   (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
           alist))]
       [(= ac-type lookup-type:gpos-cursive)
        (unless (or (eq? type 'entry) (eq? type 'exit))
          (assertion-violation
           who
           (format #f (_ "anchor class `~a' allows only 'entry and 'exit anchors")
                   (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
           alist))]
       [(= ac-type lookup-type:gpos-mark-to-ligature)
        (unless (or (eq? type 'mark) (eq? type 'ligature))
          (assertion-violation
           who
           (format #f (_ "anchor class `~a' allows only 'mark and 'ligature anchors")
                   (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
           alist))]
       [_ (assertion-violation
           who
           "internal error: unrecognized anchor class lookup type"
           ac-type)])))

  (define (find-AnchorClass ac-ptr name)
    (if (null-pointer? ac-ptr)
        #f
        (let ([ac (pointer->AnchorClass ac-ptr)])
          (if (string=? (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8")
                        name)
              ac
              (find-AnchorClass (AnchorClass:next-ref ac) name)))))

  (define (view->AnchorClass-list view)
    (AnchorClass-linked-list->AnchorClass-list
     (view->AnchorClass-linked-list view)))

  (define (AnchorClass-linked-list->AnchorClass-list ac-ptr)
    (unfold null-pointer?
            pointer->AnchorClass
            (compose AnchorClass:next-ref pointer->AnchorClass)
            ac-ptr))

  (define (view->AnchorClass-linked-list view)
    (cond [(font-view? view) (font-view->AnchorClass-linked-list view)]
          [(glyph-view? view) (glyph-view->AnchorClass-linked-list view)]
          [else (assertion-violation 'view->AnchorClass-linked-list
                                     "internal error: expected a view"
                                     view)]))

  (define (font-view->AnchorClass-linked-list fv)
    (SplineFont:anchor-classes-ref (font-view->SplineFont fv)))

  (define (glyph-view->AnchorClass-linked-list gv)
    (SplineFont:anchor-classes-ref
     (SplineChar:parent-dref (glyph-view->SplineChar gv))))

  (define (glyph-view->AnchorClass gv name)
    (find-AnchorClass (glyph-view->AnchorClass-linked-list gv) name))

  (define (AnchorClass->alist ac)
    3)

  #|
  (define (anchor-point-match? AnchorClass-linked-list alist1 alist2)
      (let ([
      )

  #|
  find_anchor_point sc::glyph partial_record = find $ SplineChar_anchor sc
  with
    match_name ap::pointer =
      (AnchorClass_name $ AnchorPoint_anchor ap) == partial_record!"name";
    match ap::pointer =
      case AnchorClass_type $ AnchorPoint_anchor ap of
        fontforge::act_mklg =
          AnchorPoint_lig_index == partial_record!"lig_index" && match_name ap;
        fontforge::act_mark = match_name ap;
        _ = AnchorPoint_type ap == partial_record!"type" && match_name ap;
      end;
    find ap::pointer = ap if null ap || match ap;
    find ap::pointer = find (AnchorPoint_next ap) otherwise;
  end;
  |#
  |#

  (define (raise:expected-string who key value alist)
    (assertion-violation
     who
     (format #f (_ "expected a string for anchor point field '~a but got ~s")
             key value)
     alist))
  
  (define (raise:expected-symbol who key value alist)
    (assertion-violation
     who
     (format #f (_ "expected a symbol for anchor point field '~a but got ~s")
             key value)
     alist))
  
  (define (raise:expected-real-coordinates who key value alist)
    (assertion-violation
     who
     (format #f (_ "expected real-number coordinates for anchor point field '~a but got ~s")
             key value)
     alist))
  
  (define (raise:expected-boolean who key value alist)
    (assertion-violation
     who
     (format #f (_ "expected #f or #t for anchor point field '~a but got ~s")
             key value)
     alist))
  
  (define (raise:expected-integer who key value alist)
    (assertion-violation
     who
     (format #f (_ "expected an integer for anchor point field '~a but got ~s")
             key value)
     alist))
  
  (define (raise:missing-anchor-point-field who key alist)
    (assertion-violation
     who
     (format #f (_ "missing anchor point field '~a") key)
     alist))
  
  ) ;; end of library.
