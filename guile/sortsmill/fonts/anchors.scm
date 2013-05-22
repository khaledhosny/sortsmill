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

(library (sortsmill fonts anchors)

  (export
   ;; (view:anchor-classes view) → list of alists
   ;;
   ;; alist keys:
   ;;   'name → string
   ;;   'subtable-name → string
   ;;   'lookup-name → string
   ;;   'lookup-type → 'gpos-cursive 'gpos-mark-to-base
   ;;                     'gpos-mark-to-ligature 'gpos-mark-to-mark
   view:anchor-classes

   ;; (glyph-view:anchor-points gv) → list of alists
   ;;
   ;; alist keys:
   ;;   'type → 'mark 'base 'ligature 'base-mark 'entry 'exit
   ;;   'coords → (list x y)
   ;;   'selected? → #f or #t
   ;;   'ligature-index → integer  (present if type is 'ligature)
   ;;
   ;; In addition, the results of @code{view:anchor-class} for the
   ;; anchor point’s class are included in the alist.
   glyph-view:anchor-points

   ;; (glyph-view:anchor-points-set! gv list-of-alists) → unspecified
   ;; (glyph-view:anchor-points-add! gv alist) → unspecified
   ;;
   ;; @code{glyph-view:anchor-points-set!} ignores duplicate entries,
   ;; and so an anchor point can be replaced simply by consing a new
   ;; entry onto the list-of-alists; see the definition of
   ;; @code{glyph-view:anchor-points-add!} for an example.
   glyph-view:anchor-points-set!
   glyph-view:anchor-points-add!

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

   ;; Control over the sort order of anchor points.
   ;;
   ;; In case anyone cares: the sort is stable, and therefore a
   ;; function always returning #f is equivalent to not sorting at
   ;; all, except for the time spent. Thus there is no special
   ;; provision for turning off sorting.
   ;;
   ;; ((fluid-ref anchor-point-<-for-sorting) alist1 alist2) → boolean
   anchor-point-<-for-sorting

   ;; Some procedures that are mainly for use for use internally,
   ;; rather than in extensions. They are liable to change or go away
   ;; without warning.
   ;;
   ;; FIXME: Change this documentation if we switch to
   ;; garbage-collected AnchorPoints.
   ;;
   ;; (AnchorClasses->scm AnchorClass*) → list of alists
   ;; (AnchorPoints->scm AnchorPoint*) → list of alists
   ;; (scm->AnchorPoint AnchorClass* alist) → malloced AnchorPoint*
   ;; (scm->AnchorPoints AnchorClass* list-of-alists) → malloced AnchorPoint*
   ;; (sort-anchor-points AnchorClass* list-of-alists) → list of alists
   ;; (sort-AnchorPoints AnchorClass* AnchorPoint*) → malloced AnchorPoint*
   AnchorClasses->scm
   AnchorPoints->scm
   scm->AnchorPoint
   scm->AnchorPoints
   sort-anchor-points
   sort-AnchorPoints
   )

  (import (sortsmill fonts views)
          (sortsmill fontforge-api)
          (sortsmill i18n)
          (sortsmill __internals__)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) alist-cons delete-duplicates unfold)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 match)
          (ice-9 format))

  ;;-----------------------------------------------------------------------

  (define (view:anchor-classes view)
    (let ([ac-list (view->AnchorClass-list view)])
      (map AnchorClass->alist ac-list)))

  (define (default-anchor-point-<-for-sorting alist1 alist2)
    (let ([coords1 (anchor-point-coords alist1)]
          [coords2 (anchor-point-coords alist2)])
      (if (< (car coords1) (car coords2))
          #t
          (if (< (car coords2) (car coords1))
              #f
              (if (< (cadr coords1) (cadr coords2))
                  #t
                  (let ([ap-type1 (anchor-point-type alist1)]
                        [ap-type2 (anchor-point-type alist2)])
                    (if (< (anchor-point-type-index ap-type1)
                           (anchor-point-type-index ap-type2))
                        #t
                        (if (eq? ap-type1 ap-type2 'gpos-mark-to-ligature)
                            (< (anchor-point-ligature-index alist1)
                               (anchor-point-ligature-index alist2))
                            #f))))))))

  (define anchor-point-<-for-sorting
    (make-fluid default-anchor-point-<-for-sorting))

  (define (glyph-view:anchor-points gv)
    (let ([ap-list (glyph-view->AnchorPoint-list gv)])
      (map AnchorPoint->alist ap-list)))

  (define (glyph-view:anchor-points-set! gv anchor-points)
    (let* ([who 'glyph-view:anchor-points-set!]
           [ac-ptr (glyph-view->AnchorClass-linked-list gv)]
           [anchor-points-sorted
            (sort-anchor-points ac-ptr anchor-points)]
           [ap-ptr (scm->AnchorPoints ac-ptr anchor-points-sorted)]
           [sc (glyph-view->SplineChar gv)])
      (SplineChar:anchor-points-set! sc ap-ptr)
      (update-changed-SplineChar (SplineChar->pointer sc) #f)))

  (define (glyph-view:anchor-points-add! gv alist)
    (let ([anchor-points (glyph-view:anchor-points gv)])
      (glyph-view:anchor-points-set! gv (cons alist anchor-points))))

  (define (anchor-point-name alist)
    (match (assq 'name alist)
      [(k . (? string? v)) v]
      [(k . v) (raise:expected-string 'anchor-point-name 'name v alist)]
      [#f (raise:missing-anchor-point-field 'anchor-point-name 'name alist)]))

  (define (anchor-point-type alist)
    (match (assq 'type alist)
      [(k . (? symbol? v))
       (check-AnchorPointType-symbol 'anchor-point-type v)
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

  (define (AnchorClasses->scm ac-ptr)
    (map AnchorClass->alist
         (AnchorClass-linked-list->AnchorClass-list ac-ptr)))

  (define (AnchorPoints->scm ap-ptr)
    (map AnchorPoint->alist
         (AnchorPoint-linked-list->AnchorPoint-list ap-ptr)))

  (define (scm->AnchorPoint AnchorClasses alist)
    (alist->AnchorPoint AnchorClasses alist 'scm->AnchorPoint))

  (define (scm->AnchorPoints AnchorClasses anchor-points)
    (match anchor-points
      [() %null-pointer]
      [(h . t)
       (let ([tail (scm->AnchorPoints AnchorClasses t)])
         (catch #t
           [lambda ()
             (let ([ap (scm->AnchorPoint AnchorClasses h)])
               (AnchorPoint:next-set! ap tail)
               (AnchorPoint->pointer ap))]
           [lambda args
             (free-AnchorPoint-linked-list tail)
             (apply throw args)] ))] ))

  (define (sort-anchor-points ac-ptr anchor-points)
    "Stable sorting of anchor point lists. Before sorting, removes
redundant entries and completes the association lists with data from
the corresponding anchor classes."
    ;; R⁶RS specifies that list-sort be stable.  See
    ;; http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-5.html#node_chap_4
    (list-sort
     (fluid-ref anchor-point-<-for-sorting)
     (map (cut complete-anchor-point ac-ptr <> 'sort-anchor-points)
          (delete-duplicates
           anchor-points (cut anchor-point-match? ac-ptr <> <>)))))

  (define (sort-AnchorPoints ac-ptr ap-ptr)
    (scm->AnchorPoints
     ac-ptr (sort-anchor-points ac-ptr (AnchorPoints->scm ap-ptr))))

  ;;-----------------------------------------------------------------------

  (define (glyph-view->AnchorPoint-list gv)
    (AnchorPoint-linked-list->AnchorPoint-list
     (SplineChar:anchor-points-ref (glyph-view->SplineChar gv))))

  (define (AnchorPoint-linked-list->AnchorPoint-list ap-ptr)
    (unfold null-pointer?
            pointer->AnchorPoint
            (compose AnchorPoint:next-ref pointer->AnchorPoint)
            ap-ptr))

  (define* (complete-anchor-point ac-ptr alist #:optional who)
    "Fill an anchor point’s association list with fields from its
anchor class. This action also corrects erroneous entries that the
programmer may have inserted."
    (let* ([name (anchor-point-name alist)]
           [ac (checking-find-AnchorClass
                (if who who 'complete-anchor-point)
                ac-ptr name (list alist))]
           [completed-alist (append (AnchorClass->alist ac) alist)])
      (delete-duplicates completed-alist
                         (lambda (a b) (eq? (car a) (car b))))))

  (define (AnchorPoint->alist ap)
    (let ([my-coords (AnchorPoint:coords-ref ap)]
          [ac (AnchorPoint:anchor-class-dref ap)])
      (let ([type (AnchorPoint->type-symbol ap)]
            [name (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8")]
            [x    (BasePoint:x-ref my-coords)]
            [y    (BasePoint:y-ref my-coords)]
            [selected? (AnchorPoint:selected-ref ap)]
            [lig-index (AnchorPoint:lig-index-ref ap)])
        ;; This association list may include whatever potentially
        ;; useful information we feel like providing. (FIXME: A Python
        ;; interface using dicts instead of tuples could do the same
        ;; thing, and would not have required creation of the
        ;; ‘anchorPointsWithSel’ version.)
        `([type      . ,type]
          [coords    . ,(list x y)]
          [selected? . ,selected?]
          ,@[AnchorClass->alist ac]
          ,@[if (eq? type 'ligature)
                `([ligature-index . ,lig-index])
                '()])
        )))

  (define (AnchorPoint->type-symbol ap)
    (AnchorPointType->type-symbol (AnchorPoint:type-ref ap)))

  (define (check-AnchorPointType-symbol who symb)
    (type-symbol->AnchorPointType symb who))

#|
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
|#

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
              (AnchorPoint:type-set! ap (type-symbol->AnchorPointType type))
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
    (let ([ac-type (AnchorClass->type-symbol ac)])
      (match ac-type
        ['gpos-mark-to-base
         (unless (or (eq? type 'mark) (eq? type 'base))
           (assertion-violation
            who
            (format #f (_ "anchor class `~a' allows only 'mark and 'base anchors")
                    (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
            alist))]
        ['gpos-mark-to-mark
         (unless (or (eq? type 'mark) (eq? type 'base-mark))
           (assertion-violation
            who
            (format #f (_ "anchor class `~a' allows only 'mark and 'base-mark anchors")
                    (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
            alist))]
        ['gpos-cursive
         (unless (or (eq? type 'entry) (eq? type 'exit))
           (assertion-violation
            who
            (format #f (_ "anchor class `~a' allows only 'entry and 'exit anchors")
                    (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8"))
            alist))]
        ['gpos-mark-to-ligature
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

  (define (AnchorClass->type-symbol ac)
    (OTLookupType->type-symbol (OTLookup:type-ref
                                (LookupSubtable:lookup-dref
                                 (AnchorClass:subtable-dref ac)))))

  (define (find-AnchorClass ac-ptr name)
    (if (null-pointer? ac-ptr)
        #f
        (let ([ac (pointer->AnchorClass ac-ptr)])
          (if (string=? (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8")
                        name)
              ac
              (find-AnchorClass (AnchorClass:next-ref ac) name)))))

  (define* (checking-find-AnchorClass who ac-ptr name potential-irritants)
    (let ([ac (find-AnchorClass ac-ptr name)])
      (unless ac
        (apply assertion-violation
               who
               (format #f (_ "anchor class `~a' not found") name)
               potential-irritants))
      ac))

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

  (define (AnchorClass->alist ac)
    (let* ([name (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8")]
           [subtable (AnchorClass:subtable-dref ac)]
           [subtable-name (pointer->string (LookupSubtable:name-ref subtable)
                                           -1 "UTF-8")]
           [lookup (LookupSubtable:lookup-dref subtable)]
           [lookup-name (pointer->string (OTLookup:name-ref lookup)
                                         -1 "UTF-8")]
           [lookup-type (OTLookupType->type-symbol (OTLookup:type-ref lookup))])
      ;; This association list may include whatever potentially useful
      ;; information we feel like providing. (FIXME: A Python
      ;; interface using dicts instead of tuples could do the same
      ;; thing.)
      `([name          . ,name]
        [subtable-name . ,subtable-name]
        [lookup-name   . ,lookup-name]
        [lookup-type   . ,lookup-type]) ))

  (define (anchor-point-match? ac-ptr alist1 alist2)
    (zero? (anchor-point-compare ac-ptr alist1 alist2)))

  (define (anchor-point-compare ac-ptr alist1 alist2)
    "@code{anchor-point-compare} determines a relative order of
@var{alist} and @var{alist2}, returning a negative number, zero, or a
positive number, accordingly.

What ‘equality’ means is determined by the needs of font-making. What
‘less than’ means, on the other hand, is arbitrary and subject to
change."
    (let* ([name1 (anchor-point-name alist1)]
           [ac1 (checking-find-AnchorClass 'anchor-point-compare
                                           ac-ptr name1 (list alist1))]
           [anchor-class1 (AnchorClass->alist ac1)]
           [name2 (anchor-point-name alist2)]
           [ac2 (checking-find-AnchorClass 'anchor-point-compare
                                           ac-ptr name2 (list alist2))]
           [anchor-class2 (AnchorClass->alist ac2)])
      (string-compare
       (assq-ref anchor-class1 'lookup-name)
       (assq-ref anchor-class2 'lookup-name)
       (lambda (ignored) -1)
       (lambda (ignored)
         (string-compare
          (assq-ref anchor-class1 'subtable-name)
          (assq-ref anchor-class2 'subtable-name)
          (lambda (ignored) -1)
          (lambda (ignored)
            (string-compare
             (assq-ref anchor-class1 'name)
             (assq-ref anchor-class2 'name)
             (lambda (ignored) -1)
             (lambda (ignored)
               (match (assq-ref anchor-class1 'lookup-type)
                 ['gpos-mark-to-base 0]
                 ['gpos-mark-to-ligature (- (anchor-point-ligature-index alist1)
                                            (anchor-point-ligature-index alist2))]
                 [_ (- (anchor-point-type-index (anchor-point-type alist1))
                       (anchor-point-type-index (anchor-point-type alist2)))] ))
             (lambda (ignored) 1)))
          (lambda (ignored) 1)))
       (lambda (ignored) 1))))

  ;;-----------------------------------------------------------------------

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
  
  ;;-----------------------------------------------------------------------

  ) ;; end of library.
