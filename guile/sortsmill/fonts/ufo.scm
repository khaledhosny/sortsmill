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

(library (sortsmill fonts ufo)

  (export ufo:read-metainfo
          ufo:read-fontinfo
          ufo:read-groups
          ufo:read-kerning
          ufo:read-lib
          ufo:read-layercontents
          ufo:read-contents
          ufo:read-layerinfo
          ufo:read-glif

          view:apply-ufo-fontinfo!
          )

  (import (sortsmill i18n)
          (sortsmill fonts fontinfo-dict)
          (sortsmill fonts general)
          (sortsmill fonts head-table)
          (sortsmill fonts hhea-table)
          (sortsmill fonts os2-table)
          (sortsmill fonts private-dict)
          (sortsmill fonts t1font-dict)
;;;;;;;;;;;;;;          (sortsmill strings rexp)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (only (srfi :60) copy-bit)
          (sxml simple)
          (sxml match)
          (ice-9 match)
          (ice-9 format))

  (define (ufo:read-metainfo ufo)
    (parse-dict-xml ufo "metainfo.plist"))

  (define (ufo:read-fontinfo ufo)
    (parse-dict-xml ufo "fontinfo.plist"))

  (define (ufo:read-groups ufo)
    (parse-dict-xml ufo "groups.plist"))

  (define (ufo:read-kerning ufo)
    (parse-dict-xml ufo "kerning.plist"))

  (define (ufo:read-lib ufo)
    (parse-dict-xml ufo "lib.plist"))

  (define (ufo:read-layercontents ufo)
    (parse-array-xml ufo "layercontents.plist"))

  (define (ufo:read-contents ufo)
    (parse-dict-xml ufo "glyphs" "contents.plist"))

  (define (ufo:read-layerinfo ufo)
    (parse-dict-xml ufo "glyphs" "layerinfo.plist"))

  (define (ufo:read-glif ufo glif-file)
    (parse-ufo-xml ufo "glyphs" glif-file))

  (define (view:apply-ufo-fontinfo! view ufo-or-fontinfo)
    (let* ([fontinfo (if (string? ufo-or-fontinfo)
                         (let ([info (ufo:read-fontinfo ufo-or-fontinfo)])
                           (if (eq? info 'file-not-found) '() info))
                         ufo-or-fontinfo)]
           [version-major (assoc-ref fontinfo 'versionMajor)]
           [version-minor (assoc-ref fontinfo 'versionMinor)]
           [version (if version-major
                        (if version-minor
                            (format #f "~a.~a" version-major version-minor)
                            (format #f "~a" version-major))
                        #f)]
           [ascender (assoc-ref fontinfo 'ascender)]
           [descender (assoc-ref fontinfo 'descender)]
           [unitsPerEm (assoc-ref fontinfo 'unitsPerEm)])
      (when version (view:fontinfo-dict-set! view "version" version))
      (let-values ([(ascent descent) (compute-vertical-layout
                                      'view:apply-ufo-fontinfo!
                                      ascender descender unitsPerEm)])
        (view:ascent-set! view ascent)
        (view:descent-set! view descent))
      (for-each
       (lambda (entry)
         (apply-ufo-fontinfo-entry! view (car entry) (cdr entry)))
       fontinfo)))

  (define (compute-vertical-layout who ascender descender unitsPerEm)
    ;; Given the UFO documentation, such as it is at the time of this
    ;; writing, it is unclear how to compute the ascent, descent, and
    ;; units-per-em. Below is an attempt to make reasonable
    ;; inferences.
    (define (unable-to-infer-units-per-em who)
      (error who (_ "I do not know how to infer the units-per-em for this UFO")))
    (define (infer-from-units-per-em units-per-em ratio)
      (let ([ascent (inexact->exact (round (* ratio units-per-em)))])
        (values ascent (- units-per-em ascent))))
    (match (list ascender descender unitsPerEm)
      [(#f #f #f) (unable-to-infer-units-per-em who)]
      [(_ #f #f)  (unable-to-infer-units-per-em who)]
      [(#f _ #f)  (unable-to-infer-units-per-em who)]
      [(#f #f _)  (infer-from-units-per-em unitsPerEm 800/1000)]
      [(_ _ #f)   (values ascender (- descender))]
      [(_ #f _)   (values ascender (- unitsPerEm ascender))]
      [(#f _ _)   (values (+ unitsPerEm descender) (- descender))]
      [(_ _ _)    (if (= unitsPerEm (- ascender descender))
                      (values ascender (- descender))
                      (let ([ratio (/ ascender (- ascender descender))])
                        (infer-from-units-per-em unitsPerEm ratio)))] ))

  (define (parse-array-xml ufo . components)
    (match (apply parse-ufo-xml ufo components)
      ['file-not-found 'file-not-found]
      [lst (isolated-array lst)] ))

  (define (parse-dict-xml ufo . components)
    (match (apply parse-ufo-xml ufo components)
      ['file-not-found 'file-not-found]
      [lst (isolated-dict lst)] ))

  (define (isolated-array lst)
    (let ([obj (isolated-element lst)])
      (if (list? obj)
          obj
          (error 'isolated-array (_ "expected an <array>") obj))))

  (define (isolated-dict lst)
    (let ([obj (isolated-element lst)])
      (if (ufo-dict? obj)
          obj
          (error 'isolated-dict (_ "expected a <dict>") obj))))

  (define (isolated-element lst)
    (match lst
      [(element) element]
      [_ (error 'one-toplevel-element
                (_ "expected one and only one element")
                lst)] ))

  (define (ufo-dict? obj)
    (and (list? obj)
         (for-all (lambda (p) (and (pair? p) (symbol? (car p)))) obj)))

  (define (parse-ufo-xml ufo . components)
    (let ([sxml (apply read-ufo-xml ufo components)])
      (if sxml (match-ufo-element sxml) 'file-not-found)))

  (define (read-ufo-xml ufo . components)
    (let ([file-name (apply ufo-file-name ufo components)])
      (if (file-exists? file-name)
          (call-with-input-file file-name
            (cut xml->sxml <> #:trim-whitespace? #t))
          #f)))

  (define (match-ufo-element expr)
    (sxml-match expr
      [(*TOP* (*PI* ,pi ...) ,[e]) e]
      [(*TOP* ,[e]) e]
      [(plist ,[e] ...) (list e ...)]
      [(string ,s) s]
      [(string) ""]
      [(integer ,n) (string->number n)]
      [(real ,x) (string->number x)]
      [(true) #t]
      [(false) #f]
      [(array ,[e] ...) (list e ...)]
      [(dict . ,pairs) (match-dict-pairs pairs)]
      [(glyph (@ (name ,n) (format ,f)) ,[e] ...)
       (list (cons 'name n)
             (cons 'format (string->number f))
             e ...)]
      [(advance (@ (width (,w "0")) (height (,h "0"))))
       (list 'advance (string->number w) (string->number h))]
      [(unicode (@ (hex ,x))) (cons 'unicode (string->number x 16))]
      [(note) (cons 'note "")]
      [(note ,n) (cons 'note n)]
      [(image (@ (fileName ,fn)
                 (xScale (,xscale "1"))
                 (xyScale (,xyscale "0"))
                 (yxScale (,yxscale "0"))
                 (yScale (,yscale "1"))
                 (xOffset (,xoffset "0"))
                 (yOffset (,yoffset "0"))
                 (color (,c #f))))
       (list 'image
             (cons 'psmat (make-psmat xscale xyscale yxscale yscale
                                      xoffset yoffset))
             (cons 'color c))]
      [(guideline (@ (x ,x) (y ,y) (angle ,angle)
                     (name (,n #f))
                     (color (,c #f))
                     (identifier (,id #f))))
       (list 'guideline
             (cons 'x (string->number x))
             (cons 'y (string->number y))
             (cons 'angle angle)
             (cons 'name n)
             (cons 'color c)
             (cons 'identifier id))]
      [(anchor (@ (x ,x) (y ,y)
                  (name (,n #f))
                  (color (,c #f))
                  (identifier (,id #f))))
       (list 'anchor
             (cons 'x (string->number x))
             (cons 'y (string->number y))
             (cons 'name n)
             (cons 'color c)
             (cons 'identifier id))]
      [(outline ,[e] ...) (list 'outline e ...)]
      [(component (@ (base ,base)
                     (xScale (,xscale "1"))
                     (xyScale (,xyscale "0"))
                     (yxScale (,yxscale "0"))
                     (yScale (,yscale "1"))
                     (xOffset (,xoffset "0"))
                     (yOffset (,yoffset "0"))
                     (identifier (,id #f))))
       (list 'component
             (cons 'base base)
             (cons 'psmat (make-psmat xscale xyscale yxscale yscale
                                      xoffset yoffset))
             (cons 'identifier id))]
      [(contour (@ (identifier (,id #f))) ,[e] ...)
       (list 'contour (cons 'identifier id) e ...)]
      [(point (@ (x ,x) (y ,y)
                 (type (,type "offcurve"))
                 (smooth (,smooth "no"))
                 (name (,n #f))))
       (list 'point
             (cons 'x (string->number x))
             (cons 'y (string->number y))
             (cons 'type (string->point-type type))
             (cons 'smooth (yes-no-string->boolean smooth))
             (cons 'name n))]
      [(lib ,[e]) (cons 'lib e)]
      [,other (error 'match-ufo-element
                     (_ "unexpected SXML")
                     other)] ))

  (define (string->point-type type)
    (match type
      ["move" 'move]
      ["line" 'line]
      ["offcurve" 'offcurve]
      ["curve" 'curve]
      ["qcurve" 'qcurve]
      [other (error 'string->point-type
                    (_ "unexpected curve type")
                    other)] ))

  (define (yes-no-string->boolean s)
    (match s
      ["yes" #t]
      ["no"  #f]
      [other (error 'yes-no-string->boolean
                    (_ "expected `yes' or `no'")
                    other)] ))

  (define (make-psmat xscale xyscale yxscale yscale xoffset yoffset)
    (list (string->number xscale)
          (string->number xyscale)
          (string->number yxscale)
          (string->number yscale)
          (string->number xoffset)
          (string->number yoffset)))

  (define* (match-dict-pairs pairs #:optional [prior '()])
    (sxml-match pairs
      [(list) (reverse prior)]
      [(list (key ,k) ,e . ,more-pairs)
       (match-dict-pairs
        more-pairs
        (acons (string->symbol k) (match-ufo-element e) prior))]
      [,other (error 'match-dict-pairs
                     (_ "unexpected SXML")
                     other)] ))

  (define (ufo-file-name ufo . components)
    (string-join (cons ufo components) "/"))

  (define (apply-ufo-fontinfo-entry! view key value)
    (match key
      ['note (view:font-comment-set! view value)]
      ['postscriptFontName (view:t1font-dict-set! view "FontName" value)]
      ['fontName (view:t1font-dict-set! view "FontName" value)]
      ['postscriptFullName (view:fontinfo-dict-set! view "FullName" value)]
      ['fullName (view:fontinfo-dict-set! view "FullName" value)]
      ['familyName (view:fontinfo-dict-set! view "FamilyName" value)]
      ['postscriptWeightName (view:fontinfo-dict-set! view "Weight" value)]
      ['weightName (view:fontinfo-dict-set! view "Weight" value)]
      ['postscriptIsFixedPitch (view:fontinfo-dict-set! view "IsFixedPitch" value)]
      ['postscriptUnderlinePosition (view:fontinfo-dict-set! view "UnderlinePosition" value)]
      ['postscriptUnderlineThickness (view:fontinfo-dict-set! view "UnderlineThickness" value)]
      ['copyright (view:fontinfo-dict-set! view "Notice" value)]
      ['italicAngle (view:fontinfo-dict-set! view "ItalicAngle" value)]
      ['postscriptBlueValues (view:private-dict-set! view "BlueValues" value)]
      ['postscriptOtherBlues (view:private-dict-set! view "OtherBlues" value)]
      ['postscriptFamilyBlues (view:private-dict-set! view "FamilyBlues" value)]
      ['postscriptFamilyOtherBlues (view:private-dict-set! view "FamilyOtherBlues" value)]
      ['postscriptStemSnapH (view:private-dict-set! view "StemSnapH" value)]
      ['postscriptStemSnapV (view:private-dict-set! view "StemSnapV" value)]
      ['postscriptBlueFuzz (view:private-dict-set! view "BlueFuzz" value)]
      ['postscriptBlueShift (view:private-dict-set! view "BlueShift" value)]
      ['postscriptBlueScale (view:private-dict-set! view "BlueScale" value)]
      ['postscriptForceBold (view:private-dict-set! view "ForceBold" value)]
      ['openTypeOS2WidthClass (view:os2-table-set! view "usWidthClass" value)]
      ['openTypeOS2WeightClass (view:os2-table-set! view "usWeightClass" value)]
      ['openTypeOS2VendorID (view:os2-table-set! view "achVendID" value)]
      ['openTypeOS2Panose (view:os2-table-set! view "panose" value)]
      ['openTypeOS2FamilyClass (view:os2-table-set! view "sFamilyClass"
                                                    (+ (* 256 (car value)) (cadr value)))]
      ['openTypeOS2UnicodeRanges (view:os2-table-set! view "ulUnicodeRange"
                                                      (bit-numbers->integers value 32 4))]
      ['openTypeOS2CodePageRanges (view:os2-table-set! view "ulCodePageRange"
                                                       (bit-numbers->integers value 32 2))]
      ['openTypeOS2TypoAscender (view:os2-table-set! view "sTypoAscender" value)]
      ['openTypeOS2TypoDescender (view:os2-table-set! view "sTypoDescender" value)]
      ['openTypeOS2TypoLineGap (view:os2-table-set! view "sTypoLineGap" value)]
      ['openTypeOS2WinAscent (view:os2-table-set! view "usWinAscent" value)]
      ['openTypeOS2WinDescent (view:os2-table-set! view "usWinDescent" value)]
      ['openTypeOS2Type (view:os2-table-set! view "fsType" (bit-numbers->integer value 16))]
      ['openTypeOS2SubscriptXSize (view:os2-table-set! view "ySubscriptXSize" value)]
      ['openTypeOS2SubscriptYSize (view:os2-table-set! view "ySubscriptYSize" value)]
      ['openTypeOS2SubscriptXOffset (view:os2-table-set! view "ySubscriptXOffset" value)]
      ['openTypeOS2SubscriptYOffset (view:os2-table-set! view "ySubscriptYOffset" value)]
      ['openTypeOS2SuperscriptXSize (view:os2-table-set! view "ySuperscriptXSize" value)]
      ['openTypeOS2SuperscriptYSize (view:os2-table-set! view "ySuperscriptYSize" value)]
      ['openTypeOS2SuperscriptXOffset (view:os2-table-set! view "ySuperscriptXOffset" value)]
      ['openTypeOS2SuperscriptYOffset (view:os2-table-set! view "ySuperscriptYOffset" value)]
      ['openTypeOS2StrikeoutSize (view:os2-table-set! view "yStrikeoutSize" value)]
      ['openTypeOS2StrikeoutPosition (view:os2-table-set! view "yStrikeoutPosition" value)]
      ['openTypeHheaAscender (view:hhea-table-set! view "Ascender" value)]
      ['openTypeHheaDescender (view:hhea-table-set! view "Descender" value)]
      ['openTypeHheaLineGap (view:hhea-table-set! view "LineGap" value)]
      ['openTypeHeadFlags (view:head-table-set! view "flags" (bit-numbers->integer value 16))]
      ['openTypeHeadCreated (view:head-table-set! view "created"
                                                  (YYYY/MM/DD_HH:MM:SS->unix-time value))]
      [_ *unspecified*] ))

  (define (bit-numbers->integer bit-numbers word-size)
    (car (bit-numbers->integers bit-numbers word-size 1)))

  (define (bit-numbers->integers bit-numbers word-size integer-count)
    (let ([ints (make-vector integer-count 0)])
      (for-each
       (lambda (bitnum)
         ;; Ignore out-of-range bit numbers.
         (when (< -1 bitnum (* word-size integer-count))
           (let-values ([(word-num bit-in-word) (div-and-mod bitnum word-size)])
             (vector-set! ints word-num
                          (copy-bit bit-in-word
                                    (vector-ref ints word-num)
                                    #t)))))
       bit-numbers)
      (vector->list ints)))

  (define (YYYY/MM/DD_HH:MM:SS->unix-time s)
    (let ([tm (strptime "%Y/%m/%d %H:%M:%S" s)])
      (car (mktime (car tm)))))

  ) ;; end of library.
