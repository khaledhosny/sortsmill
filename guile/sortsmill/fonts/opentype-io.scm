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

(library (sortsmill fonts opentype-io)

  (export
   ;; (ot:at-port-position port position thunk) → result-of-thunk
   ;;
   ;;    Change @var{port}’s position to @var{position}, execute
   ;;    @var{thunk}, then restore the port’s original
   ;;    position. Return the results of @var{thunk}.
   ;;
   ;; (ot:at-port-position port #f thunk) → result-of-thunk
   ;;
   ;;    Simply execute @var{thunk} and return its results (ignoring
   ;;    @var{port}).
   ;;
   ot:at-port-position

   ot:read-BYTE         ; (ot:read-BYTE port [position]) → integer
   ot:read-CHAR         ; (ot:read-CHAR port [position]) → integer
   ot:read-USHORT       ; (ot:read-USHORT port [position]) → integer
   ot:read-SHORT        ; (ot:read-SHORT port [position]) → integer
   ot:read-UINT24       ; (ot:read-UINT24 port [position]) → integer
   ot:read-UFWORD       ; (ot:read-UFWORD port [position]) → integer
   ot:read-FWORD        ; (ot:read-FWORD port [position]) → integer
   ot:read-GlyphID      ; (ot:read-GlyphID port [position]) → integer
   ot:read-Offset       ; (ot:read-Offset port [position]) → integer
   ot:read-Tag          ; (ot:read-Tag port [position]) → integer
   ot:read-Fixed        ; (ot:read-FWORD port [position]) → rational
   ot:read-F2DOT14      ; (ot:read-F2DOT14 port [position]) → rational
   ot:read-string ; (ot:read-string-latin1 length port [position]) → bytevector
   )

  (import (sortsmill i18n)
          (rnrs)
          (except (guile) error))

  (define (ot:at-port-position port position thunk)
    (if position
        (let ([old-position (port-position port)])
          (dynamic-wind
            (lambda () (set-port-position! port position))
            thunk
            (lambda () (set-port-position! port old-position))))
        (thunk)))

  (define-syntax define-read-ot-fixed
    (lambda (stx)
      (syntax-case stx ()
        [(_ proc size fraction-bits bv-ref)
         (let ([sz (syntax->datum #'size)]
               [denominator (expt 2 (syntax->datum #'fraction-bits))])
           #`(define* (proc port #:optional position)
               (ot:at-port-position
                port position
                (lambda ()
                  (let ([bv (get-bytevector-n port #,sz)])
                    (if (= #,sz (bytevector-length bv))
                        (if (= 1 #,denominator)
                            #,(if (= 1 sz)
                                  #'(bv-ref bv 0)
                                  #'(bv-ref bv 0 (endianness big)))
                            (/ #,(if (= 1 sz)
                                     #'(bv-ref bv 0)
                                     #'(bv-ref bv 0 (endianness big)))
                               #,denominator))
                        (error (quote proc)
                               (_ "port input ended prematurely")
                               port)))))))] )))

  (define-syntax define-read-ot-integer
    (lambda (stx)
      (syntax-case stx ()
        [(_ proc size bv-ref)
         #'(define-read-ot-fixed proc size 0 bv-ref)] )))

  (define-read-ot-integer ot:read-BYTE 1 bytevector-u8-ref)
  (define-read-ot-integer ot:read-CHAR 1 bytevector-s8-ref)
  (define-read-ot-integer ot:read-USHORT 2 bytevector-u16-ref)
  (define-read-ot-integer ot:read-SHORT 2 bytevector-s16-ref)
  (define-read-ot-integer ot:read-ULONG 4 bytevector-u32-ref)
  (define-read-ot-integer ot:read-LONG 4 bytevector-s32-ref)
  (define ot:read-UFWORD ot:read-USHORT)
  (define ot:read-FWORD ot:read-SHORT)
  (define ot:read-GlyphID ot:read-USHORT)
  (define ot:read-Offset ot:read-USHORT)
  (define ot:read-Tag ot:read-ULONG)

  (define-read-ot-fixed ot:read-Fixed 4 16 bytevector-s32-ref)
  (define-read-ot-fixed ot:read-F2DOT14 2 14 bytevector-s16-ref)

  (define* (ot:read-UINT24 port #:optional position)
    (ot:at-port-position
     port position
     (lambda ()
       (let* ([bv (make-bytevector 4 0)]
              [n (get-bytevector-n! port bv 1 3)])
         (if (equal? 3 n)
             (bytevector-u32-ref bv 0 (endianness big))
             (error 'ot:read-UINT24
                    (_ "port input ended prematurely")
                    port))))))

  (define* (ot:read-string length port #:optional position)
    (ot:at-port-position
     port position
     (lambda () (get-bytevector-n port length))))

  ) ;; end of library.
