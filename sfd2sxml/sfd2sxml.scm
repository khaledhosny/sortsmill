;; -*- coding: utf-8 -*-

;; Copyright (C) 2012 Barry Schwartz
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

(use-modules
 (ice-9 match)
 (ice-9 optargs)
 (ice-9 rdelim)
 (ice-9 regex)
 )

;; FIXME: Get epsilon a better way, such as from the C library or GSL.
(define epsilon 2.2204460492503131e-16)

(define (fuzzy= a b)
  (<= (abs (- a b)) (* (max (abs a) (abs b)) epsilon)))

(define sfd-line-end-re
  (make-regexp "^[[:space:]]*$"))

(define sfd-keyword-re
  (make-regexp "^[[:space:]]*([[:alpha:]][[:alnum:]]*:?)"))

(define sfd-integer-re
  (make-regexp "^[[:space:]]*([-+]?[[:digit:]]+)"))

;; FIXME: Do some SFDs contain locale-specific numbers? (If so, ugh.)
(define sfd-real-re
  (make-regexp (string-append "^[[:space:]]*([-+]?"
                              "[[:digit:]]+\\.?[[:digit:]]*([Ee][-+]?[[:digit:]]+)?"
                              "|[-+]?\\.?[[:digit:]]+([Ee][-+]?[[:digit:]]+)?)")))

;; Read a line of input, with optional backslash-newline processing:
;;
;; (sfd-read-line [port] [#:continuation-allowed true-or-false])
;;
(define* (sfd-read-line #:optional (port (current-input-port))
                        #:key (continuation-allowed #t))
  (let ((line (read-line port)))
    (cond
     ((eof-object? line) line)
     ((and continuation-allowed (string-suffix? "\\" line))
      (string-append (string-drop-right line 1)
                     (sfd-read-line port #:continuation-allowed
                                    continuation-allowed)))
     (else line))))

;; Convert "SplineFontDB:" to 'splinefontdb, etc.
(define (sfd-string-to-symbol s)
  (let ((trimmed-s (if (string-suffix? ":" s)
                       (string-drop-right s 1)
                       s)))
    (string->symbol (string-downcase trimmed-s))))

(define (sfd-source-info port)
  (if (input-port? port)
      (list (port-filename port)
            (port-line port)
            (port-column port))
      #f))

;; Return either a list (keyword-symbol start-pos end-pos), or #f if
;; no keyword is found. Keyword symbols are objects like
;; 'splinefontdb, 'italicangle, etc.
(define* (sfd-get-keyword line #:key port (mandatory #t))
  (let ((m (regexp-exec sfd-keyword-re line)))
    (if m
        (list (sfd-string-to-symbol (match:substring m 1))
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'expected-a-keyword (sfd-source-info port))
            #f))))

;; Return either a list (real-value real-string start-pos end-pos), or
;; #f if no real number is found.
(define* (sfd-get-real line start #:key port (mandatory #t))
  (let ((m (regexp-exec sfd-real-re line start)))
    (if m
        (list (string->number (match:substring m 1))
              (match:substring m 1)
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'expected-a-real (sfd-source-info port))
            #f))))

;; Return either a list (integer-value integer-string start-pos
;; end-pos), or #f if no integer number is found.
(define* (sfd-get-integer line start #:key port (mandatory #t))
  (let ((m (regexp-exec sfd-integer-re line start)))
    (if m
        (list (string->number (match:substring m 1))
              (match:substring m 1)
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'expected-an-integer (sfd-source-info port))
            #f))))

(define* (sfd-get-line-end line start #:key port (mandatory #t))
  (if (regexp-exec sfd-line-end-re line start)
      #t
      (if mandatory
          (throw 'expected-end-of-line (sfd-source-info port))
          #f)))

(define* (sfd-check-version version #:key port)
  (if (or
       (fuzzy= 1.0 version)
       (fuzzy= 2.0 version)
       (fuzzy= 3.0 version))
      '()
      (throw 'unrecognized-sfd-version (list version (sfd-source-info port)))))

(define sfd-empty-contents
  (list (list 'fields)))

;; Add a field to the @code{'fields} list by consing.
(define (sfd-add-field contents key value)
  (let ((old-fields (cdr (assq 'fields contents))))
    (assq-set! contents 'fields (cons (list key value) old-fields))))

;; @code{sfd-add-field} makes the @code{'fields} list by consing, so
;; reverse it to get the original order.
(define (sfd-reverse-fields contents)
  (let ((old-fields (cdr (assq 'fields contents))))
    (assq-set! contents 'fields (reverse old-fields))))

(define* (sfd-read-real-field contents key line start #:key port)
  (let ((field-str
         (match (sfd-get-real line start #:port port)
                ((_ str _ end)
                 (sfd-get-line-end line end #:port port)
                 str))))
    (sfd-add-field contents key field-str)))

(define* (sfd-read-integer-field contents key line start #:key port)
  (let ((field-str
         (match (sfd-get-integer line start #:port port)
                ((_ str _ end)
                 (sfd-get-line-end line end #:port port)
                 str))))
    (sfd-add-field contents key field-str)))

(define* (sfd-read-contents port version
                            #:optional (contents sfd-empty-contents))
  (let ((line (sfd-read-line port)))
    (if (eof-object? line)
        (sfd-reverse-fields contents)
        (match (sfd-get-keyword line #:port port
                                #:mandatory #f) ;; FIXME: this is temporary.
               (((and
                  (or 'italicangle
                      'strokewidth
                      'tilemargin
                      'underlineposition
                      'underlinewidth
                      'cidversion
                      'ufoascent
                      'ufodescent)
                  key) start end)
                (sfd-read-contents port version
                                   (sfd-read-real-field contents
                                                        key line end
                                                        #:port port)))

               (((and
                  (or 'ascent
                      'descent

                      'hheadascent
                      'hheadaoffset
                      'hheaddescent
                      'hheaddoffset
                      'os2typoascent
                      'os2typoaoffset
                      'os2typodescent
                      'os2typodoffset
                      'os2winascent
                      'os2winaoffset
                      'os2windescent
                      'os2windoffset

                      'os2subxsize
                      'os2subysize
                      'os2subxoff
                      'os2subyoff
                      'os2supxsize
                      'os2supysize
                      'os2supxoff
                      'os2supyoff
                      'os2strikeysize
                      'os2strikeypos
                      )
                  key) start end)
                (sfd-read-contents port version
                                   (sfd-read-integer-field contents
                                                           key line end
                                                           #:port port)))

               (_ (sfd-read-contents port version contents)) ;; FIXME: this is temporary.
               )
        )))

(define* (sfd-read #:optional (port (current-input-port)))
  (let ((line (sfd-read-line port)))
    (match (sfd-get-keyword line #:port port)
           (('splinefontdb start end)
            (match (sfd-get-real line end #:port port)
                   ((version version-string start end)
                    (sfd-get-line-end line end #:port port)
                    (sfd-check-version version #:port port)
                    (let ((contents (sfd-read-contents port version)))
                      (cons 'splinefontdb
                            (cons (list '@ (list 'version version-string))
                                  contents))))))

           (_ (throw 'expected-sfd (sfd-source-info port))))))




(use-modules (ice-9 pretty-print))

(with-input-from-file "Fanwood-Italic.sfd"
  (lambda () (pretty-print (sfd-read))))
