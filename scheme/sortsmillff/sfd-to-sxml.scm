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

(define-module (sortsmillff sfd-to-sxml)
  #:export (sfd->sxml sfd-error-message)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (sortsmillff iconv))

;; FIXME: Get epsilon a better way, such as from the C library or GSL.
(define epsilon 2.2204460492503131e-16)

(define (fuzzy= a b)
  (<= (abs (- a b)) (* (max (abs a) (abs b)) epsilon)))

(define (remove-embedded-nul-chars s)
  (list->string (filter (lambda (c) (not (char=? c #\nul)))
                        (string->list s))))

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

;; A string going to the end of the line, with leading and trailing
;; space trimmed away.
(define sfd-string-to-eol-re (make-regexp "^[[:space:]]*(.*[^[:space:]]|)"))

;; A UTF-7 string surrounded by double quotes.
(define sfd-utf7-string-re (make-regexp "^[[:space:]]*\"([^\"]*)\""))

;; Convert "SplineFontDB:" to 'splinefontdb, etc.
(define (sfd-string-to-symbol s)
  (let ((trimmed-s (if (string-suffix? ":" s)
                       (string-drop-right s 1)
                       s)))
    (string->symbol (string-downcase trimmed-s))))

(define (sfd-source-info port column)
  (if (input-port? port)
      (list (port-filename port)
            (port-line port)
            column)
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
            (throw 'sfd-error "expected a keyword"
                   (sfd-source-info port 0))
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
            (throw 'sfd-error "expected a real"
                   (sfd-source-info port start))
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
            (throw 'sfd-error "expected an integer"
                   (sfd-source-info port start))
            #f))))

;; Return a list (string start-pos end-pos). The string has leading
;; and trailing spaces trimmed. (NOTE: I believe the old FontForge
;; parser does not trim trailing spaces, so you might get different
;; results in some cases. But SFD was supposed to be safe for e-mail,
;; I believe, and that means no significant trailing spaces.) If the
;; string is empty, start-pos and end-pos will both indicate the end
;; of the line.
(define* (sfd-get-string-to-eol line start #:key port (mandatory #f))
  (let ((m (regexp-exec sfd-string-to-eol-re line start)))
    (list (match:substring m 1)
          (match:start m 1)
          (match:end m 1))))

(define* (sfd-get-utf7-string line start #:key port (mandatory #f))
  (let ((m (regexp-exec sfd-utf7-string-re line start)))
    (if m
        (list (remove-embedded-nul-chars
               (embedded-utf7->string (match:substring m 1)))
              (match:substring m 1)
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'sfd-error "expected a string in UTF-7 encoding"
                   (sfd-source-info port start))
            #f))))

(define* (sfd-get-line-end line start #:key port (mandatory #t))
  (if (regexp-exec sfd-line-end-re line start)
      #t
      (if mandatory
          (throw 'sfd-error "expected end of line"
                 (sfd-source-info port start))
          #f)))

;; SFD version 4.0 is not supported, due to prejudice against it.
(define* (sfd-check-version version #:key port column)
  (if (or
       (fuzzy= 1.0 version)
       (fuzzy= 2.0 version)
       (fuzzy= 3.0 version))
      '()
      (throw 'sfd-error
             (string-append "unrecognized sfd version ("
                            (number->string version) ")")
             (sfd-source-info port column))))

;; Add an entry to the contents list by consing.
(define (sfd-add-entry contents key value)
  (cons (list key value) contents))

(define* (sfd-read-real-entry contents key line start #:key port)
  (let ((entry-str
         (match (sfd-get-real line start #:port port)
                ((_ str _ end)
                 (sfd-get-line-end line end #:port port)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define* (sfd-read-integer-entry contents key line start #:key port)
  (let ((entry-str
         (match (sfd-get-integer line start #:port port)
                ((_ str _ end)
                 (sfd-get-line-end line end #:port port)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define* (sfd-read-string-to-eol-entry contents key line start #:key port)
  (let ((entry-str
         (match (sfd-get-string-to-eol line start #:port port)
                ((str _ end)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define* (sfd-read-utf7-string-entry contents key line start #:key port)
  (let ((entry-str
         (match (sfd-get-utf7-string line start #:port port)
                ((utf8-str _ _ end)
                 (sfd-get-line-end line (+ end 1) #:port port)
                 utf8-str))))
    (sfd-add-entry contents key entry-str)))

;; Replace \n with a newline and \\ with a single backslash.
;; The current implementation drops all other backslashes.
(define (sfd-process-escapes s)
  (let ((i (string-index  s #\\)))
    (if i
        (string-append
         (string-take s i)
         (let ((s1 (string-drop s (+ i 1))))
           (cond
            ((string-prefix? "n" s1)
             (string-append "\n" (sfd-process-escapes
                                  (string-drop s1 1))))
            ((string-prefix? "\\" s1)
             (string-append "\\" (sfd-process-escapes
                                  (string-drop s1 1))))
            (else (sfd-process-escapes s1)))))
        s)))

(define* (sfd-read-escaped-string-to-eol-entry contents key line start
                                               #:key port)
  (let ((entry-str
         (match (sfd-get-string-to-eol line start #:port port)
                ((str _ end)
                 (sfd-process-escapes str)))))
    (sfd-add-entry contents key entry-str)))

(define (sfd-update-contents contents line port version)
  (match
   (sfd-get-keyword line #:port port
                    #:mandatory #f) ;; FIXME: this is temporary.

   ;; Reals.
   (((and
      (or 'italicangle 'strokewidth 'tilemargin 'underlineposition
          'underlinewidth 'cidversion 'ufoascent 'ufodescent)
      key)
     start end)
    (sfd-read-real-entry contents key line end #:port port))

   ;; Integers.
   (((and
      (or 'ascent 'descent 'hheadascent 'hheadaoffset 'hheaddescent
          'hheaddoffset 'os2typoascent 'os2typoaoffset 'os2typodescent
          'os2typodoffset 'os2winascent 'os2winaoffset 'os2windescent
          'os2windoffset 'os2subxsize 'os2subysize 'os2subxoff 'os2subyoff
          'os2supxsize 'os2supysize 'os2supxoff 'os2supyoff 'os2strikeysize
          'os2strikeypos 'antialias 'displaylayer 'displaysize 'extremabound
          'fittoem 'isextendedshape 'linegap 'macstyle 'onlybitmaps
          'pfmfamily 'pfmweight 'topencoding 'ttfweight 'ttfwidth 'vlinegap
          'widthseparation 'woffmajor 'woffminor
          'os2version 'fstype)
      key)
     start end)
    (sfd-read-integer-entry contents key line end #:port port))

   ;; Strings.
   (((and
      (or 'familyname 'fontname 'fullname 'weight 'defaultbasefilename
          'version 'fondname )
      key)
     start end)
    (sfd-read-string-to-eol-entry contents key line end #:port port))

   ;; Strings with escaped newlines: \n
   (((and (or 'copyright 'comments) key)
     start end)
    (sfd-read-escaped-string-to-eol-entry contents key line end
                                          #:port port))

   ;; UTF-7 strings.
   (((and (or 'comment 'ucomments 'fontlog 'woffmetadata) key)
     start end)
    (sfd-read-utf7-string-entry contents key line end #:port port))

   ;; Everything else.
   (_ contents) ;; FIXME: this is temporary.
   ))

(define* (sfd-read-contents port version #:optional (contents '()))
  (let ((line (read-line port)))
    (if (eof-object? line)
        (reverse contents)
        (sfd-read-contents
         port version (sfd-update-contents contents line port version)))))

(define* (sfd->sxml #:optional (port (current-input-port)))
  (let ((line (read-line port)))
    (match
     (sfd-get-keyword line #:port port)
     (('splinefontdb start end)
      (match (sfd-get-real line end #:port port)
             ((version version-string start end)
              (sfd-get-line-end line end #:port port)
              (sfd-check-version version #:port port #:column start)
              (let ((contents (sfd-read-contents port version)))
                (list '*TOP*
                      '(*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
                      (cons 'splinefontdb
                            (cons (list '@ (list 'version version-string))
                                  contents)))))))

     (_ (throw 'sfd-error "expected a Spline Font Database (SFD) file"
               (sfd-source-info port 0))))))

(define (sfd-error-message msg source-info)
  (let ((location (match source-info
                         ((#f line column)
                          (simple-format #f "line ~A, column ~A"
                                         (number->string line)
                                         (number->string (+ column 1))))
                         ((filename line column)
                          (simple-format #f "~S, line ~A, column ~A"
                                         filename
                                         (number->string line)
                                         (number->string (+ column 1)))))))
    (simple-format #f "~A: ~A" location msg)))
