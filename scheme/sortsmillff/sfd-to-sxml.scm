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
  #:use-module (sortsmillff i18n)
  #:use-module (sortsmillff iconv))

;; FIXME: Get epsilon a better way, such as from the C library or GSL.
(define epsilon 2.2204460492503131e-16)

(define (fuzzy= a b)
  (<= (abs (- a b)) (* (max (abs a) (abs b)) epsilon)))

(define (remove-embedded-nul-chars s)
  (list->string (filter (lambda (c) (not (char=? c #\nul)))
                        (string->list s))))

(define (skip-spaces str i)
  (let ((m (regexp-exec sfd-spaces-re str i)))
    (match:end m 0)))

(define sfd-spaces-re (make-regexp "^[[:space:]]*"))

(define sfd-line-end-re (make-regexp "^[[:space:]]*$"))

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

;; REs for multiline strings.
(define sfd-full-quoted-string-re
  (make-regexp "^[[:space:]]*\"(([^\\]|\\\\.)*)\""))
(define sfd-start-quoted-string-re
  (make-regexp "^[[:space:]]*\"(([^\\]|\\\\.)*\\\\?)$"))
(define sfd-end-quoted-string-re
  (make-regexp "^(([^\\]|\\\\.)*)\""))

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

(define (continue-multiline-string string-so-far port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        (throw 'sfd-error (_ "unexpected end of file")
               (sfd-source-info port line 1)) ; FIXME: Is the location
                                              ; here what we want?
        (let ((m1 (regexp-exec sfd-end-quoted-string-re line)))
          (if m1
              (list line
                    (sfd-process-escapes
                     (string-append string-so-far "\n"
                                    (match:substring m1 1)))
                    1 (match:end m1 1))
              (continue-multiline-string
               (string-append string-so-far "\n" line) port))))))

(define* (sfd-get-multiline-string line start port #:key (mandatory #t))
  (let ((m1 (regexp-exec sfd-full-quoted-string-re line start)))
    (if m1
        (list line
              (sfd-process-escapes (match:substring m1 1))
              (match:start m1 1)
              (match:end m1 1))
        (let ((m2 (regexp-exec sfd-start-quoted-string-re line start)))
          (if m2
              (continue-multiline-string (match:substring m2 1) port)
              (if mandatory
                  (throw 'sfd-error (_ "expected a multiline string")
                         (sfd-source-info port (skip-spaces line start)))
                  #f))))))

;; Return either a list (keyword-symbol start-pos end-pos), or #f if
;; no keyword is found. Keyword symbols are objects like
;; 'splinefontdb, 'italicangle, etc.
(define* (sfd-get-keyword line port #:key (mandatory #t))
  (let ((m (regexp-exec sfd-keyword-re line)))
    (if m
        (list (sfd-string-to-symbol (match:substring m 1))
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'sfd-error (_ "expected a keyword")
                   (sfd-source-info port (skip-spaces line 0)))
            #f))))

;; Return either a list (real-value real-string start-pos end-pos), or
;; #f if no real number is found.
(define* (sfd-get-real line start port #:key (mandatory #t))
  (let ((m (regexp-exec sfd-real-re line start)))
    (if m
        (list (string->number (match:substring m 1))
              (match:substring m 1)
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'sfd-error (_ "expected a real")
                   (sfd-source-info port (skip-spaces line start)))
            #f))))

;; Return either a list (integer-value integer-string start-pos
;; end-pos), or #f if no integer number is found.
(define* (sfd-get-integer line start port #:key (mandatory #t))
  (let ((m (regexp-exec sfd-integer-re line start)))
    (if m
        (list (string->number (match:substring m 1))
              (match:substring m 1)
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'sfd-error (_ "expected an integer")
                   (sfd-source-info port (skip-spaces line start)))
            #f))))

;; Return a list (string start-pos end-pos). The string has leading
;; and trailing spaces trimmed. (NOTE: I believe the old FontForge
;; parser does not trim trailing spaces, so you might get different
;; results in some cases. But SFD was supposed to be safe for e-mail,
;; I believe, and that means no significant trailing spaces.) If the
;; string is empty, start-pos and end-pos will both indicate the end
;; of the line.
(define* (sfd-get-string-to-eol line start port #:key (mandatory #f))
  (let ((m (regexp-exec sfd-string-to-eol-re line start)))
    (list (match:substring m 1)
          (match:start m 1)
          (match:end m 1))))

(define* (sfd-get-utf7-string line start port #:key (mandatory #f))
  (let ((m (regexp-exec sfd-utf7-string-re line start)))
    (if m
        (list (remove-embedded-nul-chars
               (embedded-utf7->string (match:substring m 1)))
              (match:substring m 1)
              (match:start m 1)
              (match:end m 1))
        (if mandatory
            (throw 'sfd-error (_ "expected a string in UTF-7 encoding")
                   (sfd-source-info port (skip-spaces line start)))
            #f))))

(define* (sfd-get-line-end line start port #:key (mandatory #t))
  (if (regexp-exec sfd-line-end-re line start)
      #t
      (if mandatory
          (throw 'sfd-error (_ "expected end of line")
                 (sfd-source-info port (skip-spaces line start)))
          #f)))

;; SFD version 4.0 is not supported, due to prejudice against it.
(define (sfd-check-version version port line column)
  (if (or
       (fuzzy= 1.0 version)
       (fuzzy= 2.0 version)
       (fuzzy= 3.0 version))
      '()
      (throw 'sfd-error
             (string-append (_ "unrecognized SFD version (")
                            (number->string version) ")")
             (sfd-source-info port (skip-spaces line column)))))

;; Add an entry to the contents list by consing.
(define (sfd-add-entry contents key value)
  (cons (list key value) contents))

(define (sfd-read-multiline-string-entry contents key line start
                                         port)
  (let ((entry-str
         (match (sfd-get-multiline-string line start port)
                ((line str _ end)
                 (sfd-get-line-end line (+ 1 end) port)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define (sfd-read-real-entry contents key line start port)
  (let ((entry-str
         (match (sfd-get-real line start port)
                ((_ str _ end)
                 (sfd-get-line-end line end port)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define (sfd-read-integer-entry contents key line start port)
  (let ((entry-str
         (match (sfd-get-integer line start port)
                ((_ str _ end)
                 (sfd-get-line-end line end port)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define (sfd-read-string-to-eol-entry contents key line start port)
  (let ((entry-str
         (match (sfd-get-string-to-eol line start port)
                ((str _ end)
                 str))))
    (sfd-add-entry contents key entry-str)))

(define (sfd-read-utf7-string-entry contents key line start port)
  (let ((entry-str
         (match (sfd-get-utf7-string line start port)
                ((utf8-str _ _ end)
                 (sfd-get-line-end line (+ end 1) port)
                 utf8-str))))
    (sfd-add-entry contents key entry-str)))

;; Replace \n with a newline, \" with a double quote, and \\ with a
;; single backslash.  The current implementation drops all other
;; backslashes.
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
            ((string-prefix? "\"" s1)
             (string-append "\"" (sfd-process-escapes
                                  (string-drop s1 1))))
            (else (sfd-process-escapes s1)))))
        s)))

(define (sfd-read-escaped-string-to-eol-entry contents key line start
                                              port)
  (let ((entry-str
         (match (sfd-get-string-to-eol line start port)
                ((str _ end)
                 (sfd-process-escapes str)))))
    (sfd-add-entry contents key entry-str)))

(define (sfd-update-contents contents line port version)
  (match
   (sfd-get-keyword line port
                    #:mandatory #f) ;; FIXME: this is temporary.

   ;; Reals.
   (((and
      (or 'italicangle 'strokewidth 'tilemargin 'underlineposition
          'underlinewidth 'cidversion 'ufoascent 'ufodescent)
      key)
     start end)
    (sfd-read-real-entry contents key line end port))

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
    (sfd-read-integer-entry contents key line end port))

   ;; Strings.
   (((and
      (or 'familyname 'fontname 'fullname 'weight 'defaultbasefilename
          'version 'fondname )
      key)
     start end)
    (sfd-read-string-to-eol-entry contents key line end port))

   ;; Strings with escaped newlines: \n
   (((and (or 'copyright 'comments) key)
     start end)
    (sfd-read-escaped-string-to-eol-entry contents key line end
                                          port))

   ;; UTF-7 strings.
   (((and (or 'comment 'ucomments 'fontlog 'woffmetadata) key)
     start end)
    (sfd-read-utf7-string-entry contents key line end port))

   ;; Pickled Python data.
   (((and 'pickleddata key) start end)
    (sfd-read-multiline-string-entry contents key line end port))

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
     (sfd-get-keyword line port)
     (('splinefontdb start end)
      (match (sfd-get-real line end port)
             ((version version-string start end)
              (sfd-get-line-end line end port)
              (sfd-check-version version port line start)
              (let ((contents (sfd-read-contents port version)))
                (list '*TOP*
                      '(*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
                      (cons 'splinefontdb
                            (cons (list '@ (list 'version version-string))
                                  contents)))))))

     (_ (throw 'sfd-error
               (_ "expected a Spline Font Database (SFD) file")
               (sfd-source-info port (skip-spaces line 0)))))))

(define (sfd-error-message msg source-info)
  (let ((location (match source-info
                         ((#f line column)
                          (simple-format #f (_ "<input> @ ~A:~A")
                                         (number->string line)
                                         (number->string (+ column 1))))
                         ((filename line column)
                          (simple-format #f (_ "~S @ ~A:~A")
                                         filename
                                         (number->string line)
                                         (number->string (+ column 1)))))))
    (simple-format #f "~A: ~A" location msg)))
