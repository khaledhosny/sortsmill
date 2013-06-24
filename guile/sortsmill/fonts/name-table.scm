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

(library (sortsmill fonts name-table)

  (export
   name-table-set!
   name-table-remove!
   name-table-ref
   name-table-partition
   name-table-append

   name-table:check-language-id

   ;; (name-table:name-id->mnemonic integer) → string or #f
   ;; (name-table:name-mnemonic->id string) → integer or #f
   ;; (name-table:windows-language-id->mnemonic integer) → string or #f
   ;; (name-table:windows-language-mnemonic->id string) → integer or #f
   name-table:name-id->mnemonic
   name-table:name-mnemonic->id
   name-table:windows-language-id->mnemonic
   name-table:windows-language-mnemonic->id

   ;; (name-table:name-ids) → list-of-integers
   ;; (name-table:name-mnemonics) → list-of-strings
   ;; (name-table:windows-languages-ids) → list-of-integers
   ;; (name-table:windows-language-mnemonics) → list-of-strings
   name-table:name-ids
   name-table:name-mnemonics      ; Lists the primary mnemonics, only.
   name-table:windows-language-ids
   name-table:windows-language-mnemonics ; Lists the primary mnemonics, only.

   name-table:read-from-sfnt-at-offset
   encoded-name-table:write-to-sfnt
   encoded-name-table->name-table

   name-table:string-is-subset?

   ;; (name-table:name-table:partition-by-name-and-language-ids name-table) → list-of-name-tables
   name-table:partition-by-name-and-language-ids

   ;; (name-table:languages-match? namerec1 namerec2) → boolean
   ;;
   ;; The name-table:languages-match? function is stored in the fluid
   ;; name-table:languages-match?-function
   name-table:languages-match?
   name-table:languages-match?-function

   name-table:select-name-string-function
   name-table:select-name-string

   name-table:save-redundant-strings?

   name-table->encoded-name-table
   name-table->encoded-name-table-fluid
   name-table:output-platforms
   name-table:platform-0-encoding-id
   name-table:platform-0-english-only?
   name-table:platform-1-english-only?
   name-table:platform-3-output-symbol-encoding?
   name-table:platform-3-output-ucs2-encoding?
   name-table:platform-3-output-ucs4-encoding?
   name-table:platform-3-output-language-specific-encoding?
   name-table:restrict-postscript-name-entries?
   name-table:encoding-name

   name-table:prune-and-prepare
   )

  (import (sortsmill dynlink)
          (sortsmill fonts opentype-io)
          (sortsmill strings ietf-language-tags)
          (sortsmill postscript)
          (sortsmill mac-encodings)
          (sortsmill kwargs)
          (sortsmill notices)
          (sortsmill i18n)
          (sortsmill fontforge-api)
          (rnrs)
          (except (guile) error)
          (only (srfi :1)
                take count filter-map list-tabulate partition
                list-index delete-duplicates)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 match)
          (ice-9 format))

  ;;-------------------------------------------------------------------------

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_sortsmill_parsettf_guile"))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_sortsmill_tottf_guile"))

  ;;-------------------------------------------------------------------------

;;;; FIXME: without-adjacent-duplicates looks very reusable. If made
;;;; reusable one might also, as well, rewrite it in C, which should
;;;; not be difficult. It is basically a fast delete-duplicates for
;;;; long, sorted lists.
  (define (without-adjacent-duplicates dup? lst)
    (if (null? lst)
        '()
        (reverse
         (fold-left
          (lambda (prior x)
            (if (dup? x (car prior))
                prior
                (cons x prior)))
          (take lst 1)
          (cdr lst)))))

  ;;-------------------------------------------------------------------------
  ;;
  ;; How the name table is represented internally.
  ;;
  ;; Currently this storage is based on format 1 of the Naming Table,
  ;; which is specified at
  ;; http://www.microsoft.com/typography/otspec/name.htm
  ;;
  ;; The name table is stored as a Guile vector containing one element:
  ;;
  ;;    #(name-records)
  ;;
  ;; The name-records field is a Guile list of ‘name records’.
  ;;
  ;; A ‘name record’ is a Guile vector like this:
  ;;
  ;;    #(platform-id language-id name-id value)
  ;;
  ;; The platform-id is an unsigned Guile integer.
  ;;
  ;; The language-id is either an unsigned Guile integer less than
  ;; #x8000 or a Guile string. If it is an integer, then it represents a
  ;; Windows platform-dependent language ID
  ;; [http://www.microsoft.com/typography/otspec/name.htm]. If the
  ;; language-id is a string, it represents a case-insensitive (but
  ;; arbitrarily capitalized) BCP 47 language tag
  ;; [http://en.wikipedia.org/wiki/IETF_language_tag].
  ;;
  ;; The name field is an unsigned Guile integer representing a Name ID
  ;; [http://www.microsoft.com/typography/otspec/name.htm].
  ;;
  ;; The value field is a Guile string.
  ;;
  ;; When a name table is ready for output, and optionally when it has
  ;; just been inputted, instead of regular name records there are
  ;; ‘encoded name records’. An encoded name record is a Guile vector
  ;; like this:
  ;;
  ;;    #(platform-id language-id name-id value encoding-id)
  ;;
  ;; The value is now a bytevector, rather than a Guile string, and
  ;; the encoding-id is an unsigned Guile integer.
  ;;  
  ;;-------------------------------------------------------------------------

  (define/kwargs (name-table-set! name-table platform-id language-id name-id
                                  value [strict? #f])
    (assert (string? value))
    (assert (and (integer? name-id) (not (negative? name-id))))
    (assert-name-table-platform-supported 'name-table-set! platform-id)
    (let ([language-id (name-table:check-language-id language-id)])
      (if language-id
          (let ([lst-without-old-entry
                 (remp (lambda (entry)
                         (name-record-keys-match?
                          'name-table-set! entry platform-id language-id name-id))
                       (vector-ref name-table 0))]
                [new-entry `#(,platform-id
                              ,(reformat-ietf-language-tag language-id)
                              ,name-id ,value)])
            (vector-set! name-table 0 (cons new-entry lst-without-old-entry)))
          (when strict?
            (not-a-valid-language-id 'name-table-set! language-id)))))

  (define/kwargs (name-table-remove! name-table platform-id language-id
                                     name-id [strict? #f])
    (assert (or (eq? name-id #t) (and (integer? name-id) (not (negative? name-id)))))
    (unless (eq? platform-id #t)
      (assert-name-table-platform-supported 'name-table-remove! platform-id))
    (let ([language-id (name-table:check-language-id language-id #t)])
      (if language-id
          (let ([lst-without-old-entry
                 (remp (lambda (entry)
                         (name-record-keys-match?
                          'name-table-remove! entry platform-id language-id name-id))
                       (vector-ref name-table 0))])
            (vector-set! name-table 0 lst-without-old-entry))
          (when strict?
            (not-a-valid-language-id 'name-table-set! language-id)))))

  (define/kwargs (name-table-ref name-table platform-id language-id name-id
                                 [strict? #f])
    (assert (or (eq? name-id #t) (and (integer? name-id) (not (negative? name-id)))))
    (unless (eq? platform-id #t)
      (assert-name-table-platform-supported 'name-table-ref platform-id))
    (let ([language-id (name-table:check-language-id language-id #t)])
      (if language-id
          (let ([sublist
                 (memp (lambda (entry) (name-record-keys-match?
                                        'name-table-ref entry platform-id language-id
                                        name-id))
                       (vector-ref name-table 0))])
            (if sublist (car sublist) #f))
          (if strict?
              (not-a-valid-language-id 'name-table-set! language-id)
              #f))))

  (define/kwargs (name-table-partition name-table
                                       [platform-id #t]
                                       [language-id #t]
                                       [name-id #t])
    (let ([language-id (name-table:check-language-id language-id #t)])
      (if language-id
          (let-values ([(matched unmatched)
                        (partition
                         (lambda (entry)
                           (name-record-keys-match? 'name-table-ref entry
                                                    platform-id language-id
                                                    name-id))
                         (vector-ref name-table 0))])
            (values `#(,matched) `#(,unmatched)))
          (values #f #f))))

  (define (name-table-append . args)
    (match args
      [() '#(())]
      [(name-table . more-name-tables)
       `#(,(append (vector-ref name-table 0)
                   (vector-ref (apply name-table-append more-name-tables) 0)))]))

  (define (name-record-keys-match? who name-record platform-id language-id name-id )
    (if (and (or (eq? name-id #t) (eqv? name-id (vector-ref name-record 2)))
             (or (eq? platform-id #t) (eqv? platform-id (vector-ref name-record 0))))
        (if (eq? language-id #t)
            #t
            (cond [(integer? language-id)
                   (eqv? language-id (vector-ref name-record 1))]
                  [(string? language-id)
                   (ietf-language-tag=? language-id (vector-ref name-record 1))]
                  [else
                   (assertion-violation who (_ "expected an integer or string")
                                        language-id)]))
        #f))

  (define/kwargs (name-table:check-language-id language-id [allow-wildcard? #f])
    (if (and allow-wildcard? (eq? language-id #t))
        #t
        (cond [(integer? language-id)     ; Platform-specific language ID.
               (if (<= 0 language-id #x7fff) language-id #f)]
              [(string? language-id)        ; An IETF language tag.
               (let ([decomposition (parse-ietf-language-tag language-id)])
                 (if decomposition (unparse-ietf-language-tag decomposition) #f))]
              [else #f])))

  (define (assert-name-table-platform-supported who platform-id)
    ;; Platforms that may appear in a ‘name’ table. (There are more
    ;; that may appear in a ‘cmap’ table.)
    (unless (or (eqv? platform-id 0)       ; Unicode.
                (eqv? platform-id 1)       ; Macintosh.
                (eqv? platform-id 3))      ; Windows.
      (assertion-violation who (_ "platform ID not supported in name tables")
                           platform-id)))

  (define (not-a-valid-language-id who language)
    (assertion-violation who (_ "not a value language ID") language))

  ;;-------------------------------------------------------------------------

  (define name-id-associations
    '((2 . "Subfamily") ;; ← The primary mnemonic.
      (2 . "SubFamily") ;; ← The mnemonic used in FontForge.
      (2 . "Style")
      (2 . "Styles")
      (0 . "Copyright")
      (1 . "Family")
      (4 . "Fullname")
      (3 . "UniqueID")
      (5 . "Version")
      (6 . "PostScript Name") ;; ← The primary mnemonic.
      (6 . "PostScriptName")  ;; ← The mnemonic used in FontForge.
      (7 . "Trademark")
      (8 . "Manufacturer")
      (9 . "Designer")
      (10 . "Descriptor")
      (11 . "Vendor URL")
      (12 . "Designer URL")
      (13 . "License")
      (14 . "License URL")
      ;; Slot 15 is reserved.
      (16 . "Preferred Family")
      (17 . "Preferred Subfamily") ;; ← The primary mnemonic.
      (17 . "Preferred Styles") ;; ← The mnemonic used in FontForge.
      (17 . "Preferred SubFamily")
      (17 . "Preferred Style")
      (18 . "Compatible Full")
      (19 . "Sample Text")
      (20 . "CID findfont Name")
      (21 . "WWS Family")
      (22 . "WWS Subfamily") ;; ← The primary mnemonic and also the mnemonic used in FontForge.
      (22 . "WWS SubFamily")
      (22 . "WWS Style")
      (22 . "WWS Styles")))

  (define windows-language-associations
    '((#x436 . "Afrikaans")
      (#x41c . "Albanian")
      (#x45e . "Amharic")
      (#x401 . "Arabic (Saudi Arabia)")
      (#x801 . "Arabic (Iraq)")
      (#xc01 . "Arabic (Egypt)")
      (#x1001 . "Arabic (Libya)")
      (#x1401 . "Arabic (Algeria)")
      (#x1801 . "Arabic (Morocco)")
      (#x1C01 . "Arabic (Tunisia)")
      (#x2001 . "Arabic (Oman)")
      (#x2401 . "Arabic (Yemen)")
      (#x2801 . "Arabic (Syria)")
      (#x2c01 . "Arabic (Jordan)")
      (#x3001 . "Arabic (Lebanon)")
      (#x3401 . "Arabic (Kuwait)")
      (#x3801 . "Arabic (U.A.E.)")
      (#x3c01 . "Arabic (Bahrain)")
      (#x4001 . "Arabic (Qatar)")
      (#x42b . "Armenian")
      (#x44d . "Assamese")
      (#x42c . "Azeri (Latin)")
      (#x82c . "Azeri (Cyrillic)")
      (#x42d . "Basque")
      (#x423 . "Byelorussian")
      (#x445 . "Bengali")
      (#x845 . "Bengali Bangladesh")
      (#x402 . "Bulgarian")
      (#x455 . "Burmese")
      (#x403 . "Catalan")
      (#x453 . "Cambodian")
      (#x45c . "Cherokee")
      (#x404 . "Chinese (Taiwan)")
      (#x804 . "Chinese (PRC)")
      (#xc04 . "Chinese (Hong Kong)")
      (#x1004 . "Chinese (Singapore)")
      (#x1404 . "Chinese (Macau)")
      (#x41a . "Croatian")
      (#x101a . "Croatian Bosnia/Herzegovina")
      (#x405 . "Czech")
      (#x406 . "Danish")
      (#x465 . "Divehi")
      (#x413 . "Dutch")
      (#x813 . "Flemish (Belgian Dutch)")
      (#x466 . "Edo")
      (#x809 . "English (British)")
      (#x409 . "English (US)")
      (#x1009 . "English (Canada)")
      (#xc09 . "English (Australian)")
      (#x1409 . "English (New Zealand)")
      (#x1809 . "English (Irish)")
      (#x1c09 . "English (South Africa)")
      (#x2009 . "English (Jamaica)")
      (#x2409 . "English (Caribbean)")
      (#x2809 . "English (Belize)")
      (#x2c09 . "English (Trinidad)")
      (#x3009 . "English (Zimbabwe)")
      (#x3409 . "English (Philippines)")
      (#x3809 . "English (Indonesia)")
      (#x3c09 . "English (Hong Kong)")
      (#x4009 . "English (India)")
      (#x4409 . "English (Malaysia)")
      (#x425 . "Estonian")
      (#x438 . "Faeroese")
      (#x429 . "Farsi")
      (#x464 . "Filipino")
      (#x40b . "Finnish")
      (#x40c . "French French")
      (#x80c . "French Belgium")
      (#xc0c . "French Canadian")
      (#x100c . "French Swiss")
      (#x140c . "French Luxembourg")
      (#x180c . "French Monaco")
      (#x1c0c . "French West Indies")
      (#x200c . "French Réunion")
      (#x240c . "French D.R. Congo")
      (#x280c . "French Senegal")
      (#x2c0c . "French Camaroon")
      (#x300c . "French Côte d'Ivoire")
      (#x340c . "French Mali")
      (#x380c . "French Morocco")
      (#x3c0c . "French Haiti")
      (#xe40c . "French North Africa")
      (#x462 . "Frisian")
      (#x467 . "Fulfulde")
      (#x43c . "Gaelic (Scottish)")
      (#x83c . "Gaelic (Irish)")
      (#x467 . "Galician")
      (#x437 . "Georgian")
      (#x407 . "German German")
      (#x807 . "German Swiss")
      (#xc07 . "German Austrian")
      (#x1007 . "German Luxembourg")
      (#x1407 . "German Liechtenstein")
      (#x408 . "Greek")
      (#x474 . "Guarani")
      (#x447 . "Gujarati")
      (#x468 . "Hausa")
      (#x475 . "Hawaiian")
      (#x40d . "Hebrew")
      (#x439 . "Hindi")
      (#x40e . "Hungarian")
      (#x469 . "Ibibio")
      (#x40f . "Icelandic")
      (#x470 . "Igbo")
      (#x421 . "Indonesian")
      (#x45d . "Inuktitut")
      (#x410 . "Italian")
      (#x810 . "Italian Swiss")
      (#x411 . "Japanese")
      (#x44b . "Kannada")
      (#x471 . "Kanuri")
      (#x860 . "Kashmiri (India)")
      (#x43f . "Kazakh")
      (#x453 . "Khmer")
      (#x440 . "Kirghiz")
      (#x457 . "Konkani")
      (#x412 . "Korean")
      (#x812 . "Korean (Johab)")
      (#x454 . "Lao")
      (#x426 . "Latvian")
      (#x476 . "Latin")
      (#x427 . "Lithuanian")
      (#x827 . "Lithuanian (Classic)")
      (#x42f . "Macedonian")
      (#x43e . "Malay")
      (#x83e . "Malay (Brunei)")
      (#x44c . "Malayalam")
      (#x43a . "Maltese")
      (#x458 . "Manipuri")
      (#x481 . "Maori")
      (#x44e . "Marathi")
      (#x450 . "Mongolian (Cyrillic)")
      (#x850 . "Mongolian (Mongolian)")
      (#x461 . "Nepali")
      (#x861 . "Nepali (India)")
      (#x414 . "Norwegian (Bokmal)")
      (#x814 . "Norwegian (Nynorsk)")
      (#x448 . "Oriya")
      (#x472 . "Oromo")
      (#x479 . "Papiamentu")
      (#x463 . "Pashto")
      (#x415 . "Polish")
      (#x416 . "Portuguese (Portugal)")
      (#x816 . "Portuguese (Brasil)")
      (#x446 . "Punjabi (India)")
      (#x846 . "Punjabi (Pakistan)")
      (#x46b . "Quecha (Bolivia)")
      (#x86b . "Quecha (Ecuador)")
      (#xc6b . "Quecha (Peru)")
      (#x417 . "Rhaeto-Romanic")
      (#x418 . "Romanian")
      (#x818 . "Romanian (Moldova)")
      (#x419 . "Russian")
      (#x819 . "Russian (Moldova)")
      (#x43b . "Sami (Lappish)")
      (#x43b . "Sanskrit")
      (#x46c . "Sepedi")
      (#xc1a . "Serbian (Cyrillic)")
      (#x81a . "Serbian (Latin)")
      (#x459 . "Sindhi India")
      (#x859 . "Sindhi Pakistan")
      (#x45b . "Sinhalese")
      (#x41b . "Slovak")
      (#x424 . "Slovenian")
      (#x42e . "Sorbian")
      (#x40a . "Spanish (Traditional)")
      (#x80a . "Spanish Mexico")
      (#xc0a . "Spanish (Modern)")
      (#x100a . "Spanish (Guatemala)")
      (#x140a . "Spanish (Costa Rica)")
      (#x180a . "Spanish (Panama)")
      (#x1c0a . "Spanish (Dominican Republic)")
      (#x200a . "Spanish (Venezuela)")
      (#x240a . "Spanish (Colombia)")
      (#x280a . "Spanish (Peru)")
      (#x2c0a . "Spanish (Argentina)")
      (#x300a . "Spanish (Ecuador)")
      (#x340a . "Spanish (Chile)")
      (#x380a . "Spanish (Uruguay)")
      (#x3c0a . "Spanish (Paraguay)")
      (#x400a . "Spanish (Bolivia)")
      (#x440a . "Spanish (El Salvador)")
      (#x480a . "Spanish (Honduras)")
      (#x4c0a . "Spanish (Nicaragua)")
      (#x500a . "Spanish (Puerto Rico)")
      (#x540a . "Spanish (United States)")
      (#xe40a . "Spanish (Latin America)")
      (#x430 . "Sutu")
      (#x441 . "Swahili (Kenyan)")
      (#x41d . "Swedish (Sweden)")
      (#x81d . "Swedish (Finland)")
      (#x45a . "Syriac")
      (#x464 . "Tagalog")
      (#x428 . "Tajik")
      (#x45f . "Tamazight (Arabic)")
      (#x85f . "Tamazight (Latin)")
      (#x449 . "Tamil")
      (#x444 . "Tatar (Tatarstan)")
      (#x44a . "Telugu")
      (#x41e . "Thai")
      (#x451 . "Tibetan (PRC)")
      (#x851 . "Tibetan Bhutan")
      (#x473 . "Tigrinya Ethiopia")
      (#x873 . "Tigrinyan Eritrea")
      (#x431 . "Tsonga")
      (#x432 . "Tswana")
      (#x41f . "Turkish")
      (#x442 . "Turkmen")
      (#x480 . "Uighur")
      (#x422 . "Ukrainian")
      (#x420 . "Urdu (Pakistan)")
      (#x820 . "Urdu (India)")
      (#x443 . "Uzbek (Latin)")
      (#x843 . "Uzbek (Cyrillic)")
      (#x433 . "Venda")
      (#x42a . "Vietnamese")
      (#x452 . "Welsh")
      (#x434 . "Xhosa")
      (#x478 . "Yi")
      (#x43d . "Yiddish")
      (#x46a . "Yoruba")
      (#x435 . "Zulu")))

  (define (name-table:name-id->mnemonic id)
    (assv-ref name-id-associations id))

  (define (name-table:name-mnemonic->id mnemonic)
    (let ([reversed-alist (map
                           (lambda (pair) (cons (cdr pair) (car pair)))
                           name-id-associations)])
      (assoc-ref reversed-alist mnemonic)))

  (define (name-table:windows-language-id->mnemonic id)
    (assv-ref windows-language-associations id))

  (define (name-table:windows-language-mnemonic->id mnemonic)
    (let ([reversed-alist (map
                           (lambda (pair) (cons (cdr pair) (car pair)))
                           windows-language-associations)])
      (assoc-ref reversed-alist mnemonic)))

  (define name-table:name-ids
    (let ([ids (without-adjacent-duplicates
                = (list-sort < (map car name-id-associations)))])
      (lambda () ids)))

  (define name-table:names-mnemonics
    (let ([langs (map
                  (lambda (id) (name-table:name-id->mnemonic id))
                  (name-table:name-ids))])
      (lambda () langs)))

  (define name-table:windows-language-ids
    (let ([ids (without-adjacent-duplicates
                = (list-sort < (map car windows-language-associations)))])
      (lambda () ids)))

  (define name-table:windows-language-mnemonics
    (let* ([pairs-without-dups (without-adjacent-duplicates
                                (lambda (a b) (= (car a) (car b)))
                                windows-language-associations)]
           [langs (map cdr pairs-without-dups)])
      (lambda () langs)))

  ;;-------------------------------------------------------------------------

  (define/kwargs (name-table:read-from-sfnt-at-offset port/fd offset
                                                      [decode? #t])
    (let ([port (if (port? port/fd) port/fd (fdes->inport port/fd))])
      (ot:at-port-position
       port offset
       (lambda ()
         (let*-values
             ([(table-format) (ot:read-USHORT port)]
              [(namerec-count) (ot:read-USHORT port)]
              [(string-offset) (ot:read-USHORT port)]
              [(namerecs)
               (list-tabulate namerec-count
                              (lambda (i)
                                (read-name-record
                                 port (+ offset string-offset))))]
              [(langtag-count) (if (= 1 table-format) (ot:read-USHORT port) 0)]
              [(langtagrecs)
               (list-tabulate langtag-count
                              (lambda (i)
                                (read-lang-tag-record
                                 port (+ offset string-offset))))]
              [(namerecs fix-up-errors)
               (fix-up-encoded-namerecs namerecs langtagrecs)]
              [(encoded-name-table) `#(,namerecs)])
;;;;;;;;;;;           (encoded-name-table:write-to-sfnt 1 encoded-name-table)
           (if decode?
               (let-values ([(name-table errors)
                             (encoded-name-table->name-table encoded-name-table)])
;;;;;;;;                 ((@ (ice-9 pretty-print) pretty-print) (vector-ref (name-table:prune-and-prepare name-table) 0))
;;;;;;;;;                 ((@ (ice-9 pretty-print) pretty-print) (map (match-lambda [#(p g n v e) `#(,p ,g ,e)])(vector-ref encoded-name-table 0)))
                 ((@ (ice-9 pretty-print) pretty-print) (name-table->encoded-name-table (name-table:prune-and-prepare name-table)))
                 (values name-table (append fix-up-errors errors)))
               (values encoded-name-table (append fix-up-errors))))))))

  (define/kwargs (encoded-name-table->name-table encoded-name-table)
    (let*-values
        ([(encoded-namerecs) (vector-ref encoded-name-table 0)]
         [(namerecs-and-errors)
          (map
           (match-lambda
            [#(platform-id language-id name-id value encoding-id)
             (let ([str (recode-ot-string-as-utf8 (bytevector->pointer value)
                                                  (bytevector-length value)
                                                  platform-id encoding-id
                                                  language-id)])
               (if str
                   (cons `#(,platform-id ,language-id ,name-id ,str) '())
                   (begin
                     ;; Undecodable string.
                     ;;
                     ;; FIXME: Post a warning and log a validation problem.
                     ;;
                     (cons #f (list (list 'undecodable-string))))))])
           encoded-namerecs)]
         [(namerecs) (filter-map car namerecs-and-errors)]
         [(errors) (fold-namerec-errors namerecs-and-errors)]
         [(namerecs ps-name-errors) (fix-postscript-font-name namerecs)])
      (values `#(,namerecs) (append errors ps-name-errors))))

  (define (read-name-record port string-buffer-position)
    (let* ([platform-id (ot:read-USHORT port)]
           [encoding-id (ot:read-USHORT port)]
           [language-id (ot:read-USHORT port)]
           [name-id (ot:read-USHORT port)]
           [length (ot:read-USHORT port)]
           [offset (ot:read-USHORT port)]
           [value (ot:read-string length port
                                  (+ string-buffer-position offset))])
      `#(,platform-id ,language-id ,name-id ,value ,encoding-id)))

  (define (read-lang-tag-record port string-buffer-position)
    (let* ([length (ot:read-USHORT port)]
           [offset (ot:read-USHORT port)]
           [bv (ot:read-string length port (+ string-buffer-position offset))]
           [str (utf16->string bv (endianness big))]
           [decomposition (parse-ietf-language-tag str)]
           [tag (if decomposition (unparse-ietf-language-tag decomposition) #f)])
      tag))

  (define (fix-up-encoded-namerecs namerecs langtagrecs)
    (let* ([tags (list->vector langtagrecs)]
           [n (vector-length tags)]
           [namerecs-and-errors
            (map
             (match-lambda
              [(and #(platform-id
                      (? (lambda (lang) (< lang #x8000)) language-id)
                      name-id value encoding-id) nrec)
               ;; Platform-specific language ID.
               (cons nrec '())]
              [#(platform-id
                 (? (lambda (lang) (< (- lang #x8000) n)) language-id)
                 name-id value encoding-id)
               ;; IETF language tag.
               (let* ([i (- language-id #x8000)]
                      [tag (vector-ref tags i)])
                 (cond
                  [tag
                   (cons `#(,platform-id ,tag ,name-id ,value ,encoding-id)
                         '())]
                  [else
                   ;; Ignore any name record whose language tag was
                   ;; unparseable.
                   ;;
                   ;; FIXME: Post a warning and log a validation problem.
                   ;;
                   (cons #f (list (list 'unparseable-language-tag)))] ))]
              [#(platform-id language-id name-id value encoding-id)
               ;; Ignore any name record whose platform-specific
               ;; language ID is greater than #x7FFF.
               ;;
               ;; FIXME: Post a warning and log a validation problem.
               ;;
               (cons #f (list (list 'language-id-above-7FFF language-id)))])
             namerecs)])
      (values (filter-map car namerecs-and-errors)
              (fold-namerec-errors namerecs-and-errors))))

  (define (fix-postscript-font-name namerecs)

    (define (warn-bad-name value errors platform-id)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "The PostScript font name \"~a\"\nfor platform ~a is invalid.\nThe PostScript name should be printable ASCII,\nmust not contain (){}[]<>%%/, space,\nor a byte order mark,\nmust be no longer than 63 characters,\nand cannot be a PostScript number.")
               value platform-id)))

    (define (warn-bad-language platform-id)
      (log-fontforge-warning
       (format #f
               (_ "Warning: A PostScript font name (Naming Table entry 6)\nfor platform ~a is assigned a language ID\nother than the platform-specific code for US English.\nOpenType requires that applications ignore it.")
               platform-id)))

    (define (warn-bad-platform platform-id)
      (log-fontforge-warning
       (format #f
               (_ "Warning: There is a PostScript font name entry\n(Naming Table entry 6) for platform ~a;\nOpenType requires that applications ignore it.")
               platform-id)))

    (define (warn-missing-platform platform-id)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "Their is a PostScript font name entry\n(Naming Table entry 6) for platform ~a is missing.\nIf a PostScript font name is included,\nthere must be entries for both platforms 1 and 3.")
               platform-id)))

    (define (warn-nonidentical-values value1 value3)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "The PostScript font name entries (Naming Table entry 6)\nfor platforms 1 and 3 are not identical:\n~a\n~a")
               value1 value3)))

    (define (warn-multiple-entries platform-id)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "Their is more than one PostScript font name entry\n(Naming Table entry 6) for platform ~a.")
               platform-id)))

    (define (fix-individual-entries namerecs)
      (let ([namerecs-and-errors
             (map
              (match-lambda
               [#((? (lambda (p) (= p 1)) platform-id)
                  (? (lambda (g) (= g 0)) language-id)
                  (? (lambda (n) (eqv? 6 n)) name-id)
                  value)
                ;; PostScript name for Macintosh platform.
                (let-values ([(ps-name errors)
                              (validate-postscript-font-name value)])
                  (unless (null? errors)
                    (warn-bad-name value errors platform-id))
                  (cons `#(,platform-id 0 ,name-id ,ps-name) errors))]
               [#((? (lambda (p) (= p 3)) platform-id)
                  (? (lambda (g) (= g #x409)) language-id)
                  (? (lambda (n) (eqv? 6 n)) name-id)
                  value)
                ;; PostScript name for Windows platform.
                (let-values ([(ps-name errors)
                              (validate-postscript-font-name value)])
                  (unless (null? errors)
                    (warn-bad-name value errors platform-id))
                  (cons `#(,platform-id #x409 ,name-id ,ps-name) errors))]
               [(and #((? (lambda (p) (or (= p 1) (= p 3))) platform-id)
                       language-id
                       (? (lambda (n) (eqv? 6 n)) name-id)
                       _)
                     nrec)
                ;; PostScript name with language other than the
                ;; platform-specific ID for US English. OpenType
                ;; requires it be ignored, so issue a warning.
                (warn-bad-language platform-id)
                (cons nrec (list (list 'warning:bad-language-id
                                       platform-id language-id)))]
               [(and #(platform-id _ (? (lambda (n) (eqv? 6 n)) name-id) _)
                     nrec)
                ;; PostScript name for other platforms. OpenType
                ;; requires it be ignored, so issue a warning.
                (warn-bad-platform platform-id)
                (cons nrec (list (list 'warning:bad-platform-id platform-id)))]
               [nrec (cons nrec '())])
              namerecs)])
        (values (filter-map car namerecs-and-errors)
                (fold-namerec-errors namerecs-and-errors))))

    (define (check-entry-pairing namerecs)
      (let ([platform1-sublist
             (memp (lambda (entry)
                     (name-record-keys-match? 'fix-postscript-font-name
                                              entry 1 0 6))
                   namerecs)]
            [platform3-sublist
             (memp (lambda (entry)
                     (name-record-keys-match? 'fix-postscript-font-name
                                              entry 3 #x409 6))
                   namerecs)])
        (match (cons platform1-sublist platform3-sublist)
          [(#f . #f) '()]
          [(#f . _)
           (warn-missing-platform 1)
           (list (list 'missing-platform 1))]
          [(_ . #f)
           (warn-missing-platform 3)
           (list (list 'missing-platform 3))]
          [((#(_ _ _ val1) . _) . (#(_ _ _ val3) . _))
           (if (string=? val1 val3)
               '()
               (begin
                 (warn-nonidentical-values val1 val3)
                 (list (list 'nonidentical-values val1 val3))))])))

    (define (check-multiple-entries namerecs platform-id language-id)
      (let ([n
             (count
              (match-lambda
               [#(platform-id^
                  language-id^
                  (? (lambda (n) (eqv? 6 n)) _)
                  _)
                (and (= platform-id platform-id^)
                     (= language-id language-id^))]
               [_ #f])
              namerecs)])
        (if (<= n 1)
            '()
            (begin
              (warn-multiple-entries platform-id)
              (list (list 'multiple-entries platform-id))))))

    (let*-values
        ([(multiple-1-entry-errors) (check-multiple-entries namerecs 1 0)]
         [(multiple-3-entry-errors) (check-multiple-entries namerecs 3 #x409)]
         [(pairing-errors) (check-entry-pairing namerecs)]
         [(namerecs indiv-errors) (fix-individual-entries namerecs)])
      (values namerecs (append multiple-1-entry-errors multiple-3-entry-errors
                               pairing-errors indiv-errors))))

  (define (fold-namerec-errors namerecs-and-errors)
    (delete-duplicates
     (fold-left (lambda (prior n&e) (append (cdr n&e) prior))
                '() namerecs-and-errors)))

  ;;-------------------------------------------------------------------------

  (define* (validate-postscript-font-name font-name)
    (let*-values
        ([(name errors) (values font-name '())]
         [(name errors) (check-for-byte-order-mark name errors)]
         [(name errors) (check-for-postscript-number name errors)]
         [(name errors) (check-for-illegal-characters name errors)]
         [(name errors) (check-for-font-name-too-long name errors)])
      (values name errors)))

  (define (check-for-byte-order-mark font-name errors)
    (if (and (positive? (string-length font-name))
             (char=? #\xFEFF (string-ref font-name 0)))
        (values (substring font-name 1)
                (cons (list 'byte-order-mark font-name) errors))
        (values font-name errors)))

  (define (check-for-postscript-number font-name errors)
    (if (postscript-number? font-name)
        (values (string-append "font-" font-name)
                (cons (list 'postscript-number font-name) errors))
        (values font-name errors)))

  (define char-set:postscript-name-legal-chars
    (char-set-delete (char-set-intersection char-set:ascii char-set:graphic)
                     #\( #\[ #\{ #\< #\) #\] #\} #\> #\% #\/))

  (define (check-for-illegal-characters font-name errors)
    (let ([mapped-name (string-map
                        (lambda (c)
                          (if (char-set-contains?
                               char-set:postscript-name-legal-chars c)
                              c
                              #\_))
                        font-name)])
      (values mapped-name
              (if (string=? font-name mapped-name)
                  errors
                  (cons (list 'illegal-characters font-name) errors)))))

  (define (check-for-font-name-too-long font-name errors)
    (if (< 63 (string-length font-name))
        (values (substring font-name 0 63)
                (cons (list 'too-long font-name) errors))
        (values font-name errors)))

  ;;-------------------------------------------------------------------------

  (define (languages-match?-function-default namerec1 namerec2)
    (match namerec1
      [#(platform-id1 language-id1 name-id1 value1)
       (match namerec2
         [#(platform-id2 language-id2 name-id2 value2)
          (cond
           [(string? language-id1)
            (and (string? language-id2)
                 (ietf-language-tag=? language-id1 language-id2))]
           [else
            (let ([winlang1 (windows-language-id platform-id1 language-id1)]
                  [winlang2 (windows-language-id platform-id2 language-id2)])
              (and winlang1 winlang2 (= winlang1 winlang2)))] )] )] ))

  (define (windows-language-id platform-id language-id)
    ;; We will assume that platform 0 uses Macintosh language ID
    ;; codes, unless the language ID is an IETF language tag.
    ;;
    ;; There is no standard meaning for language ID codes under #x8000
    ;; for platform 0. The OpenType standard slyly recommends setting
    ;; it to zero, which conveniently corresponds to the Macintosh
    ;; platform code for ‘English’. See Microsoft’s Calibri fonts for
    ;; an example: language ID is set to zero, and the name strings
    ;; are in English.
    (case platform-id
      [(0 1) (windows-language-from-mac-language language-id)]
      [(3) language-id]
      [else
       ;; What platform is this?
       #f]))

  (define name-table:languages-match?-function
    (make-fluid languages-match?-function-default))

  (define (name-table:languages-match? namerec1 namerec2)
    ((fluid-ref name-table:languages-match?-function) namerec1 namerec2))

  (define/kwargs (name-table:partition-by-name-and-language-ids name-table)
    (let* ([langs-match? (fluid-ref name-table:languages-match?-function)]
           [they-match? (lambda (nrec1 nrec2)
                          (and (= (vector-ref nrec1 2) (vector-ref nrec2 2))
                               (langs-match? nrec1 nrec2)))])
      (letrec
          ([partition-by-langs
            (match-lambda
             [() '()]
             [(nrec . more-nrecs)
              (let-values
                  ([(matching nonmatching)
                    (partition (cut they-match? nrec <>) more-nrecs)])
                (cons `#(,(cons nrec matching))
                      (partition-by-langs nonmatching)))])])
        (partition-by-langs (vector-ref name-table 0)))))


  (define (select-name-string-function-default name-table)
    (let* ([namerecs (vector-ref name-table 0)]
           [unicode-names (memp-by-platform 0 namerecs)]
           [macintosh-names (memp-by-platform 1 namerecs)]
           [windows-names (memp-by-platform 3 namerecs)])
      (cond [windows-names (car windows-names)]
            [unicode-names (car unicode-names)]
            [macintosh-names (car macintosh-names)]
            [else
             (assertion-violation
              'select-name-string-function-default
              (_ "expected there to be at least one valid entry in the given name-table")
              name-table)])))

  (define (memp-by-platform platform-id namerecs)
    (memp (lambda (nrec) (= platform-id (vector-ref nrec 0))) namerecs))

  (define name-table:select-name-string-function
    (make-fluid select-name-string-function-default))

  (define (name-table:select-name-string name-table)
    ((fluid-ref name-table:select-name-string-function) name-table))

  (define (get-mismatched-namerecs namerec name-table)
    (let ([value (vector-ref namerec 3)]
          [nrecs (vector-ref name-table 0)])
      (delete-duplicates
       (filter (lambda (nrec) (not (string=? (vector-ref nrec 3) value))) nrecs)
       (lambda (nrec1 nrec2) (string=? (vector-ref nrec1 3)
                                       (vector-ref nrec2 3))))))

  (define name-table:save-redundant-strings? (make-fluid #f))

  (define (prune-single-language-and-name-id-name-table selected-namerec name-table
                                                        get-new-name-id)
    ;; A typical value for get-new-name-id, given a list of
    ;; name-tables, when you want the new name ID to be unique within
    ;; those tables:
    ;;
    ;;    (lambda () (apply get-unused-name-id list-of-name-tables))
    ;;
    (let ([language-id (vector-ref selected-namerec 1)]
          [name-id (vector-ref selected-namerec 2)]
          [value (vector-ref selected-namerec 3)]
          [mismatches (get-mismatched-namerecs selected-namerec name-table)])
      (if (fluid-ref name-table:save-redundant-strings?)
          `#(,(cons
               selected-namerec
               (map
                (lambda (nrec)
                  (let ([new-name-id (get-new-name-id)])
                    (log-fontforge-warning
                     (format
                      #f
                      (_ "Warning: For name string ~a, language ~a,\nI am taking string\n  ~s\ninstead of\n  ~s\nand changing the name ID of the latter to ~a.")
                      name-id language-id value (vector-ref nrec 3) new-name-id))
                    `#(,(vector-ref nrec 0)
                       ,(vector-ref nrec 1)
                       ,new-name-id
                       ,(vector-ref nrec 3))))
                mismatches)))
          (begin
            (for-each
             (lambda (nrec)
               (log-fontforge-warning
                (format
                 #f
                 (_ "Warning: For name string ~a, language ~a,\nI am taking string\n  ~s\ninstead of\n  ~s\nand discarding the latter.")
                 name-id language-id value (vector-ref nrec 3))))
             mismatches)
            `#(,(list selected-namerec))))))

  (define (convert-to-windows-strings name-table)
    (let ([namerecs
           (filter-map
            (match-lambda
             [#(platform-id language-id name-id value)
              (if (string? language-id)
                  `#(3 ,language-id ,name-id ,value)
                  (let ([winlang (windows-language-id platform-id language-id)])
                    (if (not winlang)
                        #f ;; FIXME: Maybe post a warning.
                        `#(3 ,winlang ,name-id ,value))))])
            (vector-ref name-table 0))])
      `#(,namerecs)))

  (define/kwargs (name-table:prune-and-prepare name-table)
    (let ([partitioned-name-tables
           (name-table:partition-by-name-and-language-ids name-table)])
      (convert-to-windows-strings
       (fold-left
        (lambda (new-name-table isolated-string-nametab)
          (let* ([get-new-name-id
                  (lambda () (get-unused-name-id new-name-table))]
                 [selected-namerec
                  (name-table:select-name-string isolated-string-nametab)]
                 [new-isolated-string-nametab
                  (prune-single-language-and-name-id-name-table
                   selected-namerec isolated-string-nametab get-new-name-id)])
            (name-table-append new-isolated-string-nametab new-name-table)))
        '#(())
        partitioned-name-tables))))

  ;;-------------------------------------------------------------------------

  (define (get-unused-name-id . name-tables)
    ;; Find a name ID, in the `font-specific' range [256,32767], that
    ;; does not yet appear in any of the name-tables listed as
    ;; arguments.

    (define (find-first-unused used-values value-to-try)
      (when (< 32767 value-to-try)
        (error 'get-unused-name-id
               "no font-specific name IDs are available (which, in practice, never should have happened)"))
      (if (memv value-to-try used-values)
          (find-first-unused used-values (+ value-to-try 1))
          value-to-try))

    (let ([font-specific-name-ids
           (apply get-font-specific-name-ids name-tables)])
      (find-first-unused font-specific-name-ids 256)))

  (define (get-font-specific-name-ids . name-tables)
    ;; Return a list of all name IDs, in the font-specific range
    ;; [256,32767], that appear in any of the name-tables listed as
    ;; arguments.
    (fold-left (lambda (prior nametab)
                 (append (fold-left
                          (lambda (prior^ nrec)
                            (let ([name-id (vector-ref nrec 2)])
                              (if (and (<= 256 name-id)
                                       (not (memq name-id prior^)))
                                  (cons name-id prior^)
                                  prior^)))
                          '()
                          (vector-ref nametab 0))
                         prior))
               '()
               name-tables))

  ;;-------------------------------------------------------------------------

  (define name-table:output-platforms (make-fluid '(1 3)))
  (define name-table:platform-0-encoding-id (make-fluid 4)) ; Unicode full repertoire.
  (define name-table:platform-0-english-only? (make-fluid #t))
  (define name-table:platform-1-english-only? (make-fluid #t))
  (define name-table:platform-3-output-symbol-encoding? (make-fluid #f))
  (define name-table:platform-3-output-ucs2-encoding? (make-fluid #t))
  (define name-table:platform-3-output-ucs4-encoding? (make-fluid #f))
  (define name-table:platform-3-output-language-specific-encoding? (make-fluid #t))
  (define name-table:restrict-postscript-name-entries? (make-fluid #t))

  ;; FIXME: This fluid, if set to something other than #f, is set to a
  ;; fontforge-api structure. This is not desirable.
  (define name-table:encoding-name (make-fluid #f))

  (define (diversify-name-record name-record)
    (let ([output-platforms
           (filter (lambda (p) (or (equal? p 0) (equal? p 1) (equal? p 3)))
                   (fluid-ref name-table:output-platforms))])
      (match name-record
        [#(3 (? string? language-id) name-id value)
         ;; The language ID is an IETF language tag.
         (map (lambda (p) `#(,p ,language-id ,name-id ,value)) output-platforms)]
        [#(3 language-id name-id value)
         ;; The language ID is platform-specific.
         (let ([plat0
                (if (and (memv 0 output-platforms)
                         (or (= language-id #x409)
                             (fluid-ref name-table:platform-0-english-only?)))
                    `(#(0
                        ,(mac-language-from-windows-language language-id)
                        ,name-id
                        ,value))
                    '())]
               [plat1
                (if (and (memv 1 output-platforms)
                         (or (= language-id #x409)
                             (fluid-ref name-table:platform-1-english-only?)))
                    `(#(1
                        ,(mac-language-from-windows-language language-id)
                        ,name-id
                        ,value))
                    '())]
               [plat3
                (if (memv 3 output-platforms)
                    `(#(3 ,language-id ,name-id ,value))
                    '())])
           (append plat0 plat1 plat3))] )))

  (define (encoding-name->windows-encoding-id encoding-name)
    (cond
     [(Encoding:is-korean-ref encoding-name) 5]   ; Wansung.
     [(Encoding:is-japanese-ref encoding-name) 2] ; ShiftJIS.
     [(Encoding:is-simple-chinese-ref encoding-name) 3] ; PRC (packed GB2312).
     [(string-ci=? "EUC-GB12345"
                   (pointer->string (Encoding:encoding-name-ref encoding-name)
                                    -1 "UTF-8")) 3]   ; Tell a lie.
     [(Encoding:is-trad-chinese-ref encoding-name) 4] ; Big5.
     [else #f] ))

  (define (encode-name-record name-record)
    (match name-record
      [(? (const (fluid-ref name-table:restrict-postscript-name-entries?))
          #(1 0 6 value))
       ;; PostScript name, in Macintosh Roman.
       `(#(1 0 6 ,(string->mac-encoded-string value 0 0) 0))]
      [(? (const (fluid-ref name-table:restrict-postscript-name-entries?))
          #(3 #x409 6 value))
       ;; PostScript name, in Windows ‘UCS-2’.
       ;;
       ;; I believe modern Windows can at least display UTF-16
       ;; surrogate pairs, but a PostScript name is required to be
       ;; ASCII, anyway.
       `(#(3 #x409 6 ,(string->utf16 value (endianness little)) 1))]
      [(? (const (fluid-ref name-table:restrict-postscript-name-entries?))
          #(_ _ 6 _))
       ;; Any other PostScript name string is supposed to be ignored
       ;; by applications, so do not bother outputting it.
       '()]
      [#(0 language-id name-id value)
       ;; Unicode platform.
       `(#(0
           ,language-id
           ,name-id
           ,(string->utf16 value (endianness big))
           ,(fluid-ref name-table:platform-0-encoding-id)))]
      [#(1 language-id name-id value)
       ;; Macintosh platform.
       (let* ([encoding-id (mac-encoding-from-mac-language language-id)]
              [bv (string->mac-encoded-string value encoding-id language-id)])
         (if (not bv)
             '()
             `(#(1 ,language-id ,name-id ,bv ,encoding-id))))]
      [#(3 language-id name-id value)
       ;; Windows platform.
       (let* ([encoded-namerec
              (lambda (encoder encoding-id)
                `#(3
                   ,language-id
                   ,name-id
                   ,(encoder value (endianness little))
                   ,encoding-id))]
              [ucs2 (if (fluid-ref name-table:platform-3-output-ucs2-encoding?)
                        ;; I believe modern Windows can at least
                        ;; display UTF-16 surrogate pairs, if not
                        ;; handle them in all ways in all
                        ;; applications.
                        (list (encoded-namerec string->utf16 1))
                        '())]
              [ucs4 (if (fluid-ref name-table:platform-3-output-ucs4-encoding?)
                        (list (encoded-namerec string->utf32 10))
                        '())]
              #|
              ;; FIXME: Instead of doing this, have the caller set the
              ;; fluid name-table:platform-3-output-symbol-encoding?
              ;; temporarily in a dynamic wind.
              ;;
              [symbol-font? (eq? font-format 'ttf-symbol)]
              [symbol (if (and symbol-font?
                               (fluid-ref name-table:platform-3-output-symbol-encoding?))
                          (list (encoded-namerec string->utf16 0))
                          '())]
              |#
              [symbol (if (fluid-ref name-table:platform-3-output-symbol-encoding?)
                          (list (encoded-namerec string->utf16 0))
                          '())]
              [language-specific
               (if (and (fluid-ref name-table:platform-3-output-language-specific-encoding?)
                        (fluid-ref name-table:encoding-name))
                   (let* ([encoding-name (fluid-ref name-table:encoding-name)]
                          [encoding-id
                           (encoding-name->windows-encoding-id encoding-name)])
                     (if (not encoding-id)
                         '()
                         (encode-name-string-for-windows value encoding-name)))
                   '())])
         (append ucs2 ucs4 symbol language-specific))] ))

  (define (name-table->encoded-name-table-stage1 name-table)
    ;; Given name records for Windows platform, create name records
    ;; for the platforms to be outputted.
    `#(,(fold-left (lambda (diversified-nrecs nrec)
                     (append (diversify-name-record nrec) diversified-nrecs))
                   '()
                   (vector-ref name-table 0))))

  (define (name-table->encoded-name-table-stage2 name-table)
    ;; Given name records for multiple platforms, encode them.
    `#(,(fold-left (lambda (encoded-nrecs nrec)
                     (append (encode-name-record nrec) encoded-nrecs))
                   '()
                   (vector-ref name-table 0))))

  (define (name-table->encoded-name-table-default name-table)
    (name-table->encoded-name-table-stage2
     (name-table->encoded-name-table-stage1 name-table)))

  (define name-table->encoded-name-table-fluid
    (make-fluid name-table->encoded-name-table-default))

  (define/kwargs (name-table->encoded-name-table name-table)
    ((fluid-ref name-table->encoded-name-table-fluid) name-table))

   ;;-------------------------------------------------------------------------

  (define/kwargs (encoded-name-table:write-to-sfnt port/fd encoded-name-table)
    (let* ([port (if (port? port/fd) port/fd (fdes->outport port/fd))]
           [encoded-namerecs (vector-ref encoded-name-table 0)]
           [namerec-count (length encoded-namerecs)]
           [langtags (unique-langtags encoded-namerecs)]
           [encoded-langtags (map (cut string->utf16 <> (endianness big))
                                  langtags)]
           [table-format (if (null? langtags) 0 1)]
           [langtagrec-count (length langtags)]
           [langtag-id (lambda (tag)
                         (+ #x8000
                            (list-index (cut string=? tag <>) langtags)))]
           [namerec-size 12]
           [namerecs-total-size (* namerec-size namerec-count)]
           [nrec-str-offsets (namerec-string-offsets encoded-namerecs)]
           [namerec-str-offsets (car nrec-str-offsets)]
           [namerec-strings-size (cadr nrec-str-offsets)]
           [langtagrec-size 4]
           [langtagrecs-total-size (* langtagrec-size langtagrec-count)]
           [ltag-str-offsets (langtag-string-offsets encoded-langtags)]
           [langtag-str-offsets (map (cut + namerecs-total-size <>)
                                     (car ltag-str-offsets))]
           [string-offset (case table-format
                            [(0) (+ 6 namerecs-total-size)]
                            [(1) (+ 8 namerecs-total-size langtagrecs-total-size)])])
      (ot:write-USHORT port table-format)
      (ot:write-USHORT port namerec-count)
      (ot:write-USHORT port string-offset)
      (for-each
       (lambda (nrec str-offset)
         (match nrec
           [#(platform-id language-id name-id value encoding-id)
            (ot:write-USHORT port platform-id)
            (ot:write-USHORT port encoding-id)
            (ot:write-USHORT port (if (string? language-id)
                                      (langtag-id language-id)
                                      language-id))
            (ot:write-USHORT port name-id)
            (ot:write-USHORT port (bytevector-length value))
            (ot:write-USHORT port str-offset)]))
       encoded-namerecs namerec-str-offsets)
      (when (= table-format 1)
        (ot:write-USHORT port langtagrec-count))
      (for-each
       (lambda (encoded-tag str-offset)
         (ot:write-USHORT port (bytevector-length encoded-tag))
         (ot:write-USHORT port str-offset))
       encoded-langtags langtag-str-offsets)
      (for-each
       (match-lambda [#(_ _ _ value _) (ot:write-string port value)])
       encoded-namerecs)
      (for-each (cut ot:write-string port <>) encoded-langtags)))

  (define (unique-langtags namerecs)
    (without-adjacent-duplicates
     string=?
     (list-sort string<?
                (filter-map (lambda (nrec)
                              (let ([language-id (vector-ref nrec 1)])
                                (if (string? language-id) language-id #f)))
                            namerecs))))

  (define (namerec-string-offsets encoded-namerecs)
    (define (nrecs->offsets offsets total-offset nrecs)
      (match nrecs
        [() (list (reverse offsets) total-offset)]
        [(#(platform-id language-id name-id value encoding-id) . more-nrecs)
         (nrecs->offsets (cons total-offset offsets)
                         (+ total-offset (bytevector-length value))
                         more-nrecs)]))
    (nrecs->offsets '() 0 encoded-namerecs))

  (define (langtag-string-offsets encoded-langtags)
    (define (tags->offsets offsets total-offset tags)
      (match tags
        [() (list (reverse offsets) total-offset)]
        [(tag . more-tags)
         (tags->offsets (cons total-offset offsets)
                         (+ total-offset (bytevector-length tag))
                         more-tags)]))
    (tags->offsets '() 0 encoded-langtags))

  ) ;; end of library.
