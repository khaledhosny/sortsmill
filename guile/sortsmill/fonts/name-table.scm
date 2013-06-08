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
   )

  (import (sortsmill strings ietf-language-tags)
          (sortsmill kwargs)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) take)
          (only (srfi :26) cut))

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
  ;; The platform-id field currently always is a Guile integer equal
  ;; to 3, meaning the ‘Windows’ platform.
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
  ;;-------------------------------------------------------------------------

  (define/kwargs (name-table-set! name-table value name-id
                                  [language #x0409] ; US English, for platform=3 (Windows).
                                  [platform 3])     ; Microsoft Windows.
    (assert (string? value))
    (assert-platform-supported 'name-table-set! platform)
    (let ([lst-without-old-entry
           (remp (lambda (entry)
                   (name-record-keys-match?
                    'name-table-set! entry name-id language platform))
                 (vector-ref name-table 0))]
          [new-entry `#(,platform
                        ,(reformat-ietf-language-tag language)
                        ,name-id ,value)])
      (vector-set! name-table 0 (cons new-entry lst-without-old-entry))))

  (define/kwargs (name-table-ref name-table name-id
                                 [language #x0409] ; US English, for platform=3 (Windows).
                                 [platform 3])     ; Microsoft Windows.
    (assert-platform-supported 'name-table-ref platform)
    (assp (lambda (entry) (name-record-keys-match?
                           'name-table-ref entry name-id language platform))
          (vector-ref name-table 0)))

  (define (name-record-keys-match? who name-record name-id language platform)
    (if (and (eqv? name-id (vector-ref name-record 2))
             (eqv? platform (vector-ref name-record 3)))
        (cond [(integer? language)
               (eqv? language (vector-ref name-record 1))]
              [(string? language)
               (ietf-language-tag=? language (vector-ref name-record 1))]
              [else
               (assertion-violation who (_ "expected an integer or string")
                                    platform)])
        #f))

  (define (assert-platform-supported who platform)
    (unless (eqv? platform 3)
      (assertion-violation who
                           (_ "only platform 3 (Windows) is supported")
                           platform)))

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

  ) ;; end of library.
