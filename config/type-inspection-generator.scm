#! /usr/bin/guile \
--no-auto-compile -s
!#

;; Example input:
;;
;;   ;; Includes.
;;   ("<stdbool.h>"
;;    "splinefont.h")
;;   ;; Instructions.
;;   ((sizeof "bool *" "boolptr_t")
;;    (struct "SplineChar")
;;    (sizeof "SplineChar")
;;    (field bool "SplineChar" "changed")
;;    (field int "SplineChar" "italic_correction" "italcorr"))

(use-modules
   (ice-9 match)
   (ice-9 regex)
   (ice-9 format))

(define (write-c-source includes types)
   (format #t "#include <config.h>\n\n")
   (format #t "#include <stdio.h>\n")
   (format #t "#include <stddef.h>\n")
   (format #t "\n")
   (write-includes includes)
   (format #t "\n")
   (format #t "int\nmain (int argc, char **argv)\n{\n")
   (write-type-info types)
   (format #t "  return 0;\n}\n")
   )

(define angle-brackets-re (make-regexp "<.*>"))
(define quotes-re (make-regexp "\".*\""))

(define (write-includes includes)
   (match includes
      ;; Nothing or an empty list.
      ((or #f ()) '())

      ;; A list of includes.
      (((? string? h) . t) (write-includes h)
                           (write-includes t))

      ;; A single include.
      ((? string? inc)
       (cond
          ((or
            (regexp-exec angle-brackets-re inc)
            (regexp-exec quotes-re inc))
           (format #t "#include ~a\n" inc))
          (else
           (format #t "#include \"~a\"\n" inc))))

      (inc (error "Unrecognized includes:" inc))))

(define (write-type-info instructions)
   (match instructions
      ;; Nothing or an empty list.
      ((or #f ()) '())

      ;; A list of instructions.
      (((and (_ . _) h) . t) (write-type-info h)
                             (write-type-info t))

      ; Example: (sizeof "bool *" "boolptr_t")
      (('sizeof (? string? type-name) (? string? replacement-name))
       (format #t
               "  printf (\"(sizeof \\\"~a\\\" %zu)\\n\", sizeof (~a));\n"
               replacement-name type-name))

      ;; Example: (sizeof "SplineChar")
      (('sizeof (? string? type-name))
       (write-type-info (list 'sizeof type-name type-name)))

      ;; Example: (field int "struct splinechar" "italic_correction" "SplineChar" "italcorr")
      ;; For structs and unions.
      (('field (? symbol? field-type)
               (? string? struct-name)
               (? string? field-name)
               (? string? replacement-struct-name)
               (? string? replacement-field-name))
       (format #t "  {\n")
       (format #t "    ~a x;\n" struct-name)
       (format #t "    printf (\"(field ~a \\\"~a\\\" \\\"~a\\\" %zu %zu)\\n\",\n"
               field-type replacement-struct-name replacement-field-name)
       (format #t "            offsetof (~a, ~a),\n" struct-name field-name)
       (format #t "            sizeof (x.~a));\n" field-name)
       (format #t "  }\n"))

      ;; Example: (field int "struct splinechar" "italic_correction" "italcorr")
      (('field (? symbol? field-type)
               (? string? struct-name)
               (? string? field-name)
               (? string? replacement-field-name))
       (write-type-info (list 'field field-type struct-name field-name
                              struct-name replacement-field-name)))

      ;; Example: (field int "SplineChar" "italic_correction")
      (('field (? symbol? field-type)
               (? string? struct-name)
               (? string? field-name))
       (write-type-info (list 'field field-type struct-name field-name
                              struct-name field-name)))

      ;; Example: (struct "struct splinechar" "SplineChar" (sizeof) (field "italic_correction"))
      (((and struct-or-union (or 'struct 'union))
        (? string? struct-name)
        (? string? replacement-struct-name) . t)
       (format #t "  printf (\"(~a \\\"~a\\\" %zu)\\n\", sizeof (~a));\n"
               struct-or-union replacement-struct-name struct-name)
       (for-each (lambda (sub-instruction)
                   (write-type-info
                    (insert-struct-names struct-name
                                         replacement-struct-name
                                         sub-instruction)))
                 t))

      ;; Example: (struct "SplineChar" (sizeof) (field "italic_correction"))
      (((and struct-or-union (or 'struct 'union))
        (? string? struct-name) . t)
       (write-type-info
        (cons* struct-or-union struct-name struct-name t)))

      (instr (error "Unrecognized instructions:" instr))))

(define (insert-struct-names struct-name
                             replacement-struct-name
                             sub-instruction)
  (match sub-instruction
         (('sizeof) (list 'sizeof struct-name replacement-struct-name))
         (('field (? symbol? field-type)
                  (? string? field-name)
                  (? string? replacement-field-name))
          (list 'field field-type struct-name field-name
                replacement-struct-name replacement-field-name))
         (('field (? symbol? field-type)
                  (? string? field-name))
          (list 'field field-type struct-name field-name
                replacement-struct-name field-name)) ))

(let* ((includes (read))
       (instructions (read)))
   (write-c-source includes instructions))
