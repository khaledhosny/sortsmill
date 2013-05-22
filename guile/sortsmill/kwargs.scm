;; (sortsmill kwargs) -- an alternate implementation of keyword arguments
;;
;; Adapted from (scheme kwargs), which is part of guile-lib.
;;
;; Copyright (C) 2013 Khaled Hosny and Barry Schwartz
;; Copyright (C) 2003, 2004, 2007 Andy Wingo <wingo at pobox dot com>
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

;;; Commentary:
;; 
;; @c
;; Support for defining functions that take python-like keyword
;; arguments.
;; 
;; In one of his early talks, Paul Graham wrote about a large system
;; called "Rtml":
;;
;; @quotation
;;
;; Most of the operators in Rtml were designed to take keyword
;; parameters, and what a help that turned out to be. If I wanted to add
;; another dimension to the behavior of one of the operators, I could
;; just add a new keyword parameter, and everyone's existing templates
;; would continue to work. A few of the Rtml operators didn't take
;; keyword parameters, because I didn't think I'd ever need to change
;; them, and almost every one I ended up kicking myself about later. If
;; I could go back and start over from scratch, one of the things I'd
;; change would be that I'd make every Rtml operator take keyword
;; parameters.
;;
;; @end quotation
;;
;; @xref{scheme kwargs lambda/kwargs,,lambda/kwargs}, for documentation
;; and examples.
;;
;; @xref{Optional Arguments,,,guile,Guile Reference Manual}, for more
;; information on Guile's standard support for optional and keyword
;; arguments.
;; 
;; Quote taken from
;; @uref{http://lib.store.yahoo.net/lib/paulgraham/bbnexcerpts.txt}.
;;
;;; Code:

(library (sortsmill kwargs)
  
  (export lambda/kwargs define/kwargs)

  (import (sortsmill i18n)
          (rnrs)
          (only (guile) keyword?)
          (only (srfi :1) break)
          (only (srfi :26) cut)
          (ice-9 optargs))

  (define-syntax lambda/kwargs
    (lambda (stx)
      "Defines a function that takes keyword arguments.

@var{bindings} is a list of bindings, each of which may either be a
symbol or a two-element symbol-and-default-value list. Symbols without
specified default values will default to @code{#f}.

For example:
@example
 (define frobulate (lambda/kwargs (foo (bar 13) (baz 42))
                     (list foo bar baz)))
 (frobulate) @result{} (#f 13 42)
 (frobulate #:baz 3) @result{} (#f 13 3)
 (frobulate #:foo 3) @result{} (3 13 42)
 (frobulate 3 4) @result{} (3 4 42)
 (frobulate 1 2 3) @result{} (1 2 3)
 (frobulate #:baz 2 #:bar 1) @result{} (#f 1 2)
 (frobulate 10 20 #:foo 3) @result{} (3 20 42)
@end example

This function differs from the standard @code{lambda*} provided by Guile
in that invoking the function will accept positional arguments.
As an example, the @code{lambda/kwargs} behaves more intuitively in the
following case:

@example
 ((lambda* (#:optional (bar 42) #:key (baz 73))
    (list bar baz))
  1 2) @result{} (1 73)
 ((lambda/kwargs ((bar 42) (baz 73))
    (list bar baz))
  1 2) @result{} (1 2)
@end example

The fact that @code{lambda*} accepts the extra @samp{2} argument is
probably just a bug. In any case, @code{lambda/kwargs} does the right
thing.

Please keep in mind that, in Guile~2.0, @code{lambda*} is specially
optimized, whereas @code{lambda/kwargs} defined here is not.
"
      (syntax-case stx ()
        [(_ bindings-syntax body body* ...)
         (let ([bindings (syntax->datum #'bindings-syntax)])
           (unless (list? bindings)
             (assertion-violation 'lambda/kwargs
                                  (_ "lambda/kwargs bindings must be a list")
                                  bindings))
           (let ([d->s (cut datum->syntax stx <>)]
                 [nbindings (length bindings)]
                 [canonical-bindings
                  (map (lambda (x) (if (list? x) x (list x #f))) bindings)]
                 [variables
                  (map (lambda (x) (if (list? x) (car x) x)) bindings)])
             #`[lambda args
                 (let-values ([(positional keyword) (break keyword? args)])
                   (when (< #,(d->s nbindings) (length positional))
                     (assertion-violation
                      'lambda/kwargs (_ "too many positional arguments") args))
                   (let-optional
                    positional #,(d->s canonical-bindings)
                    (let-keywords
                     keyword #f #,(d->s (map list variables variables))
                     body body* ...)))] ))] )))

  (define-syntax define/kwargs
    (lambda (stx)
      "Defines a function that takes keyword arguments.

@example{(define/kwargs (function-name binding ...) body ...)}
is equivalent to
@example{(define function-name (lambda/kwargs (binding ...) body ...))}
"
      (syntax-case stx ()
        [(_ func&bindings-syntax body body* ...)
         (let ([func&bindings (syntax->datum #'func&bindings-syntax)])
           (unless (list? func&bindings)
             (assertion-violation 'define/kwargs
                                  (_ "define/kwargs bindings must be a list")
                                  func&bindings))
           (unless (if (null? func&bindings) #f (symbol? (car func&bindings)))
             (assertion-violation 'define/kwargs
                                  (_ "define/kwargs bindings must start with the function name")
                                  func&bindings))
           (let* ([d->s (cut datum->syntax stx <>)]
                  [func (car func&bindings)]
                  [bindings (cdr func&bindings)]
                  [nbindings (length bindings)]
                  [canonical-bindings
                   (map (lambda (x) (if (list? x) x (list x #f))) bindings)]
                  [variables
                   (map (lambda (x) (if (list? x) (car x) x)) bindings)])
             #`[define (#,(d->s func) . args)
                 (let-values ([(positional keyword) (break keyword? args)])
                   (when (< #,(d->s nbindings) (length positional))
                     (assertion-violation
                      'define/kwargs (_ "too many positional arguments") args))
                   (let-optional
                    positional #,(d->s canonical-bindings)
                    (let-keywords
                     keyword #f #,(d->s (map list variables variables))
                     body body* ...)))] ))] )))

  ) ;; end of library.
