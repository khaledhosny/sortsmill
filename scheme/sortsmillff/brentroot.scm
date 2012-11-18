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

;;
;; FIXME: Rename these functions and use keyword parameters with
;; settings to select implementation, maybe including an 'auto mode
;; that chooses the C implementation if t1 or t2 is ‘inexact’.
;;

(define-module (sortsmillff brentroot)
  #:use-module ((rnrs) :version (6))
  #:use-module (sortsmillff math-constants)
  #:export (flbrentroot
            flbrentroot-values
            brentroot
            brentroot-values))

(load-extension "libguile-sortsmillff_brentroot"
                "init_guile_sortsmillff_brentroot")

;;-------------------------------------------------------------------------

(define* (flbrentroot-values t1 t2 func #:key (max-iters -1) (tol -1))
  (f64-brentroot max-iters tol t1 t2 func))

(define* (flbrentroot t1 t2 func #:key (max-iters -1) (tol -1))
  (let-values (((root err iter-no)
                (f64-brentroot max-iters tol t1 t2 func)))
    root))

;;-------------------------------------------------------------------------

(define (bracketed f1 f2)
  (or (and (<= f1 0) (<= 0 f2))
      (and (<= f2 0) (<= 0 f1))))

(define (bisection a b)
  (/ (- a b) 2))

(define (linear s fa fb)
  (let ((fba (/ fb fa)))
    (values (* fba 2 s)
            (- 1 fba))))

(define (inverse-quadratic s a fa b fb fc)
  (let ((fbc (/ fb fc))
        (fba (/ fb fa))
        (fac (/ fa fc)))
    (values (* fba
               (- (* 2 s fac (- fac fbc))
                  (* (- b a) (1- fbc))))
            (* (1- fac) (1- fba) (1- fbc)))))

(define (interpolate a fa b fb fb1 step step1 tolerance)
  (let*-values (((s) (bisection a b))
                ((p q)
                 (let-values (((pp qq)
                               (if (or (= fb1 fa) (= fb1 fb))
                                   (linear s fa fb)
                                   (inverse-quadratic s a fa b fb fb1))))
                   (if (positive? pp)
                       (values pp (- qq))
                       (values (- pp) qq)))))
    (if (< (* 2 p) (min (- (* 3 s q) (abs (* tolerance q)))
                        (abs (* step1 q))))
        (values (/ p q) step)
        (values s s))))

(define* (brentroot-values t1 t2 func
                           #:key (max-iters -1) (tol -1)
                           (epsilon c-dbl-epsilon-exact))
  (let ((max-iters (if (negative? max-iters) 1000000 max-iters))
        (tol (if (negative? tol) c-dbl-epsilon-exact tol)) ; FIXME: Is the default appropriate?
        (a t1)
        (b t2)
        (fa (func t1))
        (fb (func t2)))
    (if (not (bracketed fa fb))
        (values #f 1 0)         ; err == 1 means 'root not bracketed'.
        (letrec ((iter
                  (lambda (iter-no
                           a fa      ; Point at which func is larger.
                           b fb      ; Point at which func is smaller.
                           b1 fb1    ; Earlier values of b, fb.
                           step step1)  ; Last two step sizes.
                    (if (<= max-iters iter-no)
                        (values #f 2 iter-no) ; err == 2 means maximum
                                              ; iterations exceeded.
                        (let ((tolerance (+ (* 2 epsilon (abs b)) (/ tol 2))))
                          (if (or (<= (abs step) tolerance) (zero? fb))
                              (values b 0 iter-no) ; The result.
                              (let*-values (((new-step old-step)
                                             (if (or (< (abs step1) tolerance)
                                                     (<= (abs fa) (abs fb)))
                                                 ;; Interpolation is stepping too slowly.
                                                 (let ((s (bisection a b)))                                                
                                                   (values s s))
                                                 (interpolate a fa b fb fb1
                                                              step step1 tolerance)))
                                            ((guess)
                                             (cond ((< tolerance (abs new-step))
                                                    (+ b new-step))
                                                   ((negative? new-step) (- b tolerance))
                                                   (else (+ b tolerance))))
                                            ((fguess) (func guess)))
                                (if (bracketed fb fguess)
                                    (if (< (abs fguess) (abs fb))
                                        (iter (1+ iter-no) b fb guess fguess b fb
                                              new-step old-step)
                                        (iter (1+ iter-no) guess fguess b fb b fb
                                              new-step old-step))
                                    (if (< (abs fguess) (abs fa))
                                        (iter (1+ iter-no) a fa guess fguess b fb
                                              (- guess a) (- guess a))
                                        (iter (1+ iter-no) guess fguess a fa b fb
                                              (- guess a) (- guess a)))))))))))
          (if (< (abs fa) (abs fb))
              (iter 0 b fb a fa b fb (- b a) (- b a))
              (iter 0 a fa b fb a fa (- a b) (- a b)))))))

(define* (brentroot t1 t2 func
                    #:key (max-iters -1) (tol -1)
                    (epsilon c-dbl-epsilon-exact))
  (let-values
      (((root err iter-no)
        (brentroot-values t1 t2 func #:max-iters max-iters #:tol tol
                          #:epsilon epsilon)))
    root))

;;-------------------------------------------------------------------------
