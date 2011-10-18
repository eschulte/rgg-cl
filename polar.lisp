;;; polar.lisp --- polar and rectangular coordinates with conversion

;; Copyright (C) 2011  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defpackage #:polar
  (:use :common-lisp)
  (:export :tau :polar :print-object :to-rect
           :rect :to-polar
           :add :sub :distance
           :x :y :r :theta))
(in-package :polar)

(defconstant tau (* 2 Pi) "tau=2Pi is a more natural constant to use than Pi.")

(defclass polar ()
  ((r :initarg :r :accessor r)
   (theta :initarg :theta :accessor theta)))

(defmethod print-object ((polar polar) stream)
  (format stream "#polar(~a,~a)" (r polar) (theta polar)))

(defmethod to-rect ((polar polar))
  (with-slots (r theta) polar
    (make-instance 'rect
      :x (* r (cos theta))
      :y (* r (sin theta)))))

(defmethod add ((a polar) (b polar))
  (to-polar (add (to-rect a) (to-rect b))))

(defmethod sub ((a polar) (b polar))
  (to-polar (sub (to-rect a) (to-rect b))))

(defclass rect ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defmethod print-object ((rect rect) stream)
  (format stream "#rect(~a,~a)" (x rect) (y rect)))

(defmethod to-polar ((rect rect))
  (with-slots (x y) rect
    (make-instance 'polar
      :r (sqrt (+ (* x x) (* y y)))
      :theta (handler-case (atan y x) (error nil 0)))))

(defmethod add ((a rect) (b rect))
  (make-instance 'rect
    :x (+ (x a) (x b))
    :y (+ (y a) (y b))))

(defmethod sub ((a rect) (b rect))
  (make-instance 'rect
    :x (- (x a) (x b))
    :y (- (y a) (y b))))

(defmethod distance ((a rect) (b rect))
  (sqrt (+ (expt (- (x a) (x b)) 2)
           (expt (- (y a) (y b)) 2))))
