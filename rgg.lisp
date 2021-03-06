;;; rgg.lisp --- random geometric graphs with gnuplot display

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
(load "polar.lisp")
(defpackage #:rgg (:use :common-lisp :polar :sb-thread))
(in-package :rgg)

;;; world
(defvar *field-radius* 0.5)            ; size of the field
(defvar *r* 0.1)                       ; connectivity radius
(defvar *s* 0.01)                      ; step length (speed)
(defvar *m* 8)                         ; expected steps between change
(defvar *vertices* '())

(defvar *connected-for* 0)

(defun rand-rect ()
  (make-instance 'rect
    :x (- (random (* 2 *field-radius*)) *field-radius*)
    :y (- (random (* 2 *field-radius*)) *field-radius*)))

(defclass  vertex ()
  ((id     :initarg :id     :accessor id)
   (place  :initarg :place  :accessor place)
   (theta  :initarg :theta  :accessor theta)))

(defun rand-vertex (id)
  (make-instance 'vertex
    :id    id
    :place (rand-rect)
    :theta (random tau)))

(defmethod connected? ((a vertex) (b vertex))
  (>= *r* (distance (place a) (place b))))

(defmacro update (dim)
  "keep within the field"
  `(let ((edge ,(* 2 *field-radius*)))
     (when (> (abs ,dim) *field-radius*)
       (setf ,dim (if (> ,dim 0) (- ,dim edge) (+ ,dim edge))))))

(defmethod move ((v vertex))
  (when (< (random 1.0) (/ 1 *m*)) (setf (theta v) (random tau))) ; change dir
  (let ((step (to-rect (make-instance 'polar :r *s* :theta (theta v))))) ; step
    (setf (place v) (add step (place v)))
    (update (x (place v)))
    (update (y (place v)))))

(defun populate (n)
  (setq *vertices* nil)
  (dotimes (id n) (push (rand-vertex id) *vertices*)))

(defun edges ()
  "return the connected graph of the vertices"
  (flet ((cross (as bs) (mapcan (lambda (a) (mapcar (lambda (b) (cons a b)) bs)) as)))
    (remove-if (lambda (pair) (not (connected? (car pair) (cdr pair))))
               (remove-duplicates
                (mapcar (lambda (pair) (if (< (id (car pair)) (id (cdr pair)))
                                      (cons (cdr pair) (car pair)) pair))
                        (cross *vertices* *vertices*))))))

;;; graph stuff
(defun neighbors (edges vert)
  "Return the neighbors of a vertex"
  (remove-duplicates
   (apply #'append
          (mapcar (lambda (pair) (list (car pair) (cdr pair)))
                  (remove-if (lambda (edge) (not (or (eq vert (car edge))
                                                (eq vert (cdr edge)))))
                     edges)))))

(defun connected-to- (edges already new)
  (if (null new)
      already
      (let* ((reachable (remove-duplicates
                         (mapcan (lambda (v) (neighbors edges v)) new)))
             (new (set-difference reachable already)))
        (connected-to- edges (append new already) new))))

(defun connected-to (edges vert)
  (connected-to- edges (list vert) (list vert)))

(defun full-cnn ()
  (= (length (connected-to (edges) (first *vertices*)))
     (length *vertices*)))

(defvar *gnuplot-counter* 0)

(defun gnuplot (&key (stream t))
  "Plot a series of polar or rectangular coordinates using gnuplot."
  (let ((n (length *vertices*))
        (e (exp 1.0d0)))
    ;; ;; used to create a video
    ;; (format stream "set term png~%")
    ;; (format stream "set output 'plots/~a.png'~%" *gnuplot-counter*)
    (incf *gnuplot-counter*)
    (format stream "set title 'n=~d s=~f m=~f r=~f u=~5,4f C=~d'~%"
            n *s* *m* *r*
            (* n (expt e (* (- 0 Pi) (expt *r* 2) n)))
            *connected-for*))
  (format stream "~&unset arrow~%")
  ;; edges
  (dolist (edge (edges))
    (let ((from (place (car edge)))
          (to  (place  (cdr edge))))
      (format stream "~&set arrow from ~f,~f to ~f,~f nohead~%"
              (x from) (y from) (x to) (y to))))
  ;; points
  (format stream "~&plot '-' notitle~%~T~{~a~%~T~}e~%"
          (mapcar (lambda (place) (format nil "~f~T~f" (x place) (y place)))
                  (mapcar #'place *vertices*))))

(defun run-to (path)
  (make-thread
   (lambda ()
     (with-open-file (out path :direction :output :if-exists :append)
       (format out "set xrange [~f:~f]~%" (- 0 *field-radius*) *field-radius*)
       (format out "set yrange [~f:~f]~%" (- 0 *field-radius*) *field-radius*)
       (loop while t
          do (mapc #'move *vertices*)
            (if (full-cnn)
                (if (>= *connected-for* 0)
                    (incf *connected-for*)
                    (setf *connected-for* 0))
                (if (<= *connected-for* 0)
                    (decf *connected-for*)
                    (setf *connected-for* 0)))
            (gnuplot :stream out))))
   :name "rgg"))

;; To start gnuplot running
;;  1. mkfifo /tmp/feedgnuplot
;;  2. gnuplot < /tmp/feedgnuplot
;;  3. (populate 20)
;;  4. (setq *gnuplot* (run-to #P"/tmp/feedgnuplot"))
;;
;; while running you can continue to setf variables at the REPL
;;
;; To stop gnuplot running
;;  1. (terminate-thread *gnuplot*)
