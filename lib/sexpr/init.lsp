;; Copyright (c) 2013-2014 Free Software Foundation, Inc.
;; Written by Gary V. Vaughan, 2013
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Fifth Floor, 51 Franklin Street, Boston,
;; MA 02111-1301, USA.

(defmacro defun (name params body)
  (setq name (lambda params body)))

(defmacro or (a b)
  (if a a b))

(defmacro and (a b)
  (if a b nil))

(defun <= (x y)
  (or (< x y) (eq x y)))

(defun > (x y)
  (< y x))

(defun >= (x y)
  (<= y x))

(defun nullp (x)
  (eq x nil))
