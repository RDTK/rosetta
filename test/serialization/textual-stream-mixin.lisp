;;; textual-stream-mixin.lisp --- Unit tests for the textual-stream-mixin class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rosetta.serialization.test)


;;; Mock mechanism class
;;

(defmethod find-mechanism-class ((spec (eql :mock-for-textual-stream-mixin)))
  (find-class 'mechanism-mock-for-textual-stream-mixin))

(defclass mechanism-mock-for-textual-stream-mixin (textual-stream-mixin)
  ())

(defmethod pack ((mechanism   mechanism-mock-for-textual-stream-mixin)
		 (source      t)
		 (destination stream)
		 &key)
  (format destination "~S" source)
  (values nil destination))

(defmethod unpack ((mechanism   mechanism-mock-for-textual-stream-mixin)
		   (source      stream)
		   (destination t)
		   &key)
  (values (read source) nil))


;;; Test suite
;;

(deftestsuite textual-stream-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-textual-stream-mixin)))
  (:documentation
   "Root test suite for the `textual-stream-mixin' class."))

(define-basic-textual-mechanism-tests textual-stream-mixin)
