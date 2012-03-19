;;; textual-stream-mixin.lisp --- Mixin for textual mechanisms based on streams.
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

(cl:in-package :rosetta.serialization)

(defclass textual-stream-mixin (textual-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
implement textual mechanisms based on streams."))

(defmethod pack ((mechanism   textual-stream-mixin)
		 (source      t)
		 (destination (eql 'string))
		 &rest args
		 &key)
  "The value of DESTINATION indicates that a string should be created
as destination."
  (let ((result (with-output-to-string (stream)
		  (apply #'pack mechanism source stream args))))
    (values (length result) result)))

(defmethod unpack ((mechanism   textual-stream-mixin)
		   (source      pathname)
		   (destination t)
		   &rest args
		   &key)
  "Open a stream for SOURCE and unpack the contents into DESTINATION."
  (with-input-from-file (stream source)
    (apply #'unpack mechanism stream destination args)))
