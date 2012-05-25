;;; type-enum.lisp ---
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

(cl:in-package :rosetta.model.data)


;;; `base-enum-value' class
;;

(defclass enum-value (named-mixin ;; named-component-mixin
		      singleton ;;; TODO(jmoringe, 2012-05-03): singleton-mixin
		      documentation-mixin
		      print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))


;;; `base-enum' class
;;

(define-composite-mixin value
    :class-name values-mixin)

(defclass enum (named-mixin
		parented-mixin
		typed-mixin
		ordered-mixin
		values-mixin
		;; toplevel-mixin
		documentation-mixin
		print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod (setf lookup) :before ((new-value t)
				  (container enum)
				  (kind      (eql :value))
				  (key      string)
				  &key &allow-other-keys)
  (unless (compute-applicable-methods #'value (list new-value))
    (error "~@<Supplied value ~A for ~A does not specialize the ~S method.~@:>"
	   new-value container 'value)))
