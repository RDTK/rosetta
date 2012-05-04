;;; target-mixins.lisp --- Mixins for target classes.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rosetta.backend)


;;; `code-generating-target-mixin' mixin class
;;

(defclass code-generating-target-mixin ()
  ((optimization-settings :initarg  :optimization-settings
			  :type     list
			  :accessor optimzation-settings
			  :initform nil
			  :documentation
			  "Optimization settings that should be used
when generating code."))
  (:documentation
   "This class can be used as a superclass for target classes that
represent a code generation target."))


;;; `method-target-mixin' mixin class
;;

(defclass method-target-mixin (code-generating-target-mixin)
  ((body-target :initarg  :body-target
		:reader   body-target
		:documentation
		"Stores a target object that should be used to
generate the method body."))
  (:default-initargs
   :body-target (missing-required-initarg
		 'method-target-mixin :body-target))
  (:documentation
   "This class is intended to be mixed into target classes which emit
methods."))

(defmethod mechanism ((target method-target-mixin))
  (mechanism (body-target target)))

(defmethod emit ((node     t)
		 (target   method-target-mixin)
		 (language t)
		 &key)
  (emit node (body-target target) language))


;;; `mechanism-target-mixin' mixin class
;;

(defclass mechanism-target-mixin ()
  ((mechanism :initarg  :mechanism
	      :reader   mechanism
	      :documentation
	      "Stores the name of the mechanism class for which the
serialization-related code is `emit' ted."))
  (:default-initargs
   :mechanism (missing-required-initarg
	       'mechanism-target-mixin :mechanism))
  (:documentation
   "This class is intended to be mixed into target classes which are
used for generating serialization-related code. It provides a slot for
storing the mechanism for which the serialization code is `emit'
ted."))
