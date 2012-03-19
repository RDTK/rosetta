;;; conditions.lisp --- Conditions used in the model.data module.
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

(define-condition data-type-error (error)
  ((type :initarg  :type
	 :reader   data-type-error-type
	 :documentation
	 ""))
  (:documentation
   "This error condition class is intended to used as a superclass for
type-related error condition classes."))

(define-condition no-such-child (data-type-error)
  ((key :initarg  :key
	:reader   data-type-error-key
	:documentation
	""))
  (:report
   (lambda (condition stream)
     (format stream "~@<The requested child type ~S could not be found ~
within the type ~S.~@:>"
	     (data-type-error-key  condition)
	     (data-type-error-type condition))))
  (:documentation
   "This error is signaled when a requested child type cannot be found
within the specified container type."))
