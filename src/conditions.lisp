;;; conditions.lisp --- Basic conditions used in cl-rosetta.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rosetta)


;;; Program error conditions
;;

(define-condition missing-required-argument (program-error)
  ((parameter :initarg  :parameter
	      :type     symbol
	      :accessor missing-required-argument-parameter
	      :documentation
	      "The parameter for which a value should have been
supplied."))
  (:report
   (lambda (condition stream)
     (format stream "~@<No value has been supplied for the required ~
parameter ~S.~@:>"
	     (missing-required-argument-parameter condition))))
  (:documentation
   "This error is signaled when no value is supplied for a required
parameter."))

(defun missing-required-argument (parameter)
  "Signal a `missing-required-argument' error for PARAMETER."
  (error 'missing-required-argument
	 :parameter parameter))

(define-condition missing-required-initarg (missing-required-argument)
  ((class   :initarg  :class
	    :type     symbol
	    :accessor missing-required-initarg-class
	    :documentation
	    "The class which requires the missing initarg."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The initarg ~S is required by class ~S, but ~
has not been supplied.~@:>"
	     (missing-required-argument-parameter condition)
	     (missing-required-initarg-class      condition))))
  (:documentation
   "This error is signaled when an initarg that is required by a class
is not supplied."))

(defun missing-required-initarg (class initarg)
  "Signal a `missing-required-initarg' error for CLASS and INITARG."
  (error 'missing-required-initarg
	 :parameter initarg
	 :class     class))
