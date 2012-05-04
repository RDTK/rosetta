;;; forward-reference.lisp ---
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

(cl:in-package :rosetta.model.data)

(defclass forward-reference ()
  ((kind :initarg  :kind
	 ;; :type     symbol
	 :accessor kind
	 :documentation
	 "")
   (args :initarg  :args
	 :type     list
	 :accessor args
	 :documentation
	 ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod upgrade! ((instance forward-reference)
		     (other    t))
  "TODO(jmoringe): document"
  (let ((new-class (class-of other)))
    (apply #'change-class instance new-class nil)
    (iter (for slot in (closer-mop:class-slots new-class))
	  (let ((name (closer-mop:slot-definition-name slot)))
	    (setf (slot-value instance name)
		  (slot-value other name))))
    instance))

(defmethod print-object ((object forward-reference) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
	    (kind object)
	    (when (>= (length (args object)) 2)
	      (second (args object))))))
