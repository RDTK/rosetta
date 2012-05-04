;;; package1.lisp ---
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

(defclass package1 (named-mixin
		    parented-mixin
		    forward-reference-upgrader-mixin
		    composite-mixin
		    print-items-mixin)
  ((table :type     hash-table
	  :reader   %table
	  :initform (make-hash-table :test #'equal)
	  :documentation
	  ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod qname ((package package1))
  (if-let ((parent (parent package)))
    (append (qname parent) (list (data-type-name package)))
    (list :absolute)))

(defmethod contents ((package package1) (kind (eql :nested)))
  (hash-table-values (%table package)))

(defmethod contents/plist ((package package1))
  (hash-table-plist (%table package)))

(defmethod lookup ((package package1)
		   (kind    symbol)
		   (name    string)
		   &key &allow-other-keys)
  (values (gethash (cons kind name) (%table package))))

(defmethod (setf lookup) ((new-value t)
			  (package   package1)
			  (kind      symbol)
			  (name      string)
			  &key &allow-other-keys)
  (setf (gethash (cons kind name) (%table package)) new-value))
