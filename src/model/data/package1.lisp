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
		    container/relative-mixin
		    print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod kind ((type package1))
  :package)

(defmethod qname ((package package1))
  (if-let ((parent (parent package)))
    (append (qname parent) (list (data-type-name package)))
    (list :absolute)))

(defmethod contents/plist ((package package1))
  (hash-table-plist (%nested package)))

(defmethod lookup ((package package1)
		   (kind    symbol)
		   (name    list)
		   &key &allow-other-keys)
  (check-type name (or name/absolute name/relative))

  (typecase name
    ((and (cons (eql :relative) (cons string null)))
     (lookup package kind (second name)))

    ((and (cons (eql :relative) (cons string cons)))
     (lookup (lookup package :package (second name))
	     kind (cons :relative (nthcdr 2 name))))

    (name/absolute
     (lookup (root package) kind (cons :relative (rest name))))))
