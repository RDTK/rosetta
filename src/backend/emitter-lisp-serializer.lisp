;;; emitter-serializer-base-lisp.lisp ---
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


;;; Serialization code for fundamental types
;;

(defmethod emit ((node     rs.m.d::fundamental-type-mixin)
		 (target   target-pack)
		 (language language-lisp)
		 &key)
  (let+ (((&accessors-r/o category width) node)
	 ((&env-r/o source-var offset-var destination-var))
	 (packer (%packer-name category width)))
    `(progn
       (setf (,packer ,destination-var ,offset-var) ,source-var)
       ,(emit node :packed-size language))))

(defmethod emit ((node     rs.m.d::fundamental-type-mixin)
		 (target   target-unpack)
		 (language language-lisp)
		 &key)
  (let+ (((&accessors-r/o category width) node)
	 ((&env-r/o source-var offset-var destination-var))
	 (packer (%packer-name category width)))
    `(,packer ,source-var ,offset-var)))


;;; Serialization-related methods
;;

(defmethod emit ((node     rs.m.d::toplevel-mixin)
		 (target   target-packed-size/method)
		 (language language-lisp)
		 &key)
  (let+ (((&env-r/o name))
	 ((&env mechanism-var source-var)))
    `(defmethod packed-size ((,mechanism-var mechanism-ros-msg)
			     (,source-var    ,name)
			     &key)
       ,(call-next-method))))

(defmethod emit ((node     rs.m.d::toplevel-mixin)
		 (target   target-pack/method)
		 (language language-lisp)
		 &key)
  (let+ (((&env-r/o name))
	 ((&env mechanism-var source-var destination-var start-var end-var)))
    `(defmethod pack ((,mechanism-var   mechanism-ros-msg)
		      (,source-var      ,name)
		      (,destination-var simple-array)
		      &key
		      ((:start ,start-var) 0)
		      ((:end   ,end-var)   (length ,destination-var)))
       (check-type ,destination-var nibbles:octet-vector)

       ,(call-next-method))))

(defmethod emit ((node     rs.m.d::toplevel-mixin)
		 (target   target-unpack/method)
		 (language language-lisp)
		 &key)
  (let+ (((&env-r/o name))
	 ((&env mechanism-var source-var destination-var start-var end-var)))
    `(defmethod unpack ((,mechanism-var   mechanism-ros-msg)
			(,source-var      simple-array)
			(,destination-var ,name)
			&key
			((:start ,start-var) 0)
			((:end   ,end-var)   (length ,source-var)))
       (check-type ,source-var nibbles:octet-vector "an octet-vector")

       ,(call-next-method))))


;;; Utility functions
;;

(defun %packer-name (category width)
  "Return the a name of a function in the nibbles package which can be
used to accesses the fundamental type characterized by CATEGORY and
WIDTH."
  (format-symbol :nibbles "~A~Aref/le"
		 (ecase category
		   (:float   :ieee-)
		   (:integer :ub))
		 (ecase category
		   (:float   (ecase width
			       (32 :single)
			       (64 :double)))
		   (:integer width))))
