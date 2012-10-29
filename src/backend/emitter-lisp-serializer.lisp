;;; emitter-serializer-base-lisp.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
