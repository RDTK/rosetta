;;; type-structure.lisp ---
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

(cl:in-package :rosetta.model.data)


;;; `base-field' class
;;


(defclass base-field (named-component-mixin
		      documentation-mixin
		      print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))


;;; `base-structure' class
;;

(defclass base-structure (named-mixin
			  parented-mixin
			  ordered-mixin
			  structure-mixin
			  toplevel-mixin
			  documentation-mixin
			  print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   base-structure)
                                     (slot-names t)
                                     &key
				     (fields nil fields-supplied?))
  ;; etypecase?
  (cond
    ;; nil
    ((not fields-supplied?))

    ;; plist
    ((and (listp fields) (stringp (first fields)))
     (setf (%children instance)
	   (iter (for (name field) on fields :by #'cddr)
		 (collect field))))

    ;; sequence of named child instances
    ((and (typep fields 'sequence)
	  (or (emptyp fields)
	      (typep (elt fields 0) 'field-mixin))) ;;; TODO(jmoringe, 2012-04-12):
     (setf (%children instance) (coerce fields 'list)))

    (t
     (error 'type-error
	    :datum         fields
	    :expected-type 'sequence))))
