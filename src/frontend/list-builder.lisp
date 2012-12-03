;;; list-builder.lisp --- Builder producing lists.
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

(cl:in-package :rosetta.frontend)

;;; Format used by the `list' builder
;;
;; The output of the builder is list-based tree in which each node is
;; a list of one of the two forms:
;;
;; 1. (KIND         CHILDREN PLIST)
;; 2. ((:find KIND) nil      PLIST)
;;
;; Form 1. is produced by calls to `make-node' and form 2. is produced
;; by calls to `find-node'. In both forms, PLIST is a plist of the
;; keyword arguments passed to `make-node'/`find-node'. Calls to
;; `add-child' modify nodes of form 1. by destructively adding child
;; nodes to CHILDREN.

(defmethod kind ((thing list))
  (etypecase (first thing)
    ((cons (eql :find))
     (second (first thing)))
    (keyword
     (first thing))))

(macrolet ((define-fundamental-accessor (name
					 &key
					 (keyword (make-keyword name))
					 (writer? nil))
	     `(progn
		(defmethod ,name ((thing list))
		  (getf (cddr thing) ,keyword))

		,@(when writer?
		    `((defmethod (setf ,name) ((new-value t) (thing list))
			(setf (getf (cddr thing) ,keyword) new-value)))))))

  (define-fundamental-accessor name)
  (define-fundamental-accessor qname)
  (define-fundamental-accessor documentation1
    :writer? t)
  (define-fundamental-accessor category)
  (define-fundamental-accessor width)
  (define-fundamental-accessor signed?)
  (define-fundamental-accessor type1
    :keyword :type)
  (define-fundamental-accessor element-type)
  (define-fundamental-accessor index-type))

(defmethod validate-value ((type list) (value t)
			   &key &allow-other-keys)
  ;; This method exists mainly for the sake of unit tests.
  (when (eq (kind type) :fundamental)
    (when-let ((type-spec
		(case (category type)
		  (:bool    'boolean)
		  (:integer
		   `(,(if (signed? type) 'signed-byte 'unsigned-byte)
		     ,(width type)))
		  (:float
		   `(real most-negative-double-float most-positive-double-float))
		  (:string  'string)
		  (:bytes   'nibbles:octet-vector))))
      (typep value type-spec))))

(defmethod find-node ((builder (eql 'list))
		      (kind    t)
		      &rest args &key &allow-other-keys)
  (let+ (((kind nil &rest args)
	  (apply #'make-node builder kind
		 (remove-from-plist args :if-does-not-exist))))
    (list* (list :find kind) nil args)))

(defmethod make-node :before ((builder (eql 'list))
			      (kind    (eql :singleton))
			      &key
			      type
			      value)
  ;; This method exists mainly for the sake of unit tests.
  (validate-value type value))

(defmethod make-node ((builder (eql 'list))
		      (kind    t)
		      &rest args &key)
  (list* kind nil args))

(defmethod add-child ((builder (eql 'list))
		      (parent  list)
		      (child   t))
  (appendf (second parent) (list child))
  parent)
