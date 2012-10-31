;;; model-builder.lisp --- Builder for rosetta.model.data objects.
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

(defmethod find-builder-class ((spec (eql :model)))
  (find-class 'model-builder))

(defclass model-builder (dependency-delegating-mixin
			 location-attaching-mixin
			 comment-attaching-mixin
			 lazy-resolver-mixin
			 root-package-creating-mixin)
  ()
  (:documentation
   "This builder produces nodes by creating instances of classes from
the rosetta.model.data package."))

(defmacro define-make-node (kind (&rest args) &body body)
  (let+ (((kind &optional (class (unless body
				   (find-symbol (string kind) :rosetta.model.data))))
	  (ensure-list kind))
	 (args (mapcar #'ensure-list args))
	 ((&flet+ make-parameter ((name &optional nil))
	    `(,name (required-argument ,(make-keyword name)))))
	 ((&flet+ make-type-check ((name type))
	    `(check-type ,name ,type)))
	 ((&flet+ make-initarg ((name &optional nil))
	    `(,(make-keyword name) ,name))))
   `(defmethod make-node ((builder model-builder)
			  (kind    (eql ,kind))
			  &key
			  ,@(mapcar #'make-parameter args))
      ,@(mapcar #'make-type-check (remove nil args :key #'second))
      ,@(or body
	    `((make-instance ',class
			     ,@(mappend #'make-initarg args)))))))

(defmethod find-node ((builder model-builder)
		      (kind    (eql :fundamental))
		      &key
		      (category (required-argument :category))
		      width
		      signed?
		      encoding)
  (make-instance (ecase category
		   (:bool
		    'type-bool)
		   (:integer
		    (or (find-symbol (format nil "TYPE-~:[U~;~]INT~D"
					     signed? width)
				     :rs.m.d)
			(error "~:<There is no ~:[un~;~]signed integer ~
type with width ~D.~@>" signed? width)))
		   (:float
		    (ecase width
		      (32 'type-float32)
		      (64 'type-float64)))
		   (:string
		    (ecase encoding
		      (:ascii 'type-ascii-string)
		      (:utf-8 'type-utf-8-string)))
		   (:bytes
		    'type-octet-vector))))

(defmethod find-node ((builder model-builder)
		      (kind    (eql :singleton))
		      &key
		      (type  (required-argument :type))
		      (value (required-argument :value)))
  (make-instance 'singleton
		 :type  type
		 :value value))

(define-make-node :comment ((content string))
  content)

(define-make-node :constant ((name string) value)
  (warn "~@<Ignoring constant ~S = ~S~@:>" name value))

(define-make-node :enum-value ((name string) value))

(define-make-node :enum ((name string) type))

(define-make-node (:field base-field) ((name string) type))

(define-make-node (:structure base-structure) ((name string)))

(define-make-node (:array base-array) (element-type index-type))

(define-make-node (:package package1) ((name string)))

(defmethod add-child ((builder model-builder) ;;; TODO(jmoringe, 2012-04-24):
		      (parent  rs.m.d::base-repository)
		      (child   named-mixin))
  (assert (not (eq parent child))) ; TODO(jmoringe, 2012-10-24): proper condition

  (setf (lookup parent (kind child) (list :absolute (name child))) child) ; TODO(jmoringe, 2012-04-24): temp
  parent)

(defmethod add-child ((builder model-builder) ;;; TODO(jmoringe, 2012-04-24):
		      (parent  package1)
		      (child   named-mixin))
  (assert (not (eq parent child))) ; TODO(jmoringe, 2012-10-24): proper condition

  (setf (lookup parent (kind child) (name child)) child) ; TODO(jmoringe, 2012-04-24):
  parent)


;;; Comment handling
;;

(defmethod (setf comment) :after ((new-value t)
				  (builder   model-builder)
				  (for       t))
  (add-child builder for new-value))


;;; mapping
;;

;;; TODO(jmoringe, 2012-06-01): move somewhere else
(define-make-node :mapping (data-holder wire-schema rules))

(define-make-node :rule (lhs rhs)
  (cons lhs rhs))
