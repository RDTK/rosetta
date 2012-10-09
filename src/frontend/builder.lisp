;;; builder.lisp --- Builder for rosetta.model.data objects.
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

(cl:in-package :rosetta.frontend)


;;; model builder
;;

(defmethod find-builder-class ((spec (eql :model)))
  (find-class 'model-builder))

(defclass model-builder (location-attaching-mixin
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

;;; TODO(jmoringe, 2012-10-24): move into separate class
(define-make-node :dependency/file ((pathname pathname))
  (let+ ((search-path '("~/code/cor-lab/rst/trunk/rst/proto/stable/"))
	 (locations (ecase (first (pathname-directory pathname))
		      (:absolute
		       (list pathname))
		      (:relative
		       (mapcar (curry #'merge-pathnames pathname)
			       search-path))))
	 (pathname/resolved
	  ;; Restrict locations to existing files and process
	  ;; candidate set.
	  (let ((candidates (remove-if-not #'probe-file locations)))
	    (cond
	      ((emptyp candidates)
	       (cannot-resolve-dependency pathname locations))
	      ((length= 1 candidates)
	       (first candidates))
	      (t
	       (ecase :first #+no if-ambiguous
		      (:first (first candidates))
		      (:error (error "ambiguous import ~A ~A" pathname candidates)))))))

	 (format (guess-format pathname)))

    (parse format pathname/resolved builder)))

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


;;; list builder
;;

(defmethod find-node ((builder (eql 'list))
		      (kind    t)
		      &rest args &key &allow-other-keys)
  (let+ (((kind nil &rest args) (apply #'make-node builder kind args)))
    (list* (list :find kind) nil args)))

(defmethod make-node ((builder (eql 'list))
		      (kind    t)
		      &rest args &key)
  (list* kind nil args))

(defmethod add-child ((builder (eql 'list))
		      (parent  list)
		      (child   t))
  (appendf (second parent) (list child))
  parent)
