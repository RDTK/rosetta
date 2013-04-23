;;;; model-builder.lisp --- Builder for rosetta.model.data objects.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

(defmethod find-builder-class ((spec (eql :model)))
  (find-class 'model-builder))

(defclass model-builder (source-level-caching-mixin
                         dependency-delegating-mixin
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
         ((&flet make-supplied-var (name)
            (symbolicate name '#:-supplied?)))
         ((&flet+ make-parameter ((name &optional &ign (required? t)))
            `(,name
              ,(when (and required? (not (eq '&ign required?)))
                 `(required-argument ,(make-keyword name)))
              ,(make-supplied-var name))))
         ((&flet+ make-type-check ((name &optional type (required? t)))
            (cond
              ((not type)
               nil)
              ((and required? (not (eq '&ign required?)))
               `(check-type ,name ,type))
              (t
               `(when ,(make-supplied-var name)
                  (check-type ,name ,type))))))
         ((&flet+ make-initarg ((name &optional &ign &ign))
            `(when ,(make-supplied-var name)
               (list ,(make-keyword name) ,name)))))
   `(defmethod make-node ((builder model-builder)
                          (kind    (eql ,kind))
                          ,@(when class '(&rest args))
                          &key
                          ,@(mapcar #'make-parameter args)
                          ,@(when class `((class ',class))))
      (declare (ignorable
                ,@(mapcar (compose #'make-supplied-var #'first) args)
                ,@(mapcar #'car (remove '&ign args :test-not #'eq :key #'third))
                ,@(when class '(class))))
      ,@(remove nil (mapcar #'make-type-check args))
      ,@(or body
            `((apply
               #'make-instance class
               (append
                ,@(mapcar #'make-initarg (remove '&ign args :key #'third))
                (remove-from-plist
                 args :class
                 ,@(mapcar (compose #'make-keyword #'first) args)))))))))

(defmethod find-node ((builder model-builder)
                      (kind    (eql :fundamental))
                      &key
                      (category (required-argument :category))
                      width
                      signed?
                      encoding)
  (ecase category
    (:bool    +bool+)
    (:integer (or (symbol-value
                   (find-symbol (format nil "+~:[U~;~]INT~D+" signed? width)
                                :rs.m.d))
                  (error "~:<There is no ~:[un~;~]signed integer type ~
                          with width ~D.~@>"
                         signed? width)))
    (:float  (ecase width
               (32 +float32+)
               (64 +float64+)))
    (:string (ecase encoding
               (:ascii +ascii-string+)
               (:utf-8 +utf-8-string+)))
    (:bytes  +octet-vector+)))

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

(define-make-node :constant ((name          string)
                             value
                             (documentation string nil))
  (warn "~@<Ignoring constant ~S = ~S~@:>" name value))

(define-make-node :enum-value ((name          string)
                               value
                               (documentation string nil)))

(define-make-node :enum ((name          string)
                         (qname         name/absolute &ign)
                         type
                         (documentation string        nil)))

(define-make-node (:field base-field) ((name          string)
                                       type
                                       (documentation string nil)))

(define-make-node (:structure base-structure) ((name          string)
                                               (qname         name/absolute &ign)
                                               (documentation string        nil)))

(define-make-node (:array base-array) (element-type index-type))

(define-make-node (:package package1) ((name          string)
                                       (qname         name/absolute &ign)
                                       (documentation string        nil)))

(defmethod add-child ((builder model-builder) ; TODO(jmoringe, 2012-04-24):
                      (parent  rs.m.d::base-repository)
                      (child   named-mixin))
  (setf (lookup parent (kind child) (list :absolute (name child))) child) ; TODO(jmoringe, 2012-04-24): temp
  parent)

;;; Comment handling

(defmethod (setf comment) :after ((new-value t)
                                  (builder   model-builder)
                                  (for       t))
  (add-child builder for new-value))

;;; mapping
;;; TODO(jmoringe, 2012-06-01): move somewhere else
(define-make-node :mapping (data-holder wire-schema rules))

(define-make-node :rule (lhs rhs)
  (cons lhs rhs))
