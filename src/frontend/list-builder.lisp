;;;; list-builder.lisp --- Builder producing lists.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.frontend)

;;; Format used by the `list' builder
;;;
;;; The output of the builder is list-based tree in which each node is
;;; a list of one of the two forms:
;;;
;;; 1. (KIND         CHILDREN PLIST)
;;; 2. ((:find KIND) nil      PLIST)
;;;
;;; Form 1. is produced by calls to `make-node' and form 2. is
;;; produced by calls to `find-node'. In both forms, PLIST is a plist
;;; of the keyword arguments passed to `make-node'/`find-node'. Calls
;;; to `add-child' modify nodes of form 1. by destructively adding
;;; child nodes to CHILDREN.

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

(defmethod lookup ((container list) (kind t) (key string)
                   &key &allow-other-keys)
  (find-if #'(lambda (element)
               (and (listp element)
                    (or (eq kind t) (eq kind (kind element)))
                    (equal key (name element))))
           (second container)))

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
  (let+ (((kind &ign &rest args)
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
