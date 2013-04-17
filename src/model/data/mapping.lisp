;;;; mapping.lisp --- Associate wire-schema to given data-holder.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)

(defclass mapping (print-items-mixin)
  ((data-holder :initarg  :data-holder
                :reader   data-holder
                :documentation
                "Stores the structure type which describes the
data-holder part of the mapping.")
   (wire-schema :initarg  :wire-schema
                :reader   wire-schema
                :documentation
                "Stores the structure type which describes the
wire-schema part of the mapping.")
   (rules       :initarg  :rules
                :type     list
                :accessor rules
                :initform nil
                :documentation
                "Stores the rules which associate field of the
data-holder and wire-schema structure types."))
  (:default-initargs
   :data-holder (missing-required-initarg 'mapping     :data-holder)
   :wire-schema (missing-required-initarg 'wire-schema :wire-schema))
  (:documentation
   "instances of this class associate an arbitrary wire-schema (which
is described as a structure type) to a data-holder (which is described
as a structure type as well) by means of a set of rules.

Rules associates the values fields of the data-holder and wire-schema
via simple expressions."))

(defmethod name ((type mapping))
  (name (data-holder type)))

(defmethod qname ((type mapping))
  (qname (data-holder type)))

(defmethod print-items append ((object mapping))
  (let+ (((&accessors-r/o data-holder wire-schema rules) object))
    (list (list :data-holder (name data-holder))
          (list :wire-schema (name wire-schema)  " -> ~A")
          (list :num-items   (length rules)      " (~D)"))))
