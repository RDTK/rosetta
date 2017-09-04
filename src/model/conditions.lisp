;;;; conditions.lisp --- Conditions used in the model module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model)

(define-condition container-condition ()
  ((container :initarg :container
              :reader  container-condition-container
              :documentation
              "Stores the container to which the condition refers."))
  (:default-initargs
   :container (missing-required-initarg 'container-condition :container))
  (:documentation
   "Superclass for conditions referring to container instances."))

(define-condition child-condition ()
  ((key :initarg  :key
        :reader   child-condition-key
        :documentation
        "Stores the key identifying the child to which the condition
         refers."))
  (:default-initargs
   :key (missing-required-initarg 'child-condition :key))
  (:documentation
   "Superclass for conditions referring to children of a container."))

(define-condition child-error (container-condition
                               child-condition
                               error)
  ()
  (:documentation
   "Superclass for errors related to composite objects and their
    children."))

(define-condition chainable-child-error (chainable-condition
                                         child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Child ~S caused an error within ~
                     ~S~/more-conditions::maybe-print-cause/~@:>"
             (child-condition-key           condition)
             (container-condition-container condition)
             condition)))
  (:documentation
   "Signaled when a child-related error is caused by some other
    condition.

    It can also serve as a superclass for more specific child-related
    errors."))

(defun chainable-child-error (container child &optional cause)
  "Signal a `chainable-child-error' for TYPE and CHILD which has been
   caused by CAUSE."
  (apply #'error 'chainable-child-error :container container :key child
         (when cause (list :cause cause))))

(define-condition simple-child-error (simple-condition
                                      child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error for child ~S in ~A~
                     ~/more-conditions:maybe-print-explanation/~@:>"
             (child-condition-key           condition)
             (container-condition-container condition)
             condition)))
  (:documentation
   "Can be used to signal child-related error which include a simple
    description of the error."))

(defun simple-child-error (container child
                           format-control &rest format-arguments)
  "Signal a `simple-child-error' for CONTAINER and CHILD with a report
   generated from FORMAT-CONTROL and FORMAT-ARGUMENTS."
  (error 'simple-child-error
         :container        container
         :key              child
         :format-control   format-control
         :format-arguments format-arguments))

(define-condition no-such-child (child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The requested child ~S could not be found ~
                     within ~S.~@:>"
             (child-condition-key           condition)
             (container-condition-container condition))))
  (:documentation
   "Signaled when a requested child type cannot be found within the
    specified container type."))

(defun no-such-child (container key)
  "Signal a `no-such-child' error for CONTAINER and KEY."
  (error 'no-such-child :container container :key key))

(define-condition duplicate-child-key (child-error)
  ((existing :initarg  :existing
             :reader   duplicate-child-key-existing
             :initform nil
             :documentation
             "Stores the existing child."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The child key ~S is already in use~@[ by child ~
                     ~A~] within type ~S.~@:>"
             (child-condition-key           condition)
             (duplicate-child-key-existing  condition)
             (container-condition-container condition))))
  (:documentation
   "Signaled when a attempt is made to add child to a composite
    structure using a key that which is already in use."))

(defun duplicate-child-key (container key &optional existing)
  "Signal a `duplicate-child-key-error' for CONTAINER, KEY and
   optionally EXISTING."
  (apply #'error 'duplicate-child-key :container container :key key
         (when existing (list :existing existing))))
