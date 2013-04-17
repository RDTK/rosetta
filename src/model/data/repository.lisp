;;;; repository.lisp --- A basic repository class for storing data types.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)

;;; `forward-reference-upgrader-mixin' mixin class

(defclass forward-reference-upgrader-mixin ()
  ((forward-references :initarg  :forward-references
                       :type     list
                       :accessor forward-references
                       :initform nil
                       :documentation
                       ""))
  (:documentation
   "This class is intended to be mixed into repository classes which
permit temporary forward references to be stored and later upgraded to
the objects they represent."))

(defmethod lookup ((repository forward-reference-upgrader-mixin)
                   (kind       symbol)
                   (name       list)
                   &key &allow-other-keys)
  (let+ (((&labels+ matches? ((entry-kind &rest entry-name))
            (cond
              ((not (member kind (ensure-list entry-kind)))
               nil)

              ((eq (first entry-name) 'or)
               (some (compose #'matches? (curry #'cons entry-kind))
                     (rest entry-name)))

              (t
               (equal entry-name name))))))

    (cdr (find-if #'matches? (forward-references repository)
                  :key #'car))))

(defmethod (setf lookup) ((new-value  forward-reference)
                          (repository forward-reference-upgrader-mixin)
                          (kind       t)
                          (name       list)
                          &key &allow-other-keys)
  (check-type name name-expression/absolute)

  (push (cons (cons kind name) new-value)
        (forward-references repository))
  new-value)

(defmethod (setf lookup) :around ((new-value  t)
                                  (repository forward-reference-upgrader-mixin)
                                  (kind       symbol)
                                  (name       list)
                                  &key &allow-other-keys)
  (check-type name name/absolute)

  (if (typep new-value 'forward-reference)
      (call-next-method)
      (let ((entry (lookup repository kind name
                           :if-does-not-exist nil)))
        (if (typep entry 'forward-reference)
            (restart-case
                (call-next-method)
              (upgrade (&optional (value new-value))
                :test (lambda (condition)
                        (typep condition 'duplicate-child-key)
                        t) ; TODO(jmoringe, 2012-04-24):
                (removef (forward-references repository) entry
                         :key #'cdr :test #'eq :count 1)
                (setf (lookup repository kind name) (upgrade! entry value))))
            (call-next-method)))))

;;; `base-repository' class

(defclass base-repository (container/absolute-mixin
                           forward-reference-upgrader-mixin
                           print-items-mixin)
  ()
  (:documentation
   "Instance of this class provide basic repository behavior of
storing data type objects and satisfying queries for these objects."))

(defmethod contents/plist ((repository base-repository))
  (hash-table-plist (%nested repository)))
