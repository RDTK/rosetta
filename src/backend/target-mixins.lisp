;;;; target-mixins.lisp --- Mixins for target classes.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.backend)


;;; `code-generating-target-mixin' mixin class
;;

(defclass code-generating-target-mixin ()
  ((optimization-settings :initarg  :optimization-settings
			  :type     list
			  :accessor optimization-settings
			  :initform nil
			  :documentation
			  "Optimization settings that should be used
when generating code."))
  (:documentation
   "This class can be used as a superclass for target classes that
represent a code generation target."))


;;; `method-target-mixin' mixin class
;;

(defclass method-target-mixin (code-generating-target-mixin)
  ((body-target :initarg  :body-target
		:reader   body-target
		:documentation
		"Stores a target object that should be used to
generate the method body."))
  (:default-initargs
   :body-target (missing-required-initarg
		 'method-target-mixin :body-target))
  (:documentation
   "This class is intended to be mixed into target classes which emit
methods."))

(defmethod mechanism ((target method-target-mixin))
  (mechanism (body-target target)))

(defmethod emit ((node     t)
		 (target   method-target-mixin)
		 (language t))
  (generate node (body-target target) language))


;;; `mechanism-target-mixin' mixin class
;;

(defclass mechanism-target-mixin ()
  ((mechanism :initarg  :mechanism
	      :reader   mechanism
	      :documentation
	      "Stores the name of the mechanism class for which the
serialization-related code is `emit' ted."))
  (:default-initargs
   :mechanism (missing-required-initarg
	       'mechanism-target-mixin :mechanism))
  (:documentation
   "This class is intended to be mixed into target classes which are
used for generating serialization-related code. It provides a slot for
storing the mechanism for which the serialization code is `emit'
ted."))

(defmethod make-target-like ((target mechanism-target-mixin)
			     (key    symbol)
			     &rest args)
  (let ((spec (format-symbol
	       :keyword "~@[~A-~]~A"
	       (when-let* ((mechanism (mechanism target))
			   (name      (string
				       (class-name (class-of mechanism)))))
		 (if (starts-with-subseq "MECHANISM-" name)
		     (subseq name 10)
		     name))
	       key)))
    (apply #'make-instance (find-target-class spec) args)))
