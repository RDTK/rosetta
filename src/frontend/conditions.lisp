;;;; conditions.lisp --- Conditions used in the frontend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

;;; Parsing-related conditions

(define-condition location-condition (condition)
  ((location :initarg  :location
             :type     location-info
             :reader   location
             :documentation
             "Stores the location at which the condition
originated."))
  (:default-initargs
   :location (missing-required-initarg 'location-condition :location))
  (:documentation
   "This class is intended to be mixed into condition classes which
have an associated location in some source."))

(macrolet ((define-delegation (name &optional args)
             (let ((value
                    (mappend (lambda+ (((keyword variable) &ign &ign))
                              (list keyword variable))
                             (nth-value
                              3 (parse-ordinary-lambda-list args)))))
              `(defmethod ,name ((condition location-condition) ,@args)
                 (,name (location condition) ,@value)))))
  (define-delegation source)
  (define-delegation source-content)
  (define-delegation bounds)
  (define-delegation line (&key (of :start)))
  (define-delegation column (&key (of :start))))

(define-condition builder-condition (condition)
  ((builder :initarg  :builder
            :reader   builder
            :initform nil
            :documentation
            "Stores the builder (if available) that was being used
when the condition occurred."))
  (:documentation
   "This class is intended to be mixed into condition classes which
have an associated builder object."))

(define-condition parsing-condition (location-condition
                                     chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o location source-content) condition))
       (format stream "~<When parsing ~
                       ~:/rosetta.frontend::format-location/~@:> ~:[~*~; which is~
                       ~2&~/rosetta.frontend::format-content-with-delimiters/~&~]~
                       ~@<~/more-conditions::maybe-print-cause/~@:>"
               (list location)
               source-content location
               condition))))
  (:documentation
   "Instances of subclasses of this condition are signaled during
parsing the contents of a source."))

(define-condition processing-condition (location-condition
                                        builder-condition
                                        chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o location source-content builder) condition))
       (format stream "~<When processing ~
                       ~:/rosetta.frontend::format-location/~@:>~:[~*~; which is~
                       ~2&~/rosetta.frontend::format-content-with-delimiters/~&~]~
                       ~@<~@[with builder
                       ~A~].~/more-conditions::maybe-print-cause/~@:>"
               (list location)
               source-content location
               builder condition))))
  (:documentation
   "Instances of subclasses of this conditions are signaled during
processing the contents of a source after or during parsing."))

(macrolet
    ((define-frontend-conditions
         (kind
          &key
          (parse-name      (format-symbol *package* "PARSE-~A" kind))
          (processing-name (format-symbol *package* "PROCESSING-~A" kind)))
       `(progn
          (define-condition ,parse-name (,kind
                                         parsing-condition)
            ()
            (:documentation
             ,(format nil "This ~(~A~) is signaled when a problem is ~
                           encountered during parsing of the contents ~
                           of a source."
                      kind)))

          (define-condition ,processing-name (,kind
                                              processing-condition)
            ()
            (:documentation

             ,(format nil "This ~(~A~) is signaled when a problem is ~
                           encountered during processing of the ~
                           contents of a source after or during ~
                           parsing."
                      kind))))))

  (define-frontend-conditions warning)
  (define-frontend-conditions error :parse-name parse-error1))

;;; Dependency errors

(define-condition dependency-error (error)
  ((dependency :initarg  :dependency
               :reader   dependency-error-dependency
               :documentation
               ""))
  (:default-initargs
   :dependency (missing-required-initarg 'dependency-error :dependency))
  (:report
   (lambda (condition stream)
     (format stream "~@<Dependency ~S caused an error.~@:>"
             (dependency-error-dependency condition))))
  (:documentation
   "This error and subclasses are signaled when a dependency causes an
error."))

(define-condition cannot-resolve-dependency (dependency-error)
  ((locations :initarg  :locations
              :type     list
              :reader   dependency-error-locations
              :initform nil
              :documentation
              "Locations that have been consulted when trying to
resolve the dependency."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The dependency ~S could not be resolved. ~:[No ~
                     locations have been tried. Check dependency ~
                     handler~;~:*These locations have been tried: ~
                     ~{~S~^, ~}~].~@:>"
             (dependency-error-dependency condition)
             (dependency-error-locations  condition))))
  (:documentation
   "This error is signaled dependency cannot be found or loaded."))

(defun cannot-resolve-dependency (dependency &optional locations)
  "Convenience function for signaling `cannot-resolve-dependency'
errors."
  (error 'cannot-resolve-dependency
         :dependency dependency
         :locations  locations))

(define-condition ambiguous-dependency (dependency-error)
  ((candidates :initarg  :candidates
               :type     list
               :reader   dependency-error-candidates
               :initform nil
               :documentation
               "The set of candidates causing the ambiguity."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Ambiguous dependency ~S. Candidates are ~
                     ~{~S~^, ~}.~@:>"
             (dependency-error-dependency condition)
             (dependency-error-candidates condition))))
  (:documentation
   "This error is signaled if there are multiple candidates for a
dependency and no resolution strategy has been specified."))

(defun ambiguous-dependency (dependency &optional candidates)
  "Convenience function for signaling `ambiguous-dependency' errors."
  (error 'ambiguous-dependency
         :dependency dependency
         :candidates candidates))
