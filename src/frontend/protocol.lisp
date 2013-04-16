;;;; protocol.lisp --- Protocol definitions of the rosetta compiler frontend.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

;;; Source and location protocol

(defgeneric source (thing)
  (:documentation
   "Return the source associated to THING."))

(defgeneric source-content (thing)
  (:documentation
   "Return the content as string of the source associated to THING.
Return nil if the content is not available."))

(defgeneric bounds (thing)
  (:documentation
   "Return the bounds of the region of the source string associated to
THING as a cons cell

  (START . END)

. Return nil if the information is not available."))

(defgeneric line (thing &key of)
  (:documentation
   "Return the line within the source string associated to
THING. Return nil if the information is not available. Line numbers
start at 0.

OF can be :START or :END and controls whether the line of the
beginning or end of the region associated to THING is used."))

(defgeneric column (thing &key of)
  (:documentation
   "Return the column within the source string associated to
THING. Return nil if the information is not available. Column numbers
start at 0.

OF can be :START or :END and controls whether the line of the
beginning or end of the region associated to THING is used."))

(defgeneric location= (left right
                       &key
                       compare-source?
                       compare-source-content?
                       compare-bounds?)
  (:documentation
   "Compare locations LEFT and RIGHT for equality and return non-nil
if they can be considered equal under the requested comparison.

COMPARE-SOURCE? controls whether sources should be compared.

COMPARE-SOURCE-CONTENT? controls whether the contents of sources
should be compared.

COMPARE-BOUNDS? controls whether bounds should be compared."))

;;; Location repository protocol

(defgeneric location-of (repository thing)
  (:documentation
   "Return a `location-info' instance for THING in REPOSITORY or nil
if there is no such information."))

(defgeneric (setf location-of) (new-value repository thing)
  (:documentation
   "Set the location information for THING within REPOSITORY to
NEW-VALUE."))

;;; Processing protocol
;; The protocol consists of the following cascade of method calls:
;; 1. client calls `process'
;;    a) `process' performs builder lookup and instantiation, if
;;       necessary (the constructed builder instance is used in all
;;       subsequent operations)
;;    b) `process' iterates over sequences of sources
;;    c) `process' performs format guessing, if requested
;;    d) `process' performs format lookup and instantiation, if
;;       necessary
;; 2. `process' establishes restarts and calls `parse'
;; 3. Methods `parse' on do the actual work
;; 4. And :around method on `parse' performs condition translation, if
;;    necessary
;;
;; Methods on `process' should be added very carefully since the
;; behavior described above can easily be disturbed by additional
;; methods.

(defgeneric process (format source builder
                     &key &allow-other-keys)
  (:argument-precedence-order builder source format)
  (:documentation
   "Parse content of SOURCE assuming it uses the format or syntax
described by FORMAT. Return an object that representing the parsed
content which is constructed using BUILDER.

FORMAT and BUILDER can be designators or FORMAT and BUILDER
classes."))

(defgeneric parse (format source builder
                   &key &allow-other-keys)
  (:documentation
   "Parse content of SOURCE assuming it uses the format or syntax
described by FORMAT."))

;; error handling

(define-condition-translating-method
    parse ((format t) (source t) (builder t)
           &key &allow-other-keys)
  (((and error (not processing-error)) parse-error1)
   :location (make-instance 'location-info
                            :source source)
   :builder  builder)
  (((and warning (not processing-warning)) parse-warning
                                           :signal-via warn)
   :location (make-instance 'location-info
                            :source source)
   :builder  builder))

;;; Default behavior for `process'
;; 1. Maybe resolve builder class
;; 2. Iterate over sources
;; 3. Maybe guess format
;; 4. Maybe resolve format class
;; 5. Dispatch to `parse'

(defmethod process ((format t) (source t) (builder t)
                    &key &allow-other-keys)
  (iter
    (restart-case
        (return (parse format source builder))
      (retry ()
        :report (lambda (stream)
                  (format stream "~@<Retry processing ~S in format ~S ~
with builder ~A.~@:>"
                          (maybe-shorten source) format builder)))
      (use-value (value)
        :report (lambda (stream)
                  (format stream "~@<Use specified value instead of ~
processing ~S in format ~S with builder ~A.~@:>"
                          (maybe-shorten source) format builder))
        :interactive (lambda ()
                       (format *query-io* "Value (evaluated): ")
                       (finish-output *query-io*)
                       (list (eval (read *query-io*))))
        (return value)))))

(defmethod process ((format t) (source sequence) (builder t)
                    &rest args &key &allow-other-keys)
  (typecase source
    (string
     (call-next-method))
    (t
     (iter (for source1 each source)
           (when-let ((result
                       (restart-case
                           (apply #'process format source1 builder args)
                         (continue (&optional condition)
                           :report (lambda (stream)
                                     (format stream "~@<Skip ~S and ~
continue with the next source.~@:>"
                                             (maybe-shorten source1)))
                           (declare (ignore condition))))))
             (collect result))))))

(defmethod process ((format t) (source pathname) (builder t)
                    &rest args &key &allow-other-keys)
  (if (wild-pathname-p source)
      (apply #'process format (directory source) builder args)
      (call-next-method)))

(defmethod process ((format list) (source t) (builder t)
                    &rest args &key &allow-other-keys)
  (let+ (((name &rest initargs) format)
         (class    (find-format-class name))
         (instance (apply #'make-instance class initargs)))
    (apply #'process instance source builder args)))

(defmethod process ((format symbol) (source t) (builder t)
                    &rest args &key &allow-other-keys)
  (apply #'process (list format) source builder args))

(defmethod process ((format (eql :guess)) (source pathname) (builder t)
                    &rest args &key &allow-other-keys)
  (apply #'process (guess-format source) source builder args))

(defmethod process ((format t) (source t) (builder list)
                    &rest args &key &allow-other-keys)
  (let+ (((name &rest initargs) builder)
         (class    (find-builder-class name))
         (instance (apply #'make-instance class initargs)))
    (apply #'process format source instance args)))

(defmethod process ((format t) (source t) (builder symbol)
                    &rest args &key &allow-other-keys)
  (if (keywordp builder)
      (apply #'process format source (list builder) args)
      (call-next-method)))

;;; Formats

(intern "FORMAT") ;; for (documentation :FORMAT 'rosetta.frontend:format)

(dynamic-classes:define-findable-class-family format
  "This family consists of input format classes. Each input format
class is associated with input sources, encodings and syntax. Input
formats may be file-based, stream-based, buffer-based, may use textual
or binary encodings and may be expressed using different kind of
grammars. Furthermore, input formats may describe semantically
different aspects like data types and software system components."
  (:package *package*))

(defmethod documentation ((thing symbol) (type (eql 'format)))
  "Obtain documentation of type FORMAT from the target class
designated by THING."
  (documentation (find-format-class thing) t))

;;; Comment attaching protocol

(defgeneric most-recent-comment (builder for)
  (:documentation
   "Return the most recent comment (usually a string) encountered by
BUILDER for object FOR or nil if there is no such comment."))

(defgeneric (setf most-recent-comment) (new-value builder for)
  (:documentation
   "Set NEW-VALUE (usually a string) as the most recent comment
encountered by BUILDER for object FOR."))

(defgeneric comment (builder for)
  (:documentation
   "Return the complete comment object (usually a string, concatenated
from individual comment strings) BUILDER created for object FOR."))

(defgeneric (setf comment) (new-value builder for)
  (:documentation
   "Install NEW-VALUE as the complete comment object BUILDER should
associate to object FOR."))

(defgeneric comment? (builder thing)
  (:documentation
   "Return non-nil when BUILDER should treat THING as a comment."))

(defgeneric prettify (builder comment)
  (:documentation
   "Try to clean up COMMENT, e.g. by removing unnecessary whitespace,
and return the result."))

;;; Dependency resolution protocol

(defgeneric resolve (resolver format location
                     &key
                     if-does-not-exist)
  (:documentation
   "Use RESOLVER to resolve the dependency described by FORMAT,
and LOCATION.

FORMAT can be nil to indicate that the format is not known and should
be derived from LOCATION, if possible.

IF-DOES-NOT-EXIST controls the behavior in case RESOLVER cannot
resolve the combination of FORMAT and LOCATION. The following values
are allowed:

  a function

    Make a `cannot-resolve-dependency' error and call
    IF-DOES-NOT-EXIST with it as the sole argument.

  nil

    Return nil."))

(defmethod resolve ((resolver t) (format t) (location t)
                    &key
                    if-does-not-exist)
  (declare (ignore if-does-not-exist))
  nil)

(defmethod resolve :around ((resolver t) (format t) (location t)
                            &key
                            (if-does-not-exist #'error))
  (let+ (((&flet handle-does-not-exist (&optional condition candidates)
            (etypecase if-does-not-exist
              (null
               (return-from resolve (values nil nil candidates)))
              (function
               (funcall if-does-not-exist
                        (make-condition
                         'cannot-resolve-dependency
                         :dependency location
                         :locations  (if (typep condition 'cannot-resolve-dependency)
                                         (dependency-error-locations condition)
                                         candidates)))))))
         ((&values format location candidates)
          (handler-bind
              (((or simple-error cannot-resolve-dependency)
                 #'handle-does-not-exist))
            (call-next-method))))
    (if location
        (values format location)
        (handle-does-not-exist nil candidates))))

(defmethod resolve ((resolver t) (format t) (location list)
                    &key
                    if-does-not-exist)
  (declare (ignore if-does-not-exist))

  ;; Try multiple alternatives specified in LOCATION.
  (etypecase location
    ((cons (eql or))
     (let ((candidates '()))
       ;; Try each alternative in turn without signaling errors. When
       ;; `resolve' fails, the second return value is the list of
       ;; tried locations.
       (iter (for alternative in (rest location))
             (let+ (((&values format location candidates1)
                     (resolve resolver format alternative
                              :if-does-not-exist nil)))
               (if location
                   (return-from resolve (values format location))
                   (appendf candidates candidates1))))
       ;; If none worked, return nil and the tried locations.
       (values nil nil candidates)))))

;;; Search path-based resolution protocol

(defgeneric search-path (resolver)
  (:documentation
   "Return the list of paths consulted by RESOLVER to resolve pathname
dependencies."))

(defgeneric (setf search-path) (new-value resolver)
  (:documentation
   "Set the list of paths consulted by RESOLVER to resolve pathname
dependencies to NEW-VALUE."))

(defgeneric if-ambiguous (resolver)
  (:documentation
   "Return the policy RESOLVER applies when encountering ambiguous
dependencies."))

(defgeneric (setf if-ambiguous) (new-value resolver)
  (:documentation
   "Set the policy RESOLVER applies when encountering ambiguous
dependencies to NEW-VALUE."))

;;; Ensure package protocol

(defgeneric ensure-package (builder
                            &key
                            qname
                            &allow-other-keys)
  (:documentation
   "Use builder to create the package designated by QNAME and its
parents, if necessary. Return the created package."))

(defmethod ensure-package ((builder t)
                           &rest args
                           &key
                           name
                           (qname (missing-required-argument :qname)))
  (let+ (((&flet qname->name (qname)
            (if (length= 1 qname)
                ""
                (lastcar qname))))
         ((&labels ensure-one-name (qname)
            (let* ((parent   (when (rest qname)
                               (ensure-one-name (butlast qname))))
                   (package  (apply #'find-node builder :package
                                    :qname             qname
                                    :if-does-not-exist nil
                                    (remove-from-plist args :name :qname)))
                   (created? nil))
              (unless package
                (setf package  (apply #'make-node builder :package
                                      :name  (qname->name qname)
                                      :qname qname
                                      (remove-from-plist args :name :qname))
                      created? t)
                (when parent
                  (add-child builder parent package)))
              (values package created?)))))

    ;; If NAME is supplied, make sure it matches the final component
    ;; of QNAME.
    (when (and name (not (string= name (qname->name qname))))
      (incompatible-arguments :name  name
                              :qname qname))

    (ensure-one-name qname)))
