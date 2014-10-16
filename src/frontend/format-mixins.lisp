;;;; format-mixins.lisp --- Mixin classes for format classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

;;; `source-attaching-mixin' mixin class

(defclass source-attaching-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into format classes for which
    it is appropriate to attach source information to signaled
    `location-condition's."))

(macrolet
    ((define-source-attaching-method (source-class replace-predicate)
       `(defmethod parse :around ((format  source-attaching-mixin)
                                  (source  ,source-class)
                                  (builder t)
                                  &key)
          "Augment conditions signaled from next methods with location
           information based on SOURCE."
          (handler-bind ((location-condition
                           (lambda (condition)
                             (let+ (((&accessors-r/o location) condition)
                                    ((&accessors (source1 source)) location))
                               (unless (,replace-predicate source1)
                                 (setf source1 source))))))
            (call-next-method)))))

  (define-source-attaching-method pathname pathnamep)
  (define-source-attaching-method puri:uri puri:uri-p))

;;; `common-sources-mixin' mixin class

(defclass common-sources-mixin ()
  ((element-type :initarg  :element-type
                 :reader   format-element-type
                 :documentation
                 "Stores the desired element type of the format."))
  (:default-initargs
   :element-type (missing-required-initarg
                  'common-sources-mixin :element-type))
  (:documentation
   "This class is intended to be mixed into format classes which
    operate on streams with particular element types and should
    support obtaining such streams from common kinds of sources."))

(defmethod parse ((format  common-sources-mixin)
                  (source  pathname)
                  (builder t)
                  &rest args &key &allow-other-keys)
  ;; Open a character or binary input stream for the file designated
  ;; by SOURCE and call a method specialized on streams.
  (with-input-from-file (stream source
                         :element-type (format-element-type format))
    (apply #'parse format stream builder args)))

(defmethod parse ((format  common-sources-mixin)
                  (source  puri:uri)
                  (builder t)
                  &rest args &key &allow-other-keys)
  (if (member (puri:uri-scheme source) '(:http :https))
      (let+ (((&values stream code &ign &ign &ign close?)
              (drakma:http-request source :want-stream t)))
        (unwind-protect
             (cond
               ((not (<= 200 code 300))
                (error "~@<Could not download from source ~A. HTTP ~
                        result code ~D.~@:>"
                       source code))
               ((not (subtypep (stream-element-type stream)
                               (format-element-type format)))
                (error "~@<The resource designated by ~A consists of ~
                        ~S elements, but ~A requires ~S elements.~@:>"
                       source (stream-element-type stream)
                       format (format-element-type format)))
               (t
                (apply #'parse format stream builder args)))
          (when close? (close stream))))
      (call-next-method)))

;;; `binary-format-mixin' mixin class

(defclass binary-format-mixin (source-attaching-mixin
                               common-sources-mixin)
  ()
  (:default-initargs
   :element-type '(unsigned-byte 8))
  (:documentation
   "This class is intended to be mixed into format classes for binary
    formats."))

;;; `text-format-mixin' mixin class

(defclass text-format-mixin (source-attaching-mixin
                             common-sources-mixin)
  ()
  (:default-initargs
   :element-type 'character)
  (:documentation
   "This class is intended to be mixed into format classes that
    operate on textual input data."))

(defmethod parse :around ((format  text-format-mixin)
                          (source  pathname)
                          (builder t)
                          &key)
  ;; Augment conditions signaled from next methods with location
  ;; information based on SOURCE.
  (handler-bind ((location-condition
                   (lambda (condition)
                     (let+ (((&accessors-r/o location) condition)
                            ((&accessors source-content) location))
                       (unless source-content
                         (ignore-errors
                          (setf source-content
                                (read-file-into-string source))))))))
    (call-next-method)))

(defmethod parse :around ((format  text-format-mixin)
                          (source  string)
                          (builder t)
                          &key)
  ;; Augment conditions signaled from next methods location
  ;; information based on SOURCE.
  (handler-bind ((location-condition
                   (lambda (condition)
                     (let+ (((&accessors-r/o location) condition)
                            ((&accessors (source1 source) source-content) location))
                       (unless source1
                         (setf source1 source))
                       (unless source-content
                         (setf source-content source))))))
    (call-next-method)))

(defmethod parse ((format  text-format-mixin)
                  (source  string)
                  (builder t)
                  &rest args &key &allow-other-keys)
  ;; Create an input stream for the content of SOURCE and call a
  ;; method specialized on streams.
  (with-input-from-string (stream source)
    (apply #'parse format stream builder args)))

(defmethod parse ((format  text-format-mixin)
                  (source  stream)
                  (builder t)
                  &rest args &key &allow-other-keys)
  ;; Read the content of SOURCE into a string and call a method
  ;; specialized on strings.
  (let ((content (read-stream-content-into-string source)))
    (apply #'parse format content builder args)))
