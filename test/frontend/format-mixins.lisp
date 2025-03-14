;;;; format-mixins.lisp --- Test for format mixin classes of the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend.test)

(deftestsuite format-mixins-root (frontend-root)
  ()
  (:documentation
   "Tests for format mixin classes provided by the frontend module."))

(defmacro define-format-mixin-suite (class &body options)
  (let+ ((suite-name (format-symbol *package* "~A-ROOT"        class))
         (mock-name  (format-symbol *package* "~A-MOCK-FORMAT" class))
         ((&plist-r/o (initargs :initargs) (superclasses :superclasses '(format-mock)))
          (reduce #'append options)))
   `(progn
      (defclass ,mock-name (,@superclasses ,class)
        ())

      (deftestsuite ,suite-name (format-mixins-root)
        ()
        (:documentation
         ,(format nil "Tests for the `~(~A~)' mixin class." class)))

      (addtest (,suite-name
                :documentation
                ,(format nil "Test constructing a `~(~A~)' instance."
                         class))
        construct/smoke

        (make-instance ',mock-name ,@initargs)))))

(defmacro ensure-format-cases ((class &rest initargs)
                               (&body cases)
                               &body body)
  "Execute BODY with an instance of CLASS (constructed with INITARGS)
for CASES entries of which have to be of the form

  (SOURCE EXPECTED)

where SOURCE and BUILDER have to be suitable arguments for `parse' and
EXPECTED can be an object which is then used in BODY."
  (let ((result-var     'result)
        (conditions-var 'conditions))
    `(ensure-cases (source expected)
         (list ,@cases)

       (let+ ((format          (make-instance ',class ,@initargs))
              (builder         (make-instance 'mock-builder))
              (,conditions-var '())
              (,result-var
               (handler-bind
                   ((condition (lambda (condition)
                                 (appendf ,conditions-var
                                          (list condition)))))
                 (parse format source builder))))
         (declare (ignorable ,result-var))
         ,@body))))

;;; `common-sources-mixin' mixin class

(define-format-mixin-suite common-sources-mixin
  (:superclasses ())
  (:initargs     (:element-type 'character)))

(defmethod parse ((format  common-sources-mixin-mock-format)
                  (source  stream)
                  (builder t)
                  &key location location-name location-type)
  (list location location-name location-type))

(addtest (common-sources-mixin-root
          :documentation
          "Smoke test for the `parse' method specialized on
           `common-sources-mixin'.")
  parse/smoke

  (let ((this-file #.(or *compile-file-pathname* *load-pathname*)))
    (ensure-format-cases (common-sources-mixin-mock-format
                          :element-type 'character)
        (`(,this-file
           ,(list this-file (pathname-name this-file) (pathname-type this-file))))
      (ensure-same (parse format source builder) expected :test #'equal))))

;;; `binary-format-mixin' mixin class

(define-format-mixin-suite binary-format-mixin)

(addtest (binary-format-mixin-root
          :documentation
          "Test attaching source information by `binary-format-mixin'
to conditions signaled during `parse'.")
  parse/smoke

  (let ((this-file #.(or *compile-file-pathname* *load-pathname*)))
    (ensure-format-cases (binary-format-mixin-mock-format)
        (`(,this-file (,(make-instance 'location-info
                                       :source this-file))))

      (ensure-same (length conditions) (length expected)
                   :test #'=)
      (iter (for location/result   in (mapcar #'location conditions))
            (for location/expected in expected)
            (ensure-same location/result location/expected
                         :test #'location=)))))

;;; `text-format-mixin' mixin class

(define-format-mixin-suite text-format-mixin)

(addtest (text-format-mixin-root
          :documentation
          "Test attaching source information by `text-format-mixin' to
conditions signaled during `parse'.")
  parse/smoke

  (let* ((this-file         #.(or *compile-file-pathname* *load-pathname*))
         (this-file-content (read-file-into-string this-file)))
    (ensure-format-cases (text-format-mixin-mock-format)
        (`("source"   (,(make-instance 'location-info
                                       :source         "source"
                                       :source-content "source")))
         `(,this-file (,(make-instance 'location-info
                                       :source         this-file
                                       :source-content this-file-content)))
          (let ((uri (puri:uri (format nil "file://~A" this-file))))
            `(,uri (,(make-instance 'location-info :source uri)))))

      (ensure-same (length conditions) (length expected)
                   :test #'=)
      (iter (for location/result   in (mapcar #'location conditions))
            (for location/expected in expected)
            (ensure-same location/result location/expected
                         :test #'location=)))))
