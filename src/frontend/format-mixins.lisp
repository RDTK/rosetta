;;;; format-mixins.lisp --- Mixin classes for format classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

;;; `binary-format-mixin' mixin class

(defclass binary-format-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into format classes for binary
formats."))

(defmethod parse ((format  binary-format-mixin)
                  (source  pathname)
                  (builder t)
                  &rest args &key &allow-other-keys)
  "Open a binary input stream for the file designated by SOURCE and
call a method specialized on streams."
  (handler-bind ((location-condition
                   (lambda (condition)
                     (let+ (((&accessors-r/o location) condition)
                            ((&accessors (source1 source)) location))
                       (unless (pathnamep source1)
                         (setf source1 source))))))
    (with-input-from-file (stream source :element-type '(unsigned-byte 8))
      (apply #'parse format stream builder args))))

;;; `text-format-mixin' mixin class

(defclass text-format-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into format classes that
operate on textual input data."))

(defmethod parse ((format  text-format-mixin)
                  (source  pathname)
                  (builder t)
                  &rest args &key &allow-other-keys)
  "Open a character input stream for the file designated by SOURCE and
call a method specialized on streams."
  (handler-bind ((location-condition
                   (lambda (condition)
                     (let+ (((&accessors-r/o location) condition)
                            ((&accessors (source1 source) source-content) location))
                       (unless (pathnamep source1)
                         (setf source1 source))
                       (unless source-content
                         (setf source-content
                               (read-file-into-string source)))))))
    (with-input-from-file (stream source)
      (apply #'parse format stream builder args))))

(defmethod parse ((format  text-format-mixin)
                  (source  string)
                  (builder t)
                  &rest args &key &allow-other-keys)
  "Create an input stream for the content of SOURCE and call a method
specialized on streams."
  (handler-bind ((location-condition
                   (lambda (condition)
                     (let+ (((&accessors-r/o location) condition)
                            ((&accessors (source1 source) source-content) location))
                       (unless source1
                         (setf source1 source))
                       (unless source-content
                         (setf source-content source))))))
    (with-input-from-string (stream source)
      (apply #'parse format stream builder args))))
