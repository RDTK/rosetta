;;;; textual-mixin.lisp --- Mixin class for textual mechanism classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.serialization)

(defclass textual-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
represent textual serialization mechanisms."))

(defmethod pack ((mechanism   textual-mixin)
                 (source      t)
                 (destination (eql nil))
                 &rest args
                 &key)
  "Map nil DESTINATION to 'string."
  (apply #'pack mechanism source 'string args))

(defmethod pack ((mechanism   textual-mixin)
                 (source      t)
                 (destination stream)
                 &rest args
                 &key)
  "Write packing result to stream DESTINATION."
  (let+ (((&values size result)
          (apply #'pack mechanism source 'string args)))
    (write-string result destination)
    (values size destination)))

(defmethod pack ((mechanism   textual-mixin)
                 (source      t)
                 (destination pathname)
                 &rest args
                 &key)
  "Write packing result to file designated by pathname DESTINATION."
  (with-output-to-file (stream destination :if-exists :supersede)
    (values (apply #'pack mechanism source stream args) destination)))

(defmethod unpack ((mechanism   textual-mixin)
                   (source      stream)
                   (destination t)
                   &rest args
                   &key)
  "Unpack contents of SOURCE into DESTINATION."
  (let ((string (read-stream-content-into-string source)))
    (apply #'unpack mechanism string destination args)))

(defmethod unpack ((mechanism   textual-mixin)
                   (source      pathname)
                   (destination t)
                   &rest args
                   &key)
  "Open a stream for SOURCE and unpack the contents into DESTINATION."
  (with-input-from-file (stream source)
    (apply #'unpack mechanism stream destination args)))
