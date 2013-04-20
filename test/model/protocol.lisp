;;;; protocol.lisp --- Unit tests for protocol functions of the model module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; This file may be licensed under the terms of the

(cl:in-package #:rosetta.model.test)

(deftestsuite protocol-root (model-root)
  ()
  (:documentation
   "Tests for protocol functions of the model module."))

(addtest (protocol-root
          :documentation
          "Smoke test for the `print-qname' function.")
  print-qname/smoke

  (ensure-cases (input separator colon? at? expected)
      ;; input                   sep col at  expected
      '(((:absolute)             nil nil nil "")
        ((:absolute)             nil t   nil "<root>")
        ((:relative)             nil nil nil ".")
        ((:relative)             nil t   nil ".")
        ((:absolute)             #\/ nil nil "")
        ((:absolute)             #\/ t   nil "<root>")
        ((:relative)             #\/ nil nil "/")
        ((:relative)             #\/ t   nil "/")

        ((:absolute "foo")       nil nil nil "foo")
        ((:relative "foo")       nil nil nil ".foo")
        ((:absolute "foo")       #\/ nil nil "foo")
        ((:relative "foo")       #\/ nil nil "/foo")

        ((:absolute "foo" "bar") nil nil nil "foo.bar")
        ((:relative "foo" "bar") nil nil nil ".foo.bar")
        ((:absolute "foo" "bar") #\/ nil nil "foo/bar")
        ((:relative "foo" "bar") #\/ nil nil "/foo/bar"))

    (ensure-same (with-output-to-string (stream)
                   (apply #'print-qname stream input colon? at?
                          (when separator (list separator))))
                 expected :test #'string=)))

(addtest (protocol-root
          :documentation
          "Smoke test for the `print-qname' function.")
  print-name-expression/smoke

  (ensure-cases (input separator colon? at? expected)
      ;; input                                    sep col at  expected
      '(((:absolute)                              nil nil nil "")
        ((or (:absolute))                         nil t   nil "<root>")
        ((:relative)                              nil nil nil ".")
        ((or (:relative))                         nil t   nil ".")
        ((:absolute)                              #\/ nil nil "")
        ((or (:absolute))                         #\/ t   nil "<root>")
        ((:relative)                              #\/ nil nil "/")
        ((or (:relative))                         #\/ t   nil "/")

        ((:absolute "foo")                        nil nil nil "foo")
        ((or (:relative "foo"))                   nil nil nil ".foo")
        ((:absolute "foo")                        #\/ nil nil "foo")
        ((or (:relative "foo"))                   #\/ nil nil "/foo")

        ((:absolute "foo" "bar")                  nil nil nil "foo.bar")
        ((or (:relative "foo" "bar"))             nil nil nil ".foo.bar")
        ((:absolute "foo" "bar")                  #\/ nil nil "foo/bar")
        ((or (:relative "foo" "bar"))             #\/ nil nil "/foo/bar")

        ((or (:absolute "foo") (:relative "foo")) nil nil nil "foo or .foo")
        ((or (:absolute "foo") (:relative "foo")) #\/ nil nil "foo or /foo"))

    (ensure-same (with-output-to-string (stream)
                   (apply #'print-name-expression stream input colon? at?
                          (when separator (list separator))))
                 expected :test #'string=)))
