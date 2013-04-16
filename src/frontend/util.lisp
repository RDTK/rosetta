;;;; util.lisp --- Utilities used in the frontend package.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

;;; String utilities

(defun maybe-shorten (source &key (max-length 30))
  (check-type max-length positive-integer)

  (cond
    ((not (stringp source))
     source)
    ((find-if (complement #'graphic-char-p) source)
     (maybe-shorten (substitute-if #\. (complement #'graphic-char-p) source)
                    :max-length max-length))
    ((> (length source) max-length)
     (concatenate 'string (subseq source 0 (1- max-length)) '(#\…)))
    (t
     source)))

;;; File-format utilities

(defun guess-format (pathname)
  "Try to guess the format of the data definition in the file
designated by PATHNAME. Return two values: the name of the format and
a boolean indicating whether a format class exists. When PATHNAME does
not have a type, return nil."
  (when-let* ((type (pathname-type pathname))
              (key  (string-upcase type)))
    (unless (emptyp key)
      (if-let ((spec (car (find key (rs.f:format-classes)
                                :test #'search
                                :key  (compose #'symbol-name #'car)))))
        (values spec               t)
        (values (make-keyword key) nil)))))

;; Local Variables:
;; coding: utf-8
;; End:
