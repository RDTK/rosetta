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

;;; Interaction utilities

(defun read-value (&key
                   (prompt     "Value")
                   (evaluated? t)
                   (stream     *query-io*))
  (format stream "~A ~:[~:;(evaluated)~]: " prompt evaluated?)
  (finish-output stream)
  (let ((raw (read stream)))
    (if evaluated? (eval raw) raw)))

;;; File-format utilities

(defmethod guess-format ((source string) &rest args &key)
  ;; Try all providers of the `guess-format/string' service on
  ;; SOURCE. The first successful providers determines the format.
  (let ((service (find-service 'guess-format/string)))
    (iter (for provider in (service-providers service))
          (when-let ((result (apply #'make-provider service provider
                                    source args)))
            (return result)))))

(defmethod guess-format ((source pathname) &rest args
                         &key
                         (read-file? t))
  ;; 1. Guess based on (pathname-type SOURCE)
  ;; 2. When permitted by READ-FILE?, read contents of SOURCE and
  ;;    guess based on that.
  (or (when-let* ((type (pathname-type source))
                  (key  (string-upcase type)))
        (unless (emptyp key)
          (apply #'%make-provider-if-exists
                 'guess-format/pathname-type (make-keyword key)
                 source args)))

      (when read-file?
        (when-let ((content (read-file-into-string source)))
          (apply #'guess-format content args)))))

(defmethod guess-format ((source puri:uri) &rest args &key)
  ;; 1. Guess based on (puri:uri-scheme SOURCE)
  ;; 2. Guess based on (pathname-type (puri:uri-path SOURCE))
  (or (when-let* ((scheme (puri:uri-scheme source)))
        (apply #'%make-provider-if-exists
               'guess-format/uri-scheme scheme
               source args))

      (apply #'guess-format (parse-namestring (puri:uri-path source))
             :read-file? nil args)))

(defun %make-provider-if-exists (service provider &rest args)
  (and (find-provider service provider :if-does-not-exist nil)
       (apply #'make-provider service provider args)))

;; Local Variables:
;; coding: utf-8
;; End:
