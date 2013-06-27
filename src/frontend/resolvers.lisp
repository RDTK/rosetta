;;;; resolvers.lisp --- Dependency resolvers for use with builders.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend)

(defclass search-path-resolver (print-items-mixin)
  ((search-path  :initarg  :search-path
                 :type     list
                 :accessor search-path
                 :initform nil
                 :documentation
                 "Stores a list of locations (such as pathnames) which
are consulted when trying to resolve a pathname dependency.")
   (if-ambiguous :initarg  :if-ambiguous
                 :type     (or (eql :first) symbol function)
                 :accessor if-ambiguous
                 :initform #'error
                 :documentation
                 "Stores a policy which is applied when the resolver
encounters an ambiguous dependency."))
  (:documentation
   "Instances of this resolver class try to resolve pathname
dependencies by using a list of pathnames in which dependencies should
be searched for."))

(defmethod merge-locations ((resolver search-path-resolver)
                            (location pathname)
                            (base     pathname))
  (merge-pathnames location base))

(defmethod merge-locations ((resolver search-path-resolver)
                            (location pathname)
                            (base     puri:uri))
  (puri:merge-uris (make-instance 'puri:uri :path (namestring location))
                   base))

(defmethod probe-location ((resolver search-path-resolver)
                           (location pathname))
  (probe-file location))

(defmethod probe-location ((resolver search-path-resolver)
                           (location puri:uri))
  ;; We can only probe HTTP[S] URLs delegate to default behavior for
  ;; all others schemes.
  (if (member (puri:uri-scheme location) '(:http :https))
      ;; Although this is a bit expensive, there is no other easy way
      ;; to probe an URL LOCATION than making a request. This can
      ;; result in all kinds of errors (e.g. missing HTTPS support,
      ;; name resolution error, HTTP errors).
      (values
       (ignore-errors
        (let+ (((&values stream code &ign &ign &ign close?)
                (drakma:http-request location :want-stream t)))
          (when close? (close stream))
          (<= 200 code 300))))
      (call-next-method)))

(defmethod resolve ((resolver search-path-resolver)
                    (format   t)
                    (location pathname)
                    &key
                    if-does-not-exist)
  (declare (ignore if-does-not-exist))

  (let+ (((&accessors-r/o search-path if-ambiguous) resolver)
         ;; Produce initial candidate set from specified relative or
         ;; absolute pathname.
         (locations (ecase (first (pathname-directory location))
                      (:absolute
                       (list location))
                      ((nil :relative)
                       (mapcar (curry #'merge-locations resolver location)
                               search-path))))
         ;; Restrict locations to existing files and select location
         ;; from resulting candidate set.
         (location/resolved
          (let ((candidates
                  (remove-if-not
                   (curry #'probe-location resolver) locations)))
            (cond
              ;; No candidate => signal an error.
              ((emptyp candidates)
               nil)

              ;; Exactly one candidate => just use it.
              ((length= 1 candidates)
               (first candidates))

              ;; Multiple candidates => apply resolution strategy or
              ;; signal an error with restarts established.
              (t
               (etypecase if-ambiguous
                 ((eql :first)
                  (first candidates))
                 ((or symbol function)
                  (restart-case
                      (funcall if-ambiguous
                               (make-condition 'ambiguous-dependency
                                               :dependency location
                                               :candidates candidates))
                    (use-value (value)
                      value)
                    (continue (&optional condition)
                      :report (lambda (stream)
                                (format stream "~@<Use the first candidate.~@:>"))
                      (declare (ignore condition))
                      (first candidates))))))))))

    ;; LOCATION/RESOLVED is the *truename* of the resolved
    ;; dependency. The builder can use it to detect already processed
    ;; locations.
    ;;
    ;; If necessary, try to guess format from resolved location.
    (if location/resolved
        (values (or format (guess-format location/resolved))
                location/resolved)
        (values nil nil locations))))

(defmethod print-items append ((object search-path-resolver))
  `((:search-path ,(length (search-path object)) "(~D)")))
