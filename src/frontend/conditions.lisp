;;; conditions.lisp --- Conditions used in the frontend module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rosetta.frontend)


;;; Parsing-related conditions
;;

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
		    (mappend (lambda+ (((keyword variable) nil nil))
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
     (let+ (((&accessors-r/o location source-content)
	     condition)
	    (start-column (column location))
	    (end-column   (column location :of :end)))
       (format stream "~<When parsing ~
~:/rosetta.frontend::format-location/~@:>~:[ ~3*~; which is ~2&~
~@[  ~V@Tv~&~]~<| ~@;~/rosetta.frontend::format-content/~:>~&~
~@[  ~V@T^~&~]~2&~]~@<~/more-conditions::maybe-print-cause/~@:>"
	       (list location)
	       source-content start-column (list location) end-column
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
     (let+ (((&accessors-r/o location source-content builder)
	     condition)
	    (start-column (column location))
	    (end-column   (column location :of :end)))
       (format stream "~<When processing ~
~:/rosetta.frontend::format-location/~@:>~:[ ~3*~; which is ~2&~
~@[  ~V@Tv~&~]~<| ~@;~/rosetta.frontend::format-content/~:>~&~
~@[  ~V@T^~&~]~2&~]~@<~@[with builder ~A~].~
~/more-conditions::maybe-print-cause/~@:>"
	       (list location)
	       source-content start-column (list location) end-column
	       builder
	       condition))))
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
	     ,(format nil "This ~(~A~) is signaled when a problem is
encountered during parsing of the contents of a source."
		      kind)))

	  (define-condition ,processing-name (,kind
					      processing-condition)
	    ()
	    (:documentation
	     ,(format nil "This ~(~A~) is signaled when a problem is
encountered during processing of the contents of a source after or
during parsing."
		      kind))))))

  (define-frontend-conditions warning)
  (define-frontend-conditions error :parse-name parse-error1))
