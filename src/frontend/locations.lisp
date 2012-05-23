;;; locations.lisp --- Representation and utilities for source locations.
;;
;; Copyright (C) 2012 Jan Moringen
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


;;; `location-info' class
;;

(defclass location-info ()
  ((source         :initarg  :source
		   :accessor source
		   :initform nil
		   :documentation
		   "Stores the source that was being parsed when the
error occurred.")
   (source-content :initarg  :source-content
		   :type     (or null string)
		   :accessor source-content
		   :initform nil
		   :documentation
		   "Stores the source that was being parsed when the
error occurred.")
   (bounds         :initarg  :bounds
		   :type     (or null bounds/cons)
		   :accessor bounds
		   :initform nil
		   :documentation
		   "Optionally stores bounds of interesting region
within source string."))
  (:documentation
   "Instances of this class represent a location within a source
string."))

(defmethod shared-initialize :after ((instance   location-info)
                                     (slot-names t)
                                     &key
				       bounds
				       position)
  (cond
    ((and bounds position)
     (error 'incompatible-initargs
	    :class      'location-info
	    :parameters (list :bounds :position)
	    :values     (list bounds position)))

    (position
     (setf (bounds instance) (cons position nil)))))

(macrolet
    ((define-method (name &body body)
       `(defmethod ,name ((info location-info)
			  &key (of :start))
	  (when-let ((content (source-content info))
		     (bounds  (bounds info)))
	    (locally (declare (type bounds/cons bounds)))
	    (let ((position (ecase of
			      (:start (car bounds))
			      (:end   (cdr bounds)))))
	      (when position
		,@body))))))

  (define-method line
      (count #\Newline content :end position))
  (define-method column
      (- position (or (when-let ((position
				  (position #\Newline content
					    :end      position
					    :from-end t))) ;;; TODO(jmoringe, 2012-05-21): function, use in format-content
			(1+ position))
		      0))))

(defmethod print-object ((object location-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format-location stream object)))

(defun format-content (stream info &optional colon? at?)
  "If CONDITIONS contains bounds information, return the source of
CONDITION narrowed to that bounds. Otherwise return the full source of
CONDITION."
  (declare (ignore at?))

  (let ((content (source-content info))
	(bounds  (bounds info)))
    (declare (type (or null bounds/cons) bounds))
    (cond
      ((and content bounds)
       (let+ (((start . end) bounds)
	      (start/line (or (when-let ((position
					  (position #\Newline content
						    :end      start
						    :from-end t)))
				(1+ position))
			      0))
	      (end/line   (or (when end
				(position #\Newline content
					  :start    (1- end)))
			      (length content))))
	 (write-string content stream
		       :start start/line
		       :end   end/line)))
      (content
       (write-string content stream))
      ((not colon?)
       (format stream "~@<<No content and/or region information for ~A>~@:>"
	       info)))))

(defun format-location (stream info &optional colon? at?)
  "Format the `location-info' object INFO onto STREAM.
If COLON? is non-nil produce a human-readable description. Otherwise
produce a parser-friendly representation."
  (declare (ignore at?))

  (let+ (((&accessors-r/o source line column) info)
	 (line/human-readable   (when line (1+ line)))
	 (column/human-readable (when column (1+ column))))
    (if colon?
	(format stream "~@[column ~D of ~]~@[line ~D of ~]~:[<unknown source>~;~:*~A~]"
		column/human-readable line/human-readable source)
	(format stream "~:[<unknown source>~;~:*~A~]~@[:~D~]~@[:~D~]"
		source line/human-readable column/human-readable))))


;;; `location-repository' class
;;

(defclass location-repository ()
  ((assoc :initarg  :assoc
	  :type     hash-table
	  :accessor %assoc
	  :initform (make-hash-table :test #'eq)
	  :documentation
	  "Associates source locations to elements."))
  (:documentation
   "Instances of this class associate (typically) `location-info'
instances to arbitrary objects."))

(defmethod location-of ((repository location-repository)
			(for        t))
  (values (gethash for (%assoc repository))))

(defmethod (setf location-of) ((new-value  t)
			       (repository location-repository)
			       (for        t))
  (setf (gethash for (%assoc repository)) new-value))
