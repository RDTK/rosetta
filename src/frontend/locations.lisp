;;;; locations.lisp --- Representation and utilities for source locations.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

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
     (incompatible-initargs 'location-info
			    :bounds   bounds
			    :position position))

    (position
     (check-type position non-negative-integer)
     (setf (bounds instance) (cons position nil))))

  (let+ (((&accessors-r/o source-content bounds) instance)
	 ((&flet check-position (position &key (inclusive? t))
	    (unless (or (not position) (not source-content)
			(<= 0 position (max 0 (- (length source-content)
						 (if inclusive? 1 0)))))
	      (incompatible-initargs 'location-info
				     :source-content source-content
				     :bounds         bounds)))))
    (when bounds
      (check-type bounds bounds/cons)
      (check-position (car bounds) :inclusive? t)
      (check-position (cdr bounds) :inclusive? nil))))

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
      (- position (%position-of-newline-before content position))))

(defmethod location= ((left  location-info)
		      (right location-info)
		      &key
		      (compare-source?         t)
		      (compare-source-content? t)
		      (compare-bounds?         t))
  (and (or (not compare-source?)
	   (equal (source left) (source right)))
       (or (not compare-source-content?)
	   (equal (source-content left) (source-content right)))
       (or (not compare-bounds?)
	   (equal (bounds left) (bounds right)))))

(defmethod print-object ((object location-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format-location stream object)))

(defun format-content (stream info &optional colon? at?)
  "If INFO contains bounds information, return the source of INFO
narrowed to that bounds. Otherwise return the full source of INFO."
  (declare (ignore at?))

  (let+ (((&accessors-r/o (content source-content) bounds) info)
	 ((&flet write/maybe-truncate (string)
	    (let ((truncate-at (cond
				 ((not *print-length*)
				  nil)
				 ((< *print-length* (length string))
				  (1- *print-length*)))))
	      (apply #'write-string string stream
		     (when truncate-at (list :end truncate-at)))
	      (when truncate-at (write-char #\… stream))))))
    (cond
      ;; Content and bounds are available => extract and print the
      ;; interesting section of the content.
      ((and content bounds)
       (let+ (((start . end) bounds)
	      (start/line (%position-of-newline-before content start))
	      (end/line   (or (when end
				(position #\Newline content
					  :start    (max 0 (1- end))))
			      (length content))))
	 (write/maybe-truncate (subseq content start/line end/line))))

      ;; Content is available, but we have no bounds => print entire
      ;; or truncated content.
      (content
       (write/maybe-truncate content))

      ;; We have no content to print. The colon argument
      ;; controls whether we still should print something.
      ((not colon?)
       (format stream "~@<<No content and/or region information for ~A>~@:>"
	       info)))))

(defun format-content-with-delimiters (stream info &optional colon? at?)
  "Use `format-content' to print the content of INFO and add
indicators delimiting the region of interest within the printed
content."
  (declare (ignore at?))

  (let ((start-column   (column info :of :start))
	(end-column     (column info :of :end))
	(*print-pretty* t))
    (cond
      ((source-content info)
       (format stream "~@[  ~V@Tv~&~]~
~<| ~@;~/rosetta.frontend::format-content/~:>~
~@[~&  ~V@T^~]"
	       start-column (list info) end-column))
      ((not colon?)
       (format-content stream info)))))

(defun format-location (stream info &optional colon? at?)
  "Format the `location-info' object INFO onto STREAM.

If COLON? is non-nil, produce a human-readable description. Otherwise
produce a parser-friendly representation.

If AT? is non-nil, and the source of INFO is a string, print the
abbreviated string. Otherwise print <string>."
  (let+ (((&accessors-r/o source) info)
	 (start-line+1   (when-let ((line (line info :of :start)))
			   (1+ line)))
	 (start-column+1 (when-let ((column (column info :of :start)))
			   (1+ column)))
	 (end-line+1     (when-let ((line (line info :of :end)))
			   (1+ line)))
	 (end-column+1   (when-let ((column (column info :of :end)))
			   (1+ column)))
	 (source-label
	  (typecase source
	    (null   "<unknown source>")
	    (string (if at?
			(prin1-to-string
			 (maybe-shorten source :max-length 16))
			"<string>"))
	    (t      source))))
    (if colon?
	;; Textual format.
	(cond
	  ((and start-column+1 end-column+1 start-line+1 end-line+1
		(= start-line+1 end-line+1))
	   (format stream "columns ~D to ~D of ~@[line ~D of ~]~A"
		   start-column+1 end-column+1 start-line+1 source-label))
	  (t
	   (format stream "~@[column ~D of ~]~@[line ~D of ~]~A"
		   start-column+1 start-line+1 source-label)))
	;; Grep-like format.
	(format stream "~A~@[:~D~]~@[:~D~]"
		source-label start-line+1 start-column+1))))


;;; `location-repository' class
;;

(defclass location-repository ()
  ((assoc :initarg  :assoc
	  :type     hash-table
	  :reader   %assoc
	  :initform (make-hash-table :test #'eq)
	  :documentation
	  "Associates source locations to elements."))
  (:documentation
   "Instances of this class associate (typically) `location-info'
instances to arbitrary objects."))

(defmethod location-of ((repository location-repository)
			(thing      t))
  (values (gethash thing (%assoc repository))))

(defmethod (setf location-of) ((new-value  t)
			       (repository location-repository)
			       (thing      t))
  (setf (gethash thing (%assoc repository)) new-value))


;;; Utility functions
;;

(defun %position-of-newline-before (string position)
  "Return the position of the rightmost newline character left of
POSITION in STRING or 0 if there is none."
  (if-let ((newline-position (position #\Newline string
				       :end      position
				       :from-end t)))
    (1+ newline-position)
    0))

;; Local Variables:
;; coding: utf-8
;; End:
