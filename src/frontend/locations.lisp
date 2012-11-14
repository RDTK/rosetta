;;; locations.lisp --- Representation and utilities for source locations.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
	 ((&flet check-position (position &optional (inclusive? t))
	    (unless (or (not position) (not source-content)
			(<= 0 position (- (length source-content)
					  (if inclusive? 1 0))))
	      (incompatible-initargs 'location-info
				     :source-content source-content
				     :bounds         bounds)))))
    (when bounds
      (check-type bounds bounds/cons)
      (check-position (car bounds))
      (check-position (cdr bounds)))))

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
	 (column/human-readable (when column (1+ column)))
	 (source-label          (typecase source
				  (null   "<unknown source>")
				  (string "<string>")
				  (t      source))))
    (if colon?
	;; Textual format.
	(format stream "~@[column ~D of ~]~@[line ~D of ~]~A"
		column/human-readable line/human-readable source-label)
	;; Grep-like format.
	(format stream "~A~@[:~D~]~@[:~D~]"
		source-label line/human-readable column/human-readable))))


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
