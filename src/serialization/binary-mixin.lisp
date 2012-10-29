;;; binary-mixin.lisp --- Mixin class for binary mechanism classes.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rosetta.serialization)

(defclass binary-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
represent binary serialization mechanisms."))

(defmethod wire-type ((mechanism binary-mixin))
  (make-instance 'rs.m.d::type-octet-vector))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination (eql 'octet-vector))
		 &rest args
		 &key
		 (start 0))
  "The value of DESTINATION indicates that an octet-vector should be
created as destination. Use `packed-size' to determine the required
size and create such an octet-vector."
  (let ((size (+ start (packed-size mechanism source))))
    (apply #'pack mechanism source (make-octet-vector size) args)))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination (eql nil))
		 &rest args
		 &key)
  "Map nil DESTINATION to octet-vector."
  (apply #'pack mechanism source 'octet-vector args))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination stream)
		 &rest args
		 &key)
  "Write packing result to stream DESTINATION."
  (let+ (((&values size result)
	  (apply #'pack mechanism source 'octet-vector args)))
    (write-sequence result destination)
    (values size destination)))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination pathname)
		 &rest args
		 &key
		 (start 0))
  "Write packing result to file designated by pathname DESTINATION."
  (with-output-to-file (stream destination
			       :element-type '(unsigned-byte 8)
			       :if-exists    :supersede)
    (when (plusp start)
      (file-position stream start))
    (values
     (apply #'pack mechanism source stream
	    (remove-from-plist args :start))
     destination)))

(macrolet
    ((define-unpack-method (name (&rest extra-args))
       `(progn
	  (defmethod ,name ((mechanism   binary-mixin)
			    (source      stream)
			    ,@extra-args
			    &rest args
			    &key
			    (start 0)
			    end)
	    ,(format nil "Prepare stream SOURCE, then ~(~A~)." name)
	    (apply #',name mechanism
		   (%prepare-binary-unpack-from-stream source start end)
		   ,@(map 'list #'first extra-args)
		   (remove-from-plist args :start :end)))

	  (defmethod ,name ((mechanism   binary-mixin)
			    (source      pathname)
			    ,@extra-args
			    &rest args
			    &key
			    (start 0)
			    end)
	    ,(format nil "Open a stream for SOURCE and, potentially ~
seek to START, then ~(~A~)." name)
	    (with-input-from-file (stream source
					  :element-type '(unsigned-byte 8))
	      (unless (zerop start)
		(file-position stream start))
	      (apply #',name mechanism stream
		     ,@(map 'list #'first extra-args)
		     :end (- (or end (file-length stream)) start)
		     (remove-from-plist args :start :end)))))))

  (define-unpack-method unpack   ((destination t)))
  (define-unpack-method location ((schema t) (part t)))
  (define-unpack-method extract  ((schema t) (part t))))


;;; Utility functions
;;

(defun %prepare-binary-unpack-from-stream (stream &optional (start 0) end)
  "Read content from STREAM into a buffer taking into account START
and END."
  ;; Advance STREAM to START position, if necessary.
  (iter (repeat start) (read-byte stream))

  ;; Read stream content into a buffer.
  (let+ (((&flet read-whole-stream ()
	    (let ((buffer (make-array 0
				      :element-type '(unsigned-byte 8)
				      :fill-pointer 0)))
	      (iter (for c in-stream stream :using #'read-byte)
		    (vector-push-extend c buffer))
	      (coerce buffer '(simple-array (unsigned-byte 8) (*))))))
	 ((&flet read-range ()
	    (let ((buffer (make-array (- end start)
				      :element-type '(unsigned-byte 8))))
	      (read-sequence buffer stream)
	      buffer))))
    (if end (read-range) (read-whole-stream))))
