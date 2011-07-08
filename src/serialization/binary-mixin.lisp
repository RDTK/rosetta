;;; binary-mixin.lisp --- Mixin class for binary mechanism classes.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rosetta.serialization)

(defclass binary-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
represent binary serialization mechanisms."))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination (eql 'binio:octet-vector))
		 &rest args
		 &key
		 (start 0))
  "The value of DESTINATION indicates that an octet-vector should be
created as destination. Use `packed-size' to determine the required
size and create such an octet-vector."
  (let ((size (+ start (packed-size mechanism source))))
    (apply #'pack mechanism source (binio:make-octet-vector size) args)))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination (eql nil))
		 &rest args
		 &key)
  "Map nil DESTINATION to octet-vector."
  (apply #'pack mechanism source 'binio:octet-vector args))

(defmethod pack ((mechanism   binary-mixin)
		 (source      t)
		 (destination stream)
		 &rest args
		 &key)
  "Write packing result to stream DESTINATION."
  (bind (((:values size result)
	  (apply #'pack mechanism source 'binio:octet-vector args)))
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

(defmethod unpack ((mechanism   binary-mixin)
		   (source      stream)
		   (destination t)
		   &rest args
		   &key
		   (start 0)
		   end)
  "Unpack OBJECT from stream SOURCE."
  (unless (zerop start)
    (iter (repeat start) (read-byte source)))

  (bind (((:flet read-whole-stream ())
	  (let ((buffer (make-array 0
				    :element-type '(unsigned-byte 8)
				    :fill-pointer 0)))
	    (iter (for c in-stream source :using #'read-byte)
		  (vector-push-extend c buffer))
	    (coerce buffer '(simple-array (unsigned-byte 8) (*)))))
	 ((:flet read-range ())
	  (let ((buffer (make-array (- end start)
				    :element-type '(unsigned-byte 8))))
	    (read-sequence buffer source)
	    buffer))
	 (buffer (if end (read-range) (read-whole-stream))))
    (apply #'unpack mechanism buffer destination
	   (remove-from-plist args :start :end))))

(defmethod unpack ((mechanism   binary-mixin)
		   (source      pathname)
		   (destination t)
		   &rest args
		   &key
		   (start 0)
		   end)
  "Open a stream for SOURCE and, potentially seek to START, then
unpack the contents into DESTINATION."
  (with-input-from-file (stream source
				:element-type '(unsigned-byte 8))
    (unless (zerop start)
      (file-position stream start))
    (apply #'unpack mechanism stream destination
	   :end (- (or end (file-length stream)) start)
	   (remove-from-plist args :start :end))))
