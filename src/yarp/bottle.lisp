;;; bottle.lisp --- (De)serialization of YARP bottles.
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

(in-package :rosetta.yarp)


;;; Type handling
;;

(defparameter *tag-codes*
  `((:int    . 1)
    (:vocab  . ,(+ 1 8))
    (:double . ,(+ 2 8))
    (:string . 4)
    (:blob   . ,(+ 4 8))
    (:list   . 256))
  "Mapping between numeric type ids and type keywords.")

(defun wire-code->tag (code)
  "Return the type tag corresponding to CODE or nil, if there is
none."
  (car (find code *tag-codes*
	     :key  #'cdr
	     :test #'=)))

(defun tag->wire-code (tag)
  "Return the wire-code of TAG. TAG can either one of the keywords in
`*tag-codes*' or a list of the form (:list SUB-TYPE)."
  (if (listp tag)
      (logior (tag->wire-code (first tag))
	      (tag->wire-code (second tag)))
      (cdr (find tag *tag-codes*
		 :key  #'car
		 :test #'eq))))

(defun list->sub-tag (value)
  "Try to determine a sub-type tag for the values of the list
VALUE. If all values are of a common type, return that type. Otherwise
return nil."
  (let ((types (map 'list #'value->tag value)))
    (when (every #'eq types (rest types))
      (first types))))

(defun value->tag (value)
  "Return a type tag for VALUE."
  (etypecase value
    (integer                    :int)
    ((eql :vocab)               :vocab) ;;; TODO(jmoringe):
    (double-float               :double)
    (string                     :string)
    ((vector (unsigned-byte 8)) :blob)
    (list
     (let ((sub-tag (list->sub-tag value)))
       (if sub-tag (list :list sub-tag) :list)))))

(defun value->wire-code (value)
  "Return the wire-code for the type-tag of VALUE."
  (tag->wire-code (value->tag value)))


;;; Size computation
;;

(defun size-of-int (value)
  "Return the number of octets required for encoding VALUE as 32-bit
integer."
  (declare (ignore value))
  4)


(defun size-of-double (value)
  "Return the number of octets required for encoding the double
VALUE."
  (declare (ignore value))
  8)

(defun size-of-string (value)
  "Return the number of octets required for encoding the string
VALUE."
  (+ 4 (length value) 1))

(defun size-of-blob (value)
  "Return the number of octets required for encoding the blob VALUE."
  (+ 4 (length value) 1))

(defun size-of-list (value &optional type)
  "Return the number octets required for encoding the contents of the
list VALUE. When non-nil, TYPE specifies the common type of the
elements of the list VALUE."
  (+ (size-of-int (length value))
     (iter (for item in value)
	   (sum (if type
		    (size-of-value/typed item type)
		    (size-of-value item))))))

(defun size-of-value (value)
  "Return the number of octets required for encoding VALUE together
with the type of VALUE."
  (bind (((main-tag &optional sub-tag)
	  (ensure-list (value->tag value))))
   (+ (size-of-int value)
      (size-of-value/typed value main-tag sub-tag))))

(defun size-of-value/typed (value type &optional sub-type)
  "Return the number of octets required for encoding VALUE assuming it
is of TYPE and potentially SUB-TYPE. Space for encoding the type of
VALUE is not considered."
  (ecase type
    (:int    (size-of-int value))
    (:vocab  (size-of-int value))
    (:double (size-of-double value))
    (:string (size-of-string value))
    (:blob   (size-of-blob value))
    (:list   (size-of-list value sub-type))))

(defun size-of-bottle (value)
  "Return the number of octets required to encode the bottle VALUE."
  (size-of-value value))


;;; Decoding of values
;;

(defun decode-int (buffer &optional (start 0))
  "Decode and return a 32-bit integer at START in BUFFER."
  (binio:decode-uint32-le buffer start))

(defun decode-double (buffer &optional (start 0))
  "Decode and return a little-endian encoded double at START in
BUFFER. "
  (binio:decode-double-le buffer start))

(defun decode-string (buffer &optional (start 0))
  "Decode and return a string at START of BUFFER."
  (let ((length (decode-int buffer start)))
    (values
     (sb-ext:octets-to-string buffer
			      :external-format :ascii
			      :start           (+ start 4)
			      :end             (+ start 4 length -1))
     (+ 4 length))))

(defun decode-blob (buffer &optional (start 0))
  "Decode and return a blob at START of BUFFER. "
  (let ((length (decode-int buffer start)))
    (values
     (subseq buffer (+ start 4) (+ start 4 length))
     (+ 4 length))))

(defun decode-list (buffer
		    &optional
		    (start 0)
		    type)
  "Decode and return a list at START of BUFFER."
  (let ((length (decode-int buffer start)))
    (iter (repeat length)
	  (with   offset = (+ start 4))
	  (bind (((:values value consumed)
		  (if type
		      (decode-value/typed buffer offset type)
		      (decode-value buffer offset))))
	   (collect value :into values)
	   (incf offset consumed))
	  (finally (return (values values (- offset start)))))))

(defun decode-value (buffer &optional (start 0))
  "Determine the type of the value stored at START of BUFFER, decode
and return it."
  (bind ((wire-code (decode-int buffer start))
	 (type-code (logand wire-code #xff))
	 (list-code (logand wire-code #x100))
	 ((:values value consumed)
	  (decode-value/typed
	   buffer
	   (+ start 4)
	   (wire-code->tag
	    (if (zerop list-code) type-code list-code))
	   (when (and list-code (plusp type-code))
	     (wire-code->tag type-code)))))
    (values value (+ 4 consumed))))

(defun decode-value/typed (buffer start type &optional sub-type)
  "Decode the value at START of BUFFER assuming it of TYPE,
and potentially SUB-TYPE."
  (ecase type
    (:int    (decode-int buffer start))
    (:vocab  (decode-int buffer start))
    (:double (decode-double buffer start))
    (:string (decode-string buffer start))
    (:blob   (decode-blob buffer start))
    (:list   (decode-list buffer start sub-type))))

(defun decode-bottle (buffer &optional (start 0))
  "The the bottle stored at START or BUFFER."
  (decode-value buffer start))


;;; Encoding of values
;;

(defun encode-int (value buffer &optional (start 0))
  "Encode VALUE as a 32-bit integer at offset START of BUFFER. Return
two values: the number of octets produced and BUFFER."
  (binio::encode-uint32-le value buffer start))

(defun encode-double (value buffer &optional (start 0))
  "Encode the double VALUE at offset START of BUFFER. Return two
values: the number of octets produced and BUFFER."
  (binio::encode-double-le value buffer start))

(defun encode-string (value buffer &optional (start 0))
  "Encode the string VALUE at offset START of BUFFER. Return two
values: the number of octets produced and BUFFER."
  (let ((length (1+ (length value))))
    (encode-int length buffer start)
    (replace buffer
	     (sb-ext:string-to-octets value
				      :external-format :ascii
				      :null-terminate  t)
	     :start1 (+ 4 start))
    (values (+ 4 length) buffer)))

(defun encode-blob (value buffer &optional (start 0))
  "Encode the blob VALUE at offset START of BUFFER. Return two values:
the number of octets produced and BUFFER."
  (let ((length (length value)))
    (encode-int length buffer start)
    (replace buffer value :start1 (+ 4 start))
    (values (+ 4 length) buffer)))

(defun encode-list (value buffer
		    &optional
		    (start 0)
		    type)
  "Encode the list VALUE at offset START of BUFFER. Return two values:
the number of octets produced and BUFFER."
  (encode-int (length value) buffer start)
  (iter (for  item   in value)
	(with offset =  (+ start 4))
	(incf offset
	      (if type
		  (encode-value/typed item buffer offset type)
		  (encode-value item buffer offset)))
	(finally (return (values (- offset start) buffer)))))

(defun encode-value (value buffer &optional (start 0))
  "Encode the value VALUE at offset START of BUFFER. Return two
values: the number of octets produced and BUFFER. The type of VALUE is
inferred and stored with the value of VALUE."
  (bind ((tag (value->tag value))
	 ((main-tag &optional sub-tag) (ensure-list tag))
	 (wire-code (tag->wire-code tag)))
    ;; Encode the type of VALUE.
    (encode-int wire-code buffer start)
    ;; Encode the value of VALUE.
    (values
     (+ 4 (encode-value/typed
	   value buffer (+ start 4) main-tag sub-tag))
     buffer)))

(defun encode-value/typed (value buffer start type &optional sub-type)
  "Encode the value VALUE at offset START of BUFFER assuming VALUE is
of type TYPE and potentially SUB-TYPE. Return two values: the number
of octets produced and BUFFER."
  (ecase type
    (:int    (encode-int value buffer start))
    (:vocab  (encode-int value buffer start))
    (:double (encode-double value buffer start))
    (:string (encode-string value buffer start))
    (:blob   (encode-blob value buffer start))
    (:list   (encode-list value buffer start sub-type))))

(defun encode-bottle (value
		      &optional
		      (buffer (binio:make-octet-vector (size-of-bottle value)))
		      (start 0))
  "Encode VALUE using the binary representation of YARP
bottles. Return two values, the size of the encoded representation and
the octet-vector containing it. If BUFFER is supplied, VALUE is
encoded into BUFFER. Otherwise a suitable octet-vector is created. If
START is supplied, the encoded representation starts at the given
offset."
  (encode-value value buffer start))
