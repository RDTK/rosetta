;;; serialization.lisp ---
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

(in-package :rosetta.yarp.backend)

(defmethod bottle-unpack ((buffer t) (object symbol))
  (bottle-unpack buffer (make-instance (find-class object))))


;;; Serializer
;;

(defclass pbb::target-bottle-serializer (pbb::code-generating-target-mixin)
  ()
  (:documentation
   "This target generates serializer code to encode (TODO currently a
subset of protocol buffer) domain objects into the binary BOTTLE
bottle format."))

(defmethod pbb:emit ((node   pb::message-desc)
		     (target pbb::target-bottle-serializer)
		     &key)
  (pbb::with-emit-symbols
    (bind (((:accessors-r/o
	     (name   pb::message-desc-name)
	     (fields pb::message-desc-field)) node)
	   (name1 (pbb::intern* (pbb::make-lisp-class-name name pbb::parent))))
      (with-unique-names (buffer-var object-var offset-var)
	(eval
	 `(defmethod bottle-pack ((,buffer-var simple-array) (,object-var ,name1))
	    (check-type ,buffer-var binio:octet-vector)

	    (let ((,offset-var 0))
	      (incf ,offset-var (rs.yarp:encode-int (rs.yarp::tag->wire-code :list) ,buffer-var ,offset-var))
	      (incf ,offset-var (rs.yarp:encode-int ,(length fields) ,buffer-var ,offset-var))
	      ,@(iter (for field in (map 'list #'pbb::recur fields))
		      (appending (funcall field buffer-var offset-var object-var)))
	      (values ,offset-var ,buffer-var))))))))

(defmethod pbb:emit ((node   pb::field-desc)
		     (target pbb::target-bottle-serializer)
		     &key)
  "Generate code to unpack a single slot"
  (pbb::with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (label     pb::field-desc-label)) node)
	   (name1     (pbb::intern* (pbb::make-lisp-slot-name name)))
	   (type1     (pbb::make-lisp-slot-type type type-name package))
	   (repeated? (eq label :repeated)))
      (with-unique-names (produced-var)
       #'(lambda (buffer-form offset-form object-form)
	   `((let ((,produced-var
		    ,(case type
		       (:float
			`(rs.yarp:encode-value
			  (coerce (slot-value ,object-form ',name1) 'double-float)
			  ,buffer-form ,offset-form))
		       (t
			`(rs.yarp:encode-value
			  (slot-value ,object-form ',name1)
			  ,buffer-form ,offset-form)))))
	       (incf ,offset-form ,produced-var))))))))
;;; TODO(jmoringe): select encoder based on type


;;; Deserializer
;;

(defclass pbb::target-bottle-deserializer (pbb::code-generating-target-mixin)
  ()
  (:documentation
   "This target generates serializer code to encode (TODO currently a
subset of protocol buffer) domain objects into the binary BOTTLE
bottle format."))

(defmethod pbb:emit ((node   pb::field-desc)
		     (target pbb::target-bottle-deserializer)
		     &key)
  "Generate code to unpack a single slot"
  (pbb::with-emit-symbols
    (bind (((:accessors-r/o
	     (name      pb::field-desc-name)
	     (number    pb::field-desc-number)
	     (type      pb::field-desc-type)
	     (type-name pb::field-desc-type-name)
	     (label     pb::field-desc-label)
	     (options   pb::field-desc-options)) node)
	   (name1     (pbb::intern* (pbb::make-lisp-slot-name name)))
	   (type1     (pbb::make-lisp-slot-type type type-name package))
	   (repeated? (eq label :repeated))
	   (packed?   (when options
			(pb::field-options-packed options)))
	   (desired-wire-type (pbb::proto-type->wire-type type repeated? packed?)))
      #'(lambda (buffer-form offset-form object-form)
	  `(((setf (slot-value ,object-form ',name1)
		   (binio:decode-float-le ,buffer-form ,offset-form))
	     (incf ,offset-form 4)))))))
