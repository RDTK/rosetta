;;; message.lisp --- Parser for the ROS message format.
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

(in-package :rosetta.ros.frontend)


;;; Actions
;;

(declaim (special *field-number*))

(defvar *field-number* 0
  "TODO(jmoringe): document")

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun make-field (type name)
    "TODO(jmoringe): document"
    (bind (((:values label type type-name)
	    (cond
	      ((stringp type) (values :required :message type))
	      ((symbolp type) (values :required (ros-type->pb-type type)))
	      ((and (listp type)
		    (member (first type) +types+))
	       (values
		:repeated
		(ros-type->pb-type (first type))))
	      ((listp type)
	       (values
		:repeated
		:message
		(format nil "~{~A~^/~}" (ensure-list (first type)))))
	      (t (error "Type not understood ~A" type)))))
      (make-instance 'pb::field-desc
		     :name      name
		     :type      type
		     :type-name (or type-name "")
		     :number    (incf *field-number*)
		     :label     label)))

  (defun ros-type->pb-type (type)
    "TODO(jmoringe): document"
    (ecase type
      (:bool    :bool)
      (:int8    :int32) ;; TODO OK?
      (:uint8   :uint32)
      (:int16   :int32)
      (:uint16  :uint32)
      (:int32   :int32)
      (:uint32  :uint32)
      (:int64   :int64)
      (:uint64  :uint64)
      (:float32 :float)
      (:float64 :double)
      (:string  :bytes)))

  (defun make-message (fields)
    "TODO(jmoringe): document"
    (prog1
	(make-instance 'pb::message-desc
		       :field (make-array (length fields)
					  :adjustable       t
					  :initial-contents fields))
      (setf *field-number* 0)))

  (defun make-qualified-name (package solidus name)
    "TODO(jmoringe): document"
    (list package name))

  (defun maybe-cons (maybe-car cdr)
    "TODO(jmoringe): document"
    (if maybe-car (cons maybe-car cdr) cdr)))


;;; Parser
;;

(yacc:define-parser *message-parser*
  (:start-symbol message)
  (:terminals    (:LEFT_SQUARE_BRACKET :RIGHT_SQUARE_BRACKET :SOLIDUS :EQUALS_SIGN :NEWLINE ;; TODO obtain from +punctuation+
		  :type :ident :number))
  (:precedence   ((:left :NEWLINE :ident)))
  (:muffle-conflicts t) ;;; TODO(jmoringe):

  (message
   ( item-list #'make-message ))

  (item-list
   ()
   ( item item-list #'maybe-cons ))

  (item
   newlines
   ( constant end (compose #'first #'list) )
   ( field end (compose #'first #'list) ))

  (end
   nil
   :NEWLINE)

  (newlines
   ( :NEWLINE (constantly nil) )
   ( :NEWLINE newlines (constantly nil)))

  (constant
   ( type-spec :ident :EQUALS_SIGN value ))

  (value
   :number
   string-parts)

  (string-parts
   ( :ident #'list )
   ( :ident string-parts #'cons ))

  (field
   ( type-spec :ident #'make-field ))

  (type-spec
   type-designator
   ( type-designator dimension-spec ))

  (type-designator
   ( :ident :SOLIDUS :ident #'make-qualified-name )
   :ident
   :type)

  (dimension-spec
   ( :LEFT_SQUARE_BRACKET :number :RIGHT_SQUARE_BRACKET (compose #'second #'list) )
   ( :LEFT_SQUARE_BRACKET :RIGHT_SQUARE_BRACKET (constantly '*) )))


;;; Client interface
;;

(defun parse (source)
  "The the contents of the stream SOURCE and return the resulting
partially post-processed syntax tree."
  (bind ((*field-number* 0)
	 ((:values lexer position1) (make-stream-lexer source)))
    (handler-case
	(yacc:parse-with-lexer lexer *message-parser*)
      (yacc:yacc-runtime-error (condition)
	(bind (((:values offset line column) (funcall position1)))
	  (error 'pbf::proto-parse-error
		 :offset            offset
		 :line              line
		 :column            column
		 :causing-condition condition))))))

;; TODO there are useful methods on pbf:load/text for files, streams etc
(defmethod pbf::load-ros/text ((source stream))
  "TODO(jmoringe): document"
  (parse source))

(defmethod pbf::load-ros/text ((source pathname))
  "TODO(jmoringe): document"
  (let ((descriptor (with-input-from-file (stream source)
		      (pbf::load-ros/text stream))))
    (setf (pb::message-desc-name descriptor) (pathname-name source))
    (make-instance 'pb::file-desc
		:name         "cl-user"
		:package      "cl-user"
		:message-type (list descriptor))))

(defclass pbb::target-ros-msg (pbb::stream-target-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod pbb:emit ((node   pb::field-desc)
		     (target pbb::target-ros-msg)
		     &key)
  "TODO(jmoringe): document"
  (bind (((:accessors-r/o (name pb::field-desc-name)
			  (type pb::field-desc-type)) node)
	 ((:accessors-r/o (stream pbb::target-stream)) target))
    (format stream "~A ~A~%" type name)))
