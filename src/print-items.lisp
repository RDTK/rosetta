;;;; print-items.lisp --- Composable printing mechanism
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta)


;;; Print Items Protocol
;;

(defgeneric print-items (object)
  (:method-combination append)
  (:documentation
   "Return a list of items that should appear in the printed
representation of OBJECT."))

(defmethod print-items append ((object t))
  "Default behavior is to not return any print items for OBJECT."
  nil)


;;; Print Items Mixin
;;

(defclass print-items-mixin ()
  ()
  (:documentation
   "This mixin class adds printing via `print-items' to classes."))

(defmethod print-object ((object print-items-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format-print-items stream (print-items object))))


;;; Utility Functions
;;

(defun format-print-items (stream items &optional colon? at?)
  "Print ITEMS onto STREAM.
ITEMS is a list of items of the form ITEM where
ITEM   ::= (KEY VALUE [FORMAT])
KEY    ::= any Lisp object
VALUE  ::= any Lisp object
FORMAT ::= a format string (Default is \"~A\")"
  (declare (ignore colon? at?))

  (mapc (lambda+ ((&ign value &optional format))
	  (format stream (or format "~A") value))
	(remove-duplicates items
			   :key      #'first
			   :from-end t)))
