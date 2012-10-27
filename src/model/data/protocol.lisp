;;; protocol.lisp --- Protocol for data types.
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

(cl:in-package :rosetta.model.data)


;;; Name protocol
;;

(defgeneric name (thing)
  (:documentation
   "Return the name of THING."))

(defgeneric qname (thing)
  (:documentation
   "Return the fully qualified name of THING."))


;;; Documentation protocol
;;

(defgeneric documentation1 (thing)
  (:documentation
   "Return the documentation string associated to THING."))

(intern "DATA-TYPE")

(defmethod documentation ((thing t) (type (eql 'data-type)))
  "Return documentation for data type THING when asked for
documentation of type 'data-type."
  (documentation1 thing))


;;; Composite data type protocol
;;

(defgeneric lookup (container kind key
		    &key
		    if-does-not-exist
		    if-exists)
  (:documentation
   "Retrieve the element of kind KIND identified by KEY,
i.e. associated to the (KIND KEY) pair, within CONTAINER.

IF-DOES-NOT-EXIST controls whether an error should be signaled
if (KIND KEY) does not designate a element within CONTAINER. The
following values are allowed:

  a function
    Make a `no-such-child' error and call the supplied function with
    it as the sole argument.

  nil
    nil is returned.

IF-EXISTS is accepted for parity with the `setf' method and
ignored."))

(defgeneric (setf lookup) (new-value container kind key
			   &key
			   if-does-not-exist
			   if-exists)
  (:documentation
   "Associate NEW-VALUE with the (KIND KEY) pair in CONTAINER.

IF-DOES-NOT-EXIST is accepted for parity with the `lookup' method and
ignored.

IF-EXISTS controls the behavior in case something is already
associated with (KIND KEY) in CONTAINER. The following values are
allowed:

  :KEEP
    Do not modify CONTAINER and return the existing value.

  :SUPERSEDE
    Replace the existing value with NEW-VALUE.

  a function
    Make a `duplicate-child-key' error and call the supplied function
    with it as the sole argument."))

(defmethod lookup ((container t)
		   (kind      t)
		   (key       t)
		   &key &allow-other-keys)
  nil)

(defmethod lookup :around ((container t)
			   (kind      t)
			   (key       t)
			   &key
			   (if-does-not-exist #'error)
			   &allow-other-keys)
  (or (call-next-method)
      (etypecase if-does-not-exist
	(null
	 nil)
	(function
	 (restart-case
	     (funcall if-does-not-exist
		      (make-condition 'no-such-child
				      :type container
				      :key  (list kind key)))
	   (use-value (value)
	     value)
	   (store-value (value)
	     :interactive (lambda ()
			    (format *query-io* "Replacement value (evaluated): ")
			    (finish-output *query-io*)
			    (list (eval (read *query-io*))))
	     (setf (lookup container kind key) value)))))))

(defmethod (setf lookup) :around ((new-value  t)
				  (container  t)
				  (kind       t)
				  (key        t)
				  &key
				  (if-exists #'error)
				  &allow-other-keys)
  (when-let ((existing (lookup container kind key
			       :if-does-not-exist nil)))
    (etypecase if-exists
      ((eql :keep)
       (return-from lookup existing))
      ((eql :supersede))
      (function
       (restart-case
	   (funcall if-exists
		    (make-condition 'duplicate-child-key
				    :type container
				    :key  (list kind key)))
	 (continue ())
	 (keep ()
	   (return-from lookup existing))))))

  (call-next-method))

(defgeneric query (container kind key)
  (:documentation
   "Retrieve the element of kind KIND identified by KEY,
i.e. associated to the (KIND KEY) pair, within CONTAINER.

KIND can be a symbol or a list of the form

  (OR ALTERNATIVE1 ALTERNATIVE2 ...)

KEY can be usually be a `cl:string', a `name/relative' or a
`name/absolute'. In addition, KEY can be a list of the form

  (OR ALTERNATIVE1 ALTERNATIVE2 ...)"))

(defmethod query ((container t)
		  (kind      t)
		  (key       t))
  (lookup container kind key :if-does-not-exist nil))

(defmethod query ((container t)
		  (kind      t)
		  (key       list))
  (if (eq (first key) 'or)
      (some (curry #'query container kind) (rest key))
      (call-next-method)))

(defmethod query ((container t)
		  (kind      list)
		  (key       t))
  (some #'(lambda (kind) (query container kind key))
	kind))

(defgeneric parent (thing)
  (:documentation
   "Assuming the data type THING is contained in a composite data type,
return that data type. Otherwise return nil.

Note: this method does not reflect super/subtype relations like
integer/uint32, but composition relations like structure/field or
tuple/item.

See: `ancestors', `root'."))

(defgeneric ancestors (thing
		       &key
		       include-self?)
  (:documentation
   "Return the list of transitive `parent's of THING.

INCLUDE-SELF? controls whether THING is included at the beginning of
the returned list.

See: `parent', `root'."))

(defgeneric root (thing)
  (:documentation
   "Return the ancestor of THING which does not have a parent (the
\"root\").

See: `parent', `ancestors'."))

(defgeneric composite? (type)
  (:documentation
   "Return non-nil when the data type TYPE is in some way composed of
other types."))

(defmethod composite? ((type t))
  nil)


;;; Typed protocol
;;

(defgeneric type1 (thing)
  (:documentation
   "Return a type instance representing the type of THING."))


;;; Fundamental type protocol
;;

(defgeneric fundamental? (type)
  (:documentation
   "Return non-nil when TYPE is a fundamental data type."))

(defgeneric category (type)
  (:documentation
   "Return a symbol indicating the category of the fundamental data
type TYPE. Categories include :bool, :integer, :float, :string,
etc."))

(defgeneric width (type)
  (:documentation
   "Return the width in bits required to store values of data type
TYPE, if it is a fixed-width data type."))

(defgeneric signed? (type)
  (:documentation
   "Return non-nil if the integer data type TYPE is signed, that is
allows negative values."))

(defgeneric encoding (type)
  (:documentation
   "Return of string data type TYPE."))

(defmethod fundamental? ((type t))
  "Arbitrary types are non-fundamental be default."
  nil)


;;; Field protocol for structure-like data types
;;

(defgeneric optional? (field)
  (:documentation
   "Return non-nil if FIELD does not have to be present in
realizations of its containing data type."))

(defmethod optional? ((field t))
  nil)


;;; Array protocol
;;

(defgeneric element-type (array)
  (:documentation
   "Return the type object corresponding to the element type of
ARRAY."))

(defgeneric index-type (array)
  (:documentation
   "Return the type object corresponding to the index type of
ARRAY."))

(defgeneric fixed-size? (type)
  (:documentation
   "Return non-nil if the array type described by TYPE has a fixed
number of elements."))


;;; Singleton value protocol
;;
;; This protocol is provided by types whose extension is a single
;; object.

(defgeneric value (singleton)
  (:documentation
   "Return the value of SINGLETON."))


;;; Forward reference protocol
;;

(defgeneric upgrade! (instance other)
  (:documentation
   "Upgrade the forward reference INSTANCE to OTHER by changing its
class to the class of OTHER and copying all slot values from OTHER to
INSTANCE."))
