;;; type-fundamental.lisp --- Fundamental data types.
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

(cl:in-package :rosetta.model.data)


;;; `fundamental-type-mixin' mixin class
;;

(defclass fundamental-type-mixin ()
  ((category :initarg  :category
	     :type     keyword
	     :reader   category
	     :documentation
	     "Stores the category to which the type belongs. Examples
are :integer, :string"))
  (:default-initargs
   :category (missing-required-initarg
	      'fundamental-type-mixin :category))
  (:documentation
   "This mixin class adds a category slot to data type classes
representing fundamental data types."))

(defmethod kind ((type fundamental-type-mixin))
  :fundamental)

(defmethod name ((type fundamental-type-mixin))
  (subseq (string (class-name (class-of type))) 5))

(defmethod qname ((type fundamental-type-mixin))
  (list :absolute (name type)))

(defmethod fundamental? ((type fundamental-type-mixin))
  t)


;;; `fixed-width-mixin' mixin class
;;

(defclass fixed-width-mixin ()
  ((width :initarg  :width
	  :type     t ;; positive-integer
	  :reader   width
	  :documentation
	  "Stores the amount of storage in bits required to store
values of the data type."))
  (:default-initargs
   :width (missing-required-initarg 'fixed-width-mixin :width))
  (:documentation
   "This mixin class adds a width slot to data type classes
representing (fundamental) data types which require a fixed amount of
storage."))


;;; `variable-width-mixin' mixin class
;;

(defclass variable-width-mixin ()
  ()
  (:documentation
   "This mixin class is intended to used as a superclass for data type
classes representing data types which require a variable amount of
storage."))


;;; Concrete fundamental types
;;

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-fundamental-type ((name (&rest supertypes) category)
				     &rest properties &key &allow-other-keys)
    (let ((name (symbolicate "TYPE-" name)))
      `(progn
	 (defclass ,name (,@supertypes fundamental-type-mixin)
	   ()
	   (:default-initargs
	    :category ,category
	    ,@properties))))))

(define-fundamental-type (bool (fixed-width-mixin) :bool)
  :width 1)

(defclass integer-mixin ()
  ((signed? :initarg  :signed?
	    :type     boolean
	    :reader   signed?
	    :documentation
	    "Stores either nil or t to indicate whether the integer
data type represents signed or unsigned integers."))
  (:default-initargs
   :signed? (missing-required-initarg 'integer-mixin :signed?))
  (:documentation
   "This mixin class adds a signed? slot to integer data type
classes."))

(macrolet
    ((define-fundamental-integer-type (signed? width)
       (let ((name (format-symbol *package* "~:[U~;~]INT~D"
				  signed? width)))
	 `(define-fundamental-type
	      (,name (fixed-width-mixin integer-mixin) :integer)
	      :signed? ,signed?
	      :width   ,width))))

  (define-fundamental-integer-type t    8)
  (define-fundamental-integer-type nil  8)
  (define-fundamental-integer-type t   16)
  (define-fundamental-integer-type nil 16)
  (define-fundamental-integer-type t   32)
  (define-fundamental-integer-type nil 32)
  (define-fundamental-integer-type t   64)
  (define-fundamental-integer-type nil 64))

(define-fundamental-type (float* (fixed-width-mixin) :float))

(define-fundamental-type (float32 (type-float*) :float)
  :width 32)

(define-fundamental-type (float64 (type-float*) :float)
  :width 64)

(defclass string-mixin ()
  ((encoding :initarg  :encoding
	     :type     (or (eql t) keyword)
	     :reader   encoding
	     :documentation
	     "Stores the encoding implied by the data type."))
  (:documentation
   "This mixin class adds an encoding slot to string type classes."))

(define-fundamental-type
    (string* (variable-width-mixin string-mixin) :string))

(define-fundamental-type (ascii-string (type-string*) :string)
  :encoding :ascii)

(define-fundamental-type (utf-8-string (type-string*) :string)
  :encoding :utf-8)

(define-fundamental-type (octet-vector (variable-width-mixin) :bytes))
