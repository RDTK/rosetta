;;; type-fundamental.lisp ---
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

(defclass fundamental-type-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod name ((type fundamental-type-mixin))
  (subseq (string (class-name (class-of type))) 5))

(defmethod qname ((type fundamental-type-mixin))
  (list :absolute (name type)))

(defmethod data-type-fundamental? ((type fundamental-type-mixin))
  t)


;;;
;;


(defmacro define-fundamental-type (name (&rest supertypes) category width)
  (let ((name (symbolicate "TYPE-" name)))
   `(progn
      (defclass ,name (,@supertypes fundamental-type-mixin)
	())

      (defmethod category ((type ,name))
	,category)

      (defmethod width ((type ,name)) ;;; TODO(jmoringe, 2012-04-12): temp
	,width))))

;; :lisp-type boolean
(define-fundamental-type bool () :bool 1)

(macrolet
    ((define-fundamental-integer-type (signed? width)
       (let ((name      (format-symbol *package* "~:[U~;~]INT~D"
				       signed? width))
	     (byte-type (if signed? 'signed-byte 'unsigned-byte)))
	 ;; :lisp-type (,byte-type ,width)
	 `(define-fundamental-type ,name () :integer ,width))))

  (define-fundamental-integer-type t    8)
  (define-fundamental-integer-type nil  8)
  (define-fundamental-integer-type t   16)
  (define-fundamental-integer-type nil 16)
  (define-fundamental-integer-type t   32)
  (define-fundamental-integer-type nil 32)
  (define-fundamental-integer-type t   64)
  (define-fundamental-integer-type nil 64))

(define-fundamental-type float* () :float 32)

;; :lisp-type single-float
(define-fundamental-type float32 (type-float*) :float 32)

;; :lisp-type double-float
(define-fundamental-type float64 (type-float*) :float 64)

;; :lisp-type string
(define-fundamental-type string* () :string :variable)

;; :lisp-type string
(define-fundamental-type ascii-string (type-string*) :string :variable)

;; :lisp-type string
(define-fundamental-type utf-8-string (type-string*) :string :variable)

;; :lisp-type binio:octet-vector
(define-fundamental-type octet-vector () :bytes :variable)
