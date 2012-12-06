;;; emitter-serializer-base-lisp.lisp --- Emitter for lisp serialization code.
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

(cl:in-package :rosetta.backend)


;;; Serialization code for fundamental types
;;

(defmethod emit ((node     fixed-width-mixin)
		 (target   target-pack)
		 (language language-lisp))
  (let+ (((&env-r/o source-var offset-var destination-var
		    (endian (endian-for (mechanism target) node))))
	 (packer (%packer-name node endian)))
    `(progn
       ,@(when source-var
	   `((setf (,packer ,destination-var ,offset-var) ,source-var)))
       ,(generate node :packed-size language))))

(defmethod emit ((node     fixed-width-mixin)
		 (target   target-unpack)
		 (language language-lisp))
  (let+ (((&env-r/o source-var offset-var destination-var
		    (endian (endian-for (mechanism target) node))))
	 (packer (%packer-name node endian)))
    `(progn
       ,@(when destination-var
	   `((setf ,destination-var (,packer ,source-var ,offset-var))))
       ,(generate node :packed-size language))))

(defmethod emit/context ((node     type-utf-8-string) ; note: utf-8-string
			 (target   target-packed-size)
			 (language language-lisp))
  (let+ (((&env-r/o source-var))
	 ((&with-gensyms temp-var)))
    (if source-var
	`(let ((,temp-var (sb-ext:string-to-octets ,source-var)))
	   ,(let+ (((&env (:source-var temp-var))))
	      (generate (make-instance 'type-octet-vector) target language)))
	0)))

(defmethod emit/context ((node     type-string*) ; note: any string
			 (target   target-pack)
			 (language language-lisp))
  (let+ (((&env-r/o source-var))
	 ((&with-gensyms temp-var)))
    (if source-var
	`(let ((,temp-var (sb-ext:string-to-octets ,source-var)))
	   ,(let+ (((&env (:source-var temp-var))))
	      (generate (make-instance 'type-octet-vector) target language)))
	0)))

(defmethod emit/context ((node     type-string*) ; note: any string
			 (target   target-unpack)
			 (language language-lisp))
  (let+ ((type-octet-vector (make-instance 'type-octet-vector))
	 ((&env-r/o destination-var))
	 ((&with-gensyms vector-var)))
    `(let ((,vector-var ,(generate type-octet-vector :instantiate language)))
       (prog1
	   ,(let+ (((&env (:destination-var vector-var))))
	      (generate type-octet-vector target language))
	 (setf ,destination-var (sb-ext:octets-to-string ,vector-var))))))

(defmethod emit ((node     type-octet-vector)
		 (target   target-pack)
		 (language language-lisp))
  (let+ (((&env-r/o source-var offset-var destination-var)))
    (if source-var
	`(progn
	   (replace ,destination-var ,source-var :start1 ,offset-var)
	   (length ,source-var))
	0)))

(defmethod emit ((node     type-octet-vector)
		 (target   target-unpack)
		 (language language-lisp))
  (let+ (((&env-r/o source-var offset-var end-var destination-var)))
    `(progn
       ,@(when destination-var
	   `((let ((length (- ,end-var ,offset-var)))
	       (unless (= (length ,destination-var) length)
		 (setf ,destination-var
		       (nibbles:make-octet-vector length)))
	       (replace ,destination-var ,source-var
			:start2 ,offset-var :end2 ,end-var))))
       (- ,end-var ,offset-var))))


;;; Serialization-related methods
;;

(macrolet
    ((define-method-target (target &body body)
       `(defmethod emit ((node     toplevel-mixin)
			 (target   ,target)
			 (language language-lisp))
	  (let+ (((&accessors-r/o mechanism) target)
		 (mechanism-class-name (class-name (class-of mechanism)))
		 ((&accessors-r/o wire-type offset-type) mechanism)
		 ((&env-r/o name)))
	    (check-type wire-type   (not null)) ; workaround to use the variables
	    (check-type offset-type (not null))

	    ,@body))))

  (define-method-target target-packed-size/method
    (let+ (((&env mechanism-var source-var)))
      `(defmethod packed-size ((,mechanism-var ,mechanism-class-name)
			       (,source-var    ,name)
			       &key)
	 ,(call-next-method))))

  (define-method-target target-pack/method
    (let+ (((&env mechanism-var source-var destination-var start-var end-var)))
      `(defmethod pack ((,mechanism-var   ,mechanism-class-name)
			(,source-var      ,name)
			(,destination-var simple-array)
			&key
			((:start ,start-var) 0)
			((:end   ,end-var)   (length ,destination-var)))
	 (declare (ignorable ,end-var)
		  (type ,(generate offset-type :reference language) ,start-var ,end-var))

	 (values ,(call-next-method) ,destination-var))))

  (define-method-target target-unpack/method
    (let+ (((&env mechanism-var source-var destination-var start-var end-var)))
      `(defmethod unpack ((,mechanism-var   ,mechanism-class-name)
			  (,source-var      simple-array)
			  (,destination-var ,name)
			  &key
			  ((:start ,start-var) 0)
			  ((:end   ,end-var)   (length ,source-var)))
	 (declare (ignorable ,end-var)
		  (type ,(generate offset-type :reference language) ,start-var ,end-var))

	 (values ,destination-var ,(call-next-method))))))


;;; Utility functions
;;

(defun sb8ref (vector index)
  (let ((value (aref vector index)))
    (+ (- (ash (ldb (byte 1 7) value) 7)) (ldb (byte 7 0) value))))

(defun (setf sb8ref) (new-value vector index)
  (setf (aref vector index) (dpb new-value (byte 8 0) 0)))

(defun %packer-name (node endian)
  "Return the a name of a function in the nibbles package which can be
used to accesses the fundamental type characterized by CATEGORY and
WIDTH."
  (let ((endian (ecase (resolve-endian endian)
		  (:little-endian '#:le)
		  (:big-endian    '#:be)))
	(width  (width node)))
    (ecase (category node)
      (:integer
       (case width
	 (8 (if (signed? node)
		'sb8ref
		'cl:aref))
	 (t (format-symbol :nibbles "~A~DREF/~A"
			   (ecase (signed? node)
			     ((nil) '#:ub)
			     (t     '#:sb))
			   width
			   endian))))
      (:float
       (format-symbol :nibbles "IEEE-~A-REF/~A"
		      (ecase width
			(32 '#:single)
			(64 '#:double))
		      endian)))))
