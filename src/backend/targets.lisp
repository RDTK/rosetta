;;; target-serializer.lisp --- Targets for serializers and deserializers.
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

(cl:in-package :rosetta.backend)


;;; Target class
;;

(defmethod find-target-class ((spec (eql :class)))
  (find-class 'target-class))

;; stream-target-mixin
(defclass target-class (code-generating-target-mixin)
  ((metaclass    :initarg  :metaclass
		 :type     (or null symbol)
		 :reader   target-metaclass
		 :initform nil
		 :documentation
		 "")
   (superclasses :initarg  :superclasses
		 :type     list
		 :reader   target-superclasses
		 :initform nil
		 :documentation
		 ""))
  (:documentation
   "Define Lisp classes based on data type definitions. The generated
classes will not automatically have associated `pack' and `unpack'
methods. These have to be generated separately."))


;;; Serialization-related target classes
;;

(macrolet
    ((define-serialization-target (spec)
       (let ((class-name (format-symbol *package* "TARGET-~A" spec)))
	 `(progn
	    (defmethod find-target-class ((spec (eql ,spec)))
	      (find-class ',class-name))

	    (defclass ,class-name (code-generating-target-mixin
				   mechanism-target-mixin)
	      ()
	      (:documentation
	       ,(format nil "Target class for the \"~(~A~)\" target
which, for example, emits methods on `rosetta.serialization:~(~:*~A~)'
or otherwise generates code for given serialization mechanisms and
classes described by model component instances."
			spec)))))))

  (define-serialization-target :packed-size)
  (define-serialization-target :pack)
  (define-serialization-target :unpack)
  (define-serialization-target :location)
  ;; determine the location of a part of a structure within the serialized representation of that containing structure.
  (define-serialization-target :extract)
  ;; to unpack individual parts of structures from serialized representations without unpacking the entire serialized representation.
  )


;;; Method target classes
;;

(macrolet
    ((define-method-target (name)
       (let ((spec       (format-symbol :keyword "~A/METHOD" name))
	     (class-name (format-symbol *package* "TARGET-~A/METHOD" name))
	     (body-class (format-symbol *package* "TARGET-~A" name)))
	`(progn
	   (defmethod find-target-class ((spec (eql ,spec)))
	     (find-class ',class-name))

	   (defclass ,class-name (method-target-mixin)
	     ()
	     (:default-initargs
	      :body-target (make-instance ',body-class))
	     (:documentation
	      ,(format nil "Target class for a method that delegates ~
to the \"~(~A~)\" target which, for example, emits methods on ~
`rosetta.serialization:~:*~(~A~)' or otherwise generates code for ~
given serialization mechanisms and classes described by model ~
component instances."
		       name)))))))

  (define-method-target packed-size)
  (define-method-target pack)
  (define-method-target unpack))
