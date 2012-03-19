;;; target-serializer.lisp --- Targets for serializers and deserializers.
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

(cl:in-package :rosetta.backend)

(defclass serialization-mixin ()
  ((mechanism :initarg  :mechanism
	      :type     symbol
	      :reader   target-mechanism
	      :documentation
	      "Stores the name of the mechanism class for which the
serialization-related code is `emit' ted."))
  (:documentation
   "This class is intended to be mixed into target classes which are
used for generating serialization-related code. It provides a slot for
storing the mechanism for which the serialization code is `emit'
ted."))


;;; Packed size
;;

(defmethod find-target-class ((spec (eql :packed-size)))
  (find-class 'target-packed-size))

(defclass target-packed-size (code-generating-target-mixin
			      serialization-mixin)
  ()
  (:documentation
   "Target class for the \"packed-size\" target which, for example,
emits methods on `rosetta.serialization:packed-size' or otherwise
generates code for given serialization mechanisms and classes
described by model component instances. This target is mainly used
with binary serialization mechanisms."))


;;; Full (de)serialization
;;

(defmethod find-target-class ((spec (eql :pack)))
  (find-class 'target-pack))

(defclass target-pack (code-generating-target-mixin
		       serialization-mixin)
  ()
  (:documentation
   "Target class for the \"pack\" target which, for example, emits
methods on `rosetta.serialization:pack' or otherwise generates code
for given serialization mechanisms and classes described by model
component instances."))

;;; TODO(jmoringe): do we really need this target?
(defmethod find-target-class ((spec (eql :before-pack)))
  (find-class 'target-before-pack))

(defclass target-before-pack (code-generating-target-mixin
			      serialization-mixin)
  ()
  (:documentation
   "Helper target class for pack target."))

(defmethod find-target-class ((spec (eql :unpack)))
  (find-class 'target-unpack))

(defclass target-unpack (code-generating-target-mixin
			 serialization-mixin)
  ()
  (:documentation
   "Target class for the \"unpack\" target which, for example, emits
methods on `rosetta.serialization:unpack' or otherwise generates code
for given serialization mechanisms and classes described by model
component instances."))


;;; Partial deserialization
;;

(defmethod find-target-class ((spec (eql :location)))
  (find-class 'target-location))

(defclass target-location (code-generating-target-mixin
			   serialization-mixin)
  ()
  (:documentation
   "Target class for the \"location\" target which, for example, emits
methods on `rosetta.serialization:location' or otherwise generates
code for given serialization mechanisms to determine the location of a
part of a structure within the serialized representation of that
containing structure."))

(defmethod find-target-class ((spec (eql :extract)))
  (find-class 'target-extract))

(defclass target-extract (code-generating-target-mixin
			  serialization-mixin)
  ()
  (:documentation
   "Target class for \"extract\" target which, for example, emits
methods on `rosetta.serialization:extract' or otherwise generates code
for given serialization mechanisms to unpack individual parts of
structures from serialized representations without unpacking the
entire serialized representation."))
