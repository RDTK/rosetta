;;; target-serializer.lisp --- Targets for serializers and deserializers.
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

(in-package :rosetta.backend)

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


;;; Packed Size
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


;;; Full (De)serialization
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
