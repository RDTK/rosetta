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


;;; Packed Size
;;

(defmethod find-target-class ((spec (eql :packed-size)))
  (find-class 'target-packed-size))

(defclass target-packed-size (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for \"packed-size\" target which, for example, emits
methods on `rosetta.serialization:packed-size' or otherwise generates
code for given serialization mechanisms and classes described by model
component instances."))


;;; Serializer
;;

(defmethod find-target-class ((spec (eql :serializer)))
  (find-class 'target-serializer))

(defclass target-serializer (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for \"serializer\" target which, for example, emits
methods on `rosetta.serialization:pack' or otherwise generates code
for given serialization mechanisms and classes described by model
component instances."))

(defmethod find-target-class ((spec (eql :before-serializer)))
  (find-class 'target-before-serializer))

(defclass target-before-serializer (code-generating-target-mixin)
  ()
  (:documentation
   "Helper target class for serializer target."))


;;; Deserializer
;;

(defmethod find-target-class ((spec (eql :deserializer)))
  (find-class 'target-deserializer))

(defclass target-deserializer (code-generating-target-mixin)
  ()
  (:documentation
   "Target class for \"deserializer\" target which, for example, emits
methods on `rosetta.serialization:unpack' or otherwise generates code
for given serialization mechanisms and classes described by model
component instances."))
