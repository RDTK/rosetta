;;; package.lisp --- Package definition for the ros.backend module.
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

(cl:in-package :cl-user)

(defpackage :rosetta.ros.backend
  (:nicknames
   :rs.ros.b)

  (:use
   :cl

   :rosetta.serialization)

  (:documentation
   "This package contains the functionality of the ros.backend module
of cl-rosetta. This includes:
+ generation of (de)serializers for the ROS msg serialization
  format"))
