;;; util.lisp --- Utility functions for the ROS module of cl-rosetta.
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

(in-package :rosetta.ros)

(defun parse-type-name (name)
  "Parse the ROS message or service type name NAME into its package
and type components. Return two values: the package component and the
type component."
  (let ((components (split-sequence:split-sequence #\/ name)))
    (unless (length= 2 components)
      (error "Invalid number of /-separated components in name ~S" name))
    (values-list components)))
