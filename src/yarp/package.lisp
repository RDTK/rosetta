;;; package.lisp --- Package definition for the yarp module.
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

(in-package :cl-user)

(defpackage :rosetta.yarp
  (:nicknames rs.yarp)

  (:use
   :cl
   :iterate
   :alexandria
   :bind)

  ;; Types
  (:export
   :value-type

   :value
   :bottle/list)

  ;; Encoding and decoding functions
  (:export
   :size-of-int    :encode-int    :decode-int
   :size-of-vocab  :encode-vocab  :decode-vocab
   :size-of-double :encode-double :decode-double
   :size-of-string :encode-string :decode-string
   :size-of-blob   :encode-blob   :decode-blob
   :size-of-list   :encode-list   :decode-list
   :size-of-value  :encode-value  :decode-value
   :size-of-bottle :encode-bottle :decode-bottle)

  (:documentation
   "This package contains common definitions and functions which are
useful in the context of YARP."))
