;;; types.lisp --- Types used in the yarp module.
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

(in-package :rosetta.yarp)

(deftype value-type ()
  "Type tags for value types that can occur in YARP bottles."
  '(member :int :vocab :double :string :blob :list))

(deftype value ()
  "YARP value. Things that can be contained in a bottle."
  `(or (signed-byte 32) ;; int
       (signed-byte 32) ;; vocab
       double-float     ;; double
       string           ;; string
       octet-vector     ;; blob
       list             ;; list
       ))

(deftype bottle/list ()
  "YARP bottle represented as nested lists of values."
  'list)
