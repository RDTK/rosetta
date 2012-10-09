;;; util.lisp --- Utilities used in the frontend package.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rosetta.frontend)


;;; File-format utilities
;;

(defun guess-format (pathname)
  "Try to guess the format of the data definition in the file
designated by PATHNAME."
  (let ((key (string-upcase (pathname-type pathname))))
    (or (car (find key (rs.f:format-classes)
		   :test #'search
		   :key  (compose #'symbol-name #'car)))
	(make-keyword key))))
