;;; util.lisp --- Utilities used in the frontend package.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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

(cl:in-package :rosetta.frontend)

;;; String utilities

(defun maybe-shorten (source &key (max-length 30))
  (check-type max-length positive-integer)

  (cond
    ((not (stringp source))
     source)
    ((find-if (complement #'graphic-char-p) source)
     (maybe-shorten (substitute-if #\. (complement #'graphic-char-p) source)
		    :max-length max-length))
    ((> (length source) max-length)
     (concatenate 'string (subseq source 0 (1- max-length)) '(#\…)))
    (t
     source)))

;;; File-format utilities

(defun guess-format (pathname)
  "Try to guess the format of the data definition in the file
designated by PATHNAME. Return two values: the name of the format and
a boolean indicating whether a format class exists. When PATHNAME does
not have a type, return nil."
  (when-let* ((type (pathname-type pathname))
	      (key  (string-upcase type)))
    (unless (emptyp key)
      (if-let ((spec (car (find key (rs.f:format-classes)
				:test #'search
				:key  (compose #'symbol-name #'car)))))
	(values spec               t)
	(values (make-keyword key) nil)))))

;; Local Variables:
;; coding: utf-8
;; End:
