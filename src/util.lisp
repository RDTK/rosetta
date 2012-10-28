;;; util.lisp --- Utilities used in the rosetta system.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta)


;;; Name normalization utilities
;;

(defun digit-boundary? (string position)
  "Return a boundary position when there is a DIGIT-NON-DIGIT
boundary (or the other way around) at POSITION in STRING. The returned
boundary position is of the form

  (START END)

."
  (when (< (1+ position) (length string))
    (let ((first?  (digit-char-p (aref string position)))
	  (second? (digit-char-p (aref string (1+ position)))))
      (when (xor first? second?)
	(list (1+ position) (1+ position))))))

(defun camel-case-boundary? (string position)
  "Return a boundary position when there is a lower case-upper case
boundary (or the other way around) at POSITION in STRING. The returned
boundary position is of the form

  (START END)

."
  (cond
    ;; For cases like Data|XOP
    ((and (< (1+ position) (length string))
	  (lower-case-p (aref string position))
	  (upper-case-p (aref string (1+ position))))
     (list (1+ position) (1+ position)))
    ;; For cases like XOP|Data and Vec2D|Double
    ((and (< (+ position 2) (length string))
	  (upper-case-p (aref string (1+ position)))
	  (lower-case-p (aref string (+ position 2))))
     (list (1+ position) (1+ position)))))

(defun underscore-boundary? (string position)
  "Return a boundary position when there is a '_' in STRING at
POSITION. The returned boundary position is of the form

  (START END)

."
  (when (char= (aref string position) #\_)
    (list position (1+ position))))

(defun normalize-name (name
		       &key
		       (boundary? (disjoin #'digit-boundary?
					   #'camel-case-boundary?
					   #'underscore-boundary?))
		       (transform #'string-downcase)
		       (separator #\-))
  "Normalize NAME to the form

  (concatenate 'string (TRANSFORM COMPONENT1) SEPARATOR
                       (TRANSFORM COMPONENT2) SEPARATOR
                       ...)

where COMPONENTN are identified by applying the BOUNDARY?
predicate. For \"soft\" boundaries, i.e. change of case without
separator character, adjacent single-char components are merged.

BOUNDARY? has to accept two arguments: a string and a position in the
string and return a boundary of the form

  (START END)

if there is a boundary in the string at the indicated position.

TRANSFORM is a function that has to take a string and return a
transformed string such as `cl:string-downcase'.

When SEPARATOR is a character it is used as explained above. When
SEPARATOR is nil, components are simply concatenated."
  (with-output-to-string (stream)
    (let+ (((&flet component (string &optional first?)
	      "Add component STRING, potentially with a separator."
	      (when (and (not first?) separator)
		(write-char separator stream))
	      (write-string (funcall transform string) stream)))
	   (first? t))
      ;; Go through NAME, testing each position for being a boundary.
      (iter (for  char     each     name :with-index i)
	    (for  previous previous char)
	    (with start    =        0)

	    ;; Once a component is detected, collect it. Only collect
	    ;; non-empty components.
	    (let+ (((&optional end new-start) (funcall boundary? name i)))
	      (when end
		;; Collect.
		(when (and (not (zerop (- end start))) ; there is a component
			   (or (/= end new-start)      ; separator character
			       (> (- end start) 1)))   ; multi-char component
		  (component (subseq name start end) first?)
		  (setf first? nil))
		;; Advance start pointer unless we delayed a
		;; single-char component.
		(unless (and (= end new-start) (= (- end start) 1))
		  (setf start new-start))))

	    ;; Collect the final component.
	    (finally
	     (component (subseq name start) first?))))))
