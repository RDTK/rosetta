;;;; util.lisp --- Utilities used in the rosetta system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta)

;;; Name normalization utilities

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
    ;; For cases like XOP|Data and Vec2D|Double, but not XO|Ps
    ;; (therefore position + 3).
    ((and (< (+ position 3) (length string))
          (upper-case-p (aref string (1+ position)))
          (lower-case-p (aref string (+ position 2))))
     (list (1+ position) (1+ position)))))

(defun separator-boundary? (string position &optional (separators '(#\_ #\-)))
  "Return a boundary position when there is a '_' in STRING at
POSITION. The returned boundary position is of the form

  (START END)

."
  (when (member (aref string position) separators :test #'char=)
    (list position (1+ position))))

(defun normalize-name (name
                       &key
                       (boundary? (disjoin #'separator-boundary?
                                           #'camel-case-boundary?
                                           #'digit-boundary?))
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

When SEPARATOR is a `character' or `string' it is used as explained
above. When SEPARATOR is nil, components are simply concatenated."
  (with-output-to-string (stream)
    (let+ (((&flet component (string &optional first?)
              "Add component STRING, potentially with a separator."
              (when (and (not first?) separator)
                (princ separator stream))
              (write-string (funcall transform string) stream)))
           (first? t))
      ;; Go through NAME, testing each position for being a boundary.
      (iter (for  char     in-vector name :with-index i)
            (for  previous previous  char)
            (with start    =         0)

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
