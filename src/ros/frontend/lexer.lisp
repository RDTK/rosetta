;;; lexer.lisp ---
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

(in-package :rosetta.ros.frontend)

(defvar +ignored-chars+ (coerce '(#\Space #\Tab #\#) 'string)
  "List of characters that are ignore by the message parser (or lexer,
to be precise).")

(defvar +number-chars+ "01234567890.-")

(defvar +identifier-chars+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

(defvar +punctuation+ "
=[]/")

(defvar +types+ '(:bool
		  :int8    :uint8
		  :int16   :uint16
		  :int32   :uint32
		  :int64   :uint64
		  :float32 :float64
		  :string)
  "TODO(jmoringe): document")


;;; Lexer
;;

(defun make-char-reader (stream)
  "Return three values: a read-char function, an unread-char function
and a position function.

The read-char function reads a single char from STREAM, skipping over
comments.

The unread-char function behaves as usual.

The position function returns three values: the current offset, line
and column in STREAM. "
  (bind ((offset      0)
	 (line        1)
	 (column      0)
	 (in-comment? nil)
	 (did-unread? nil)
	 ((:flet read1 (&key (skip-comments? t)))
	  (iter (for c next (read-char stream nil :eof))
		(unless did-unread?
		  (incf offset)
		  (incf column)
		  (case c
		    (#\Newline (incf line)
			       (setf column      0
				     in-comment? nil))
		    (#\#       (when skip-comments?
				 (setf in-comment? t)))))
		(setf did-unread? nil)
		(while (and (not (eq c :eof)) in-comment?))
		(finally (return c))))
	 ((:flet unread (c))
	  (setf did-unread? t)
	  (unread-char c stream)))
    (values #'read1 #'unread #'(lambda () (values offset line column)))))

(defun make-stream-lexer (stream)
  (bind (((:values read unread position) (make-char-reader stream))
	 ((:flet read-while (allowed-chars &key (comments? t)))
	  (iter (for c next (funcall read :skip-comments? comments?))
		(cond
		  ((eq c :eof)
		   (terminate))
		  ((find c allowed-chars :test #'eq)
		   (collect c :result-type string))
		  (t
		   (funcall unread c)
		   (terminate)))))
	 ((:flet read-identifier-like ())
	  (let* ((string  (read-while +identifier-chars+))
		 (keyword (and
			   (notany #'upper-case-p string) ;; TODO hack
			   (find-symbol (string-upcase string) :keyword))))
	    (cond
	      ((find keyword +types+) (values :type   keyword))
	      (t                      (values :ident  string)))))
	 ((:flet intern-char (c))
	  (make-keyword (string-upcase (char-name c)))))
    (values
     #'(lambda ()
	 (read-while +ignored-chars+)
	 (let ((c (peek-char nil stream nil :eof)))
	   (cond
	     ((eq c :eof)
	      nil)
	     ((find c +punctuation+)
	      (funcall read)
	      (values (intern-char c) c))
	     ((find c +number-chars+)
	      (values :number (read-while +number-chars+)))
	     (t
	      (read-identifier-like)))))
     position)))
