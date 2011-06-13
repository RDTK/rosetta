;;; service.lisp ---
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

;; TODO contains message parser
(yacc:define-parser *service-parser*
  (:start-symbol service)
  (:terminals    (:LEFT_SQUARE_BRACKET :RIGHT_SQUARE_BRACKET :SOLIDUS :EQUALS_SIGN :NEWLINE ;; TODO obtain from +punctuation+
				       :type :ident :number))
  (:precedence   ((:left :NEWLINE :ident)))
  (:muffle-conflicts t) ;;; TODO(jmoringe):

  (service
   ( message :number message #'(lambda (a b c) (cons a c)))) ;; TODO

  (message
   ()
   ( item-list #'make-message ))

  (item-list
   ()
   ( item item-list #'maybe-cons ))

  (item
   newlines
   ( constant end (compose #'first #'list) )
   ( field end (compose #'first #'list) ))

  (end
   nil
   :NEWLINE)

  (newlines
   ( :NEWLINE (constantly nil) )
   ( :NEWLINE newlines (constantly nil)))

  (constant
   ( type-spec :ident :EQUALS_SIGN value ))

  (value
   :number
   string-parts)

  (string-parts
   ( :ident #'list )
   ( :ident string-parts #'cons ))

  (field
   ( type-spec :ident #'make-field ))

  (type-spec
   type-designator
   ( type-designator dimension-spec ))

  (type-designator
   ( :ident :SOLIDUS :ident #'make-qualified-name )
   :ident
   :type)

  (dimension-spec
   ( :LEFT_SQUARE_BRACKET :number :RIGHT_SQUARE_BRACKET (compose #'second #'list) )
   ( :LEFT_SQUARE_BRACKET :RIGHT_SQUARE_BRACKET (constantly '*) )))

(defun parse-service (source)
  "The the contents of the stream SOURCE and return the resulting
partially post-processed syntax tree."
  (bind ((*field-number* 0)
	 ((:values lexer position1) (make-stream-lexer source)))
    (handler-case
	(yacc:parse-with-lexer lexer *service-parser*)
      (yacc:yacc-runtime-error (condition)
	(bind (((:values offset line column) (funcall position1)))
	  (error 'pbf::proto-parse-error
		 :offset            offset
		 :line              line
		 :column            column
		 :causing-condition condition))))))
