;;; pack.lisp ---
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

(defvar *system-definitions* nil
  "TODO(jmoringe): document")

(defvar *message-definitions* (make-hash-table :test #'equal)
  "TODO(jmoringe): document")

(defvar *service-definitions* (make-hash-table :test #'equal)
  "TODO(jmoringe): document")

(defclass ros-system (asdf:system)
  ()
  (:documentation
   "Instances of the class represent ROS packages."))

(defclass ros-message-file (asdf:source-file)
  ()
  (:default-initargs
   :type "msg")
  (:documentation
   "Instances of this class represent ROS message definition files."))

(defmethod asdf:perform ((op asdf:compile-op) (component ros-message-file))
  "TODO(jmoringe): document"
  nil)

(defmethod asdf:perform ((op asdf:load-op) (component ros-message-file))
  "TODO(jmoringe): document"
  (format t "  Loading ROS message definition from ~S~%" (asdf:component-pathname component))
  (with-input-from-file (stream (asdf:component-pathname component))
    (setf (gethash (asdf:component-name component) *message-definitions*)
	  (parse stream))))

(defclass ros-service-file (asdf:source-file)
  ()
  (:default-initargs
   :type "srv")
  (:documentation
   "Instances of this class represent ROS service definition files."))

(defmethod asdf:perform ((op asdf:compile-op) (component ros-service-file))
  "TODO(jmoringe): document"
  nil)

(defmethod asdf:perform ((op asdf:load-op) (component ros-service-file))
  "TODO(jmoringe): document"
  (format t "  Loading ROS service definition from ~S~%" (asdf:component-pathname component))
  (with-input-from-file (stream (asdf:component-pathname component))
    (setf (gethash (asdf:component-name component) *service-definitions*)
	  (parse-service stream))))

(defun make-file (class name module)
  "TODO(jmoringe): document"
  (make-instance class
		 :name     name
		 :parent   module
		 :pathname (merge-pathnames
			    (make-pathname :name name
					   :type (class-file-type class))
			    (asdf:component-pathname module))))

(defmethod class-file-type ((class symbol))
  (class-file-type (find-class class)))

(defmethod class-file-type ((class class))
  "TODO(jmoringe): document"
  (closer-mop:finalize-inheritance class)
  (second (find :type (closer-mop:class-default-initargs class)
		:key #'first)))

(defun make-module (name system component-class files description)
  "TODO(jmoringe): document"
  (let ((type   (class-file-type component-class))
	(module (make-instance 'asdf:module)))
    (reinitialize-instance
     module
     :name                    name
     :parent                  system
     :pathname                (merge-pathnames
			       (make-pathname :directory (list :relative type))
			       (asdf:component-pathname system))
     :description             description
     :default-component-class component-class)
    (reinitialize-instance
     module
     :components  (iter (for file in files)
			(let ((name (pathname-name (parse-namestring file))))
			  (collect (make-file component-class name module)))))))

;; TODO default component class
;;; TODO(jmoringe): absolute pathname
(defun load-ros-system (directory)
  (bind ((name      (lastcar (pathname-directory directory)))
	 (manifest  (merge-pathnames "manifest.xml" directory))
	 (directory (containing-directory manifest))
	 (system    (make-instance 'ros-system))
	 (doc       (with-input-from-file
			(stream manifest
				:element-type '(unsigned-byte 8))
		      (cxml:parse-stream stream (stp:make-builder)))))
    (xloc:with-locations-r/o
	((description-brief           "package/description/@brief"
	                              :if-no-match :do-nothing)
	 (description-long            "package/description/text()"
				      :if-no-match :do-nothing) ;;; TODO(jmoringe): there can be multiple nodes here and not only text nodes
	 (author                      "package/author/text()"
				      :if-no-match :do-nothing)
	 (license                     "package/license/text()"
				      :if-no-match :do-nothing)
	 (dependencies                "package/depend/@package"
				      :if-multiple-matches :all)
	 (ros-dependencies            "package/rosdep/@name"
				      :if-multiple-matches :all)
	 (build-dependencies          "package/rosbuild2/depend/@package"
				      :if-multiple-matches :all)
	 ((:val messages :type 'list) "package/rosbuild2/msgs/text()"
	                              :if-no-match :do-nothing)
	 ((:val services :type 'list) "package/rosbuild2/srvs/text()"
	                              :if-no-match :do-nothing)) doc
      (reinitialize-instance
       system
       :name              name
       :pathname          directory
       :source-file       manifest
       :version           "1.0" ;;; TODO(jmoringe): seriously? no version?
       :author            author
       :maintainer        author
       :license           license
       :description       description-brief
       :long-description  (string-trim (coerce '(#\Space #\Tab #\Newline) 'string)
				       description-long)
       :in-order-to             `((asdf:load-op
				   (asdf:load-op ,@(append dependencies
							   ;; ros-dependencies
							   ;; build-dependencies
							   )))) ;; TODO which?

       :default-component-class nil)
      (reinitialize-instance
       system
       :components        (list (make-module
				 "messages"
				 system
				 'ros-message-file
				 (append messages
					 (map 'list (lambda (p) (make-pathname :directory '(:relative "msg") :defaults p))
					      (directory (merge-pathnames "msg/*.msg" directory))))
				 (format nil "Message definitions used by the ~A system."
					 name))
				(make-module
				 "services"
				 system
				 'ros-service-file
				 (append services
					 (map 'list (lambda (p) (make-pathname :directory '(:relative "srv") :defaults p))
					      (directory (merge-pathnames "srv/*.srv" directory))))
				 (format nil "Service definitions used by the ~A system."
					 name)))
       )
      (asdf::register-system name system)

      (push system *system-definitions*)

      system)))


;;;
;;

(defun containing-directory (pathname)
  "TODO(jmoringe): document"
  (make-pathname :directory (pathname-directory pathname)))
