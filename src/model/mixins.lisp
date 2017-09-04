;;;; mixins.lisp --- Mixins class for the model module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model)

;;; `parented-mixin' mixin class

(defclass parented-mixin ()
  ((parent :initarg  :parent
           :accessor parent
           :initform nil
           :documentation
           "Stores the parent of the type."))
  (:documentation
   "Intended to be mixed into classes that have an associated parent
    object."))

(defmethod qname ((thing parented-mixin))
  (if-let ((parent (parent thing)))
    (append (qname parent) (list (name thing)))
    (list :absolute (name thing))))

(defmethod qname/kind ((thing parented-mixin))
  (let ((cell (cons (name thing) (kind thing))))
    (if-let ((parent (parent thing)))
      (append (qname/kind parent) (list cell))
      (list :absolute cell))))

(defmethod (setf parent) :before ((new-value t)
                                  (thing     parented-mixin))
  (when (member thing (ancestors new-value))
    (simple-child-error new-value thing
                        "~@<Cyclic parent relation ~{~A~^ -> ~}.~@:>"
                        (list* thing (ancestors new-value)))))

(defmethod architecture.builder-protocol:relate ((builder  t)
                                                 (relation (eql :parent))
                                                 (left     parented-mixin)
                                                 (right    t)
                                                 &key)
  (setf (parent left) right)
  left)

;;; `sequence-composite-mixin' mixin class

(defclass sequence-composite-mixin ()
  ()
  (:documentation
   "Intended to be mixed into sequence composite classes.

    That is, children are organized as sequence."))

(defmacro define-sequence-composite-mixin
    (name
     &key
     (class-name       (format-symbol *package* "~A-MIXIN" name))
     (kind             (make-keyword name))
     (kind-specializer (typecase kind
                         (keyword `(eql ,kind))
                         (t       kind)))
     (slot-name        (format-symbol *package* "~A" name))
     (accessor-name    (format-symbol *package* "%~A" slot-name))
     (set-parent?      t))
  "Define a class named NAME which implements to composite
   protocol (i.e `contents')."
  `(progn
     (defclass ,class-name (sequence-composite-mixin)
       ((,slot-name :type     vector
                    :accessor ,accessor-name
                    :initform (make-array 0 :adjustable t :fill-pointer 0)
                    :documentation
                    ,(format nil "Stores the contents of kind ~A of ~
                                  the container."
                             kind)))
       (:documentation
        ,(format nil "This class is intended to be mixed into classes ~
                      which implement the composite protocol for ~
                      kind ~A."
                 kind)))

     (defmethod contents ((container ,class-name)
                          (kind      ,kind-specializer))
       (coerce (,accessor-name container) 'list))

     (defmethod contents ((container ,class-name)
                          (kind      (eql t)))
       ,(typecase kind
          (keyword
           `(nconc (when (next-method-p)
                     (call-next-method))
                   (contents container ,kind)))
          (t
           `(call-next-method))))

     (defmethod print-items:print-items append ((object ,class-name))
       `((,',kind
          ,(length (,accessor-name object))
          ,',(format nil " (~C ~~D)" (aref (string name) 0)))))

     (defmethod architecture.builder-protocol:relate ((builder  t)
                                                      (relation (eql ,kind))
                                                      (left     ,class-name)
                                                      (right    t)
                                                      &rest args &key)
       (when args
         (error "~A ~A ~A ~A does not accept args ~S" builder relation left right args))
       (vector-push-extend right (,accessor-name left))
       left)

     ,@(when set-parent?
         `((defmethod architecture.builder-protocol:relate :around ((builder  t)
                                                                    (relation (eql ,kind))
                                                                    (left     ,class-name)
                                                                    (right    parented-mixin)
                                                                    &key)
             (architecture.builder-protocol:relate builder :parent right left)
             (call-next-method))))

     (defmethod architecture.builder-protocol:node-relations ((builder  t)
                                                              (node     ,class-name))
       (list* `(,,kind . *)
              (when (next-method-p) (call-next-method))))

     (defmethod architecture.builder-protocol:node-relation ((builder  t)
                                                             (relation (eql ,kind))
                                                             (node     ,class-name))
       (coerce (,accessor-name node) 'list)) ; TODO

     ',class-name))

;;; `mapping-composite-mixin' mixin class

(defclass mapping-composite-mixin ()
  ()
  (:documentation
   "Intended to be mixed into mapping composite classes.

    That is, children are organized as table mapping keys to child
    objects."))

(defmacro define-mapping-composite-mixin
    (name
     &key
     (class-name        (format-symbol *package* "~A-MIXIN" name))
     (kind              (make-keyword name))
     (kind-specializer  (typecase kind
                          (keyword `(eql ,kind))
                          (t       kind)))
     (slot-name         (format-symbol *package* "~A" name))
     (accessor-name     (format-symbol *package* "%~A" slot-name))
     (key-type          'string)
     (key-class         key-type)
     (make-key-form     (typecase kind
                          (keyword (lambda (kind-var key-var)
                                     (declare (ignore kind-var))
                                     key-var))
                          (t       (lambda (kind-var key-var)
                                     `(cons ,kind-var ,key-var)))))
     (key-func/any-kind (typecase kind
                          (keyword 'identity)
                          (t       'rest)))
     (set-parent?       t))
  "Define a class named NAME which implements to composite
   protocol (i.e `contents' and `lookup').

   MAKE-KEY-FORM is a function of two arguments, the name of the
   variable holding KIND and the name of the variable holding KEY,
   which returns a form which constructs a composite key using the two
   names.

   KEY-FUNC/ANY-KIND is applied to stored keys when `lookup' is called
   with KIND `t'. The returned value is compared to the KEY of the
   `lookup' call."
  `(progn
     (defclass ,class-name (mapping-composite-mixin)
       ((,slot-name :type     hash-table
                    :accessor ,accessor-name
                    :initform (make-hash-table :test #'equal)
                    :documentation
                    ,(format nil "Stores the contents of kind ~A of ~
                                  the container."
                             kind)))
       (:documentation
        ,(format nil "This class is intended to be mixed into classes ~
                      which implement the composite protocol for ~
                      kind ~A."
                 kind)))

     (defmethod contents ((container ,class-name)
                          (kind      ,kind-specializer))
       (hash-table-values (,accessor-name container)))

     (defmethod contents ((container ,class-name)
                          (kind      (eql t)))
       ,(typecase kind
          (keyword
           `(nconc (when (next-method-p)
                     (call-next-method))
                   (contents container ,kind)))
          (t
           `(call-next-method))))

     (defmethod lookup ((container ,class-name)
                        (kind      ,kind-specializer)
                        (key       ,key-class)
                        &key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
           `((check-type key ,key-type)))

       (or (when (next-method-p)
             (call-next-method))
           (values (gethash ,(funcall make-key-form 'kind 'key)
                            (,accessor-name container)))))

     (defmethod lookup ((container ,class-name)
                        (kind      (eql t))
                        (key       ,key-class)
                        &key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
           `((check-type key ,key-type)))

       (or (when (next-method-p)
             (call-next-method))
           (cdr (find key (hash-table-alist (,accessor-name container))
                      :test #'equal
                      :key  (compose #',key-func/any-kind #'car)))))

     (defmethod (setf lookup) ((new-value t)
                               (container ,class-name)
                               (kind      ,kind-specializer)
                               (key       ,key-class)
                               &key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
           `((check-type key ,key-type)))

       (setf (gethash ,(funcall make-key-form 'kind 'key)
                      (,accessor-name container))
             new-value))

     ,@(when set-parent?
         `((defmethod (setf lookup) :around ((new-value parented-mixin)
                                             (container ,class-name)
                                             (kind      ,kind-specializer)
                                             (key       ,key-class)
                                             &key &allow-other-keys)
             (let ((old-parent (parent new-value)))
               (setf (parent new-value) container)
               (handler-bind
                   ((error (lambda (condition)
                             (declare (ignore condition))
                             (setf (parent new-value) old-parent))))
                 (call-next-method))))))

     (defmethod print-items:print-items append ((object ,class-name))
       `((,',(make-keyword name)
          ,(hash-table-count (,accessor-name object))
          ,',(format nil " (~C ~~D)" (aref (string name) 0)))))

     (defmethod architecture.builder-protocol:relate ((builder  t)
                                                      (relation (eql ,(make-keyword name)))
                                                      (left     ,class-name)
                                                      (right    t)
                                                      &rest args &key key)
       ,@(when key-type
           `((check-type key ,key-type)))
       (assert (length= 2 args))

       (setf (gethash ,(funcall make-key-form 'relation 'key)
                      (,accessor-name left))
             right)
       left)

     ,@(when set-parent?
         `((defmethod architecture.builder-protocol:relate :around ((builder  t)
                                                                    (relation (eql ,(make-keyword name)))
                                                                    (left     ,class-name)
                                                                    (right    parented-mixin)
                                                                    &key)
             (architecture.builder-protocol:relate builder :parent right left)
             (call-next-method))))

     (defmethod architecture.builder-protocol:node-relations ((builder  t)
                                                              (node     ,class-name))
       (list* `(,,(make-keyword name) . (:map . :key))
              (when (next-method-p) (call-next-method))))

     (defmethod architecture.builder-protocol:node-relation ((builder  t)
                                                             (relation (eql ,(make-keyword name)))
                                                             (node     ,class-name))
       (loop :for key :being :the :hash-keys :of (,accessor-name node) :using (:hash-value value)
             :collect `(:key ,key) :into args
             :collect value :into values
             :finally (return (values values args))))

     ',class-name))
