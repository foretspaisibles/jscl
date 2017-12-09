;;;; ffi2.lisp -- Foreign function interface extension

;; We need a `define-foreign-js-class` which maps a JS class as a
;; CL-ish structure and a `define-js-class` which defines a ES6 class
;; from common lisp.


;; define-foreign-js-class

;; ---- JSCL/CL compatibility ----
;; Slurp this in your CL-repl to be able to expand the
;; DEFINE-FOREIGN-JS-CLASS macro.
(in-package :jscl)
(defun oget (&rest whatever) ())
(defun oset (&rest whatever) ())
(defun make-new (&rest whatever) ())
(export '(oget oset objectp js-null-p make-new *root*) "JSCL")
(in-package :cl-user)

(defun concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun ensure-list (x)
  (if (listp x) x (list x)))
;; ---- END JSCL/CL compatibility ----

(defun js-identifier-from-lisp-identifier (name)
  "Convert a Lisp identifier to a JavaScript camelCase identifier."
  (let ((words
          (loop for i = 0 then (+ 1 j)
                as j = (position #\- name :start i)
                collect (subseq name i j)
                while j)))
    (apply #'jscl::concat (string-downcase (car words))
           (mapcar #'string-capitalize (cdr words)))))

(defun %canonize-slot-description (sd)
  "Canonize slot descriptions."
  (cond
    ((symbolp sd)
     (list sd))
    ((and (listp sd) (car sd) (evenp (length (cdr sd))))
     (let ((options nil))
       (do ((tail (cdr sd) (cddr tail)))
           ((null tail) options)
         (push (list (first tail) (second tail)) options))
       (push (car sd) options)
       options))
    (t
     (error "Bad slot description `~S'." sd))))

;; DEFINE-FOREIGN-JS-CLASS NAME-AND-OPTIONS PROPERTY-SLOTS METHOD-SLOTS
;;  Define functions to access instances of a particular JS class.
;;
;; OPTIONS:
;;  :predicate
;;    The name of the predicate recognising instances of class NAME.
;;    When not provided, the name NAME-P.  When set to nil, no public
;;    predicate is generated.
;;
;;  :implicit-instance
;;    Set an instance to be used as instance parameter by all
;;    methods. This is useful when writing bindings to JS modules or
;;    to classes having a notion of “the current instance being
;;    processed”.  When defined, a macro WITH-NAME is created that
;;    allows to rebind the implicit instance when evluating forms.
;;
;;  :validate-class
;;    When set to T, the predicate recognising instances of class NAME
;;    is called in all accessors and methods to validate the instance
;;    parameter provided.
;;
;;  :constructor
;;    When provided, a function MAKE-NAME calling the given
;;    constructor is defined.

(defmacro define-foreign-js-class (name-and-options property-slots method-slots)
  (let* ((name-and-options (ensure-list name-and-options))
         (name (first name-and-options))
         (name-string (symbol-name name))
         (options (rest name-and-options))
         (predicate (if (assoc :predicate options)
                        (second (assoc :predicate options))
                        (intern (concat name-string "-P"))))
         (validate-class (when (assoc :validate-class options)
                           (second (assoc :validate-class options))))
         (actual-constructor (when (assoc :constructor options)
                               (second (assoc :constructor options))))
         (constructor (intern (concat "MAKE-" name-string)))
         (implicit-instance (when (assoc :implicit-instance options)
                            (second (assoc :implicit-instance options))))
         (instance-parameter (or implicit-instance 'instance-parameter))
         (value-parameter 'value-parameter)
         (read-only (if (assoc :read-only options)
                        (second (assoc :read-only options))
                        nil))

         (property-slot-descriptions
           (mapcar #'%canonize-slot-description property-slots))

         (method-slot-descriptions
           (mapcar #'%canonize-slot-description method-slots))
         predicate-expansion
         validate-class-form
         constructor-expansion)

    (unless predicate
      (setq predicate (gensym "PREDICATE")))

    (setq predicate-expansion `(defun ,predicate (x) t))

    (when validate-class
      (setq validate-class-form
            `((unless (,predicate ,instance-parameter)
                (error "The object `~S' is not of type `~S'" ,instance-parameter ,name-string)))))

    (when actual-constructor
      (setq constructor-expansion
            `((defun ,constructor (&rest args)
                (apply #'jscl:make-new ,actual-constructor args)))))

    `(progn
       ,predicate-expansion
       ,@constructor-expansion
       ,@(let (forms)
           (loop for slot in property-slot-descriptions
                 do
                 (let* ((name (first slot))
                        (options (rest slot))
                        (accessor-name (intern (concat name-string "-" (string name))))
                        (accessor-ll (if implicit-instance `() `(,instance-parameter)))
                        (setter-name (gensym (concat "SETF-" name-string "-" (string name))))
                        (js-name (if (assoc :js-name options)
                                     (second (assoc :js-name options))
                                     (js-identifier-from-lisp-identifier (string name)))))
                   (push
                    `(defsetf ,accessor-name ,setter-name)
                    forms)
                   (push
                    `(defun ,accessor-name ,accessor-ll
                       ,@validate-class-form
                       (jscl:oget ,instance-parameter ,js-name))
                    forms)
                   (push
                    `(defun ,setter-name (,@accessor-ll ,value-parameter)
                       ,@validate-class-form
                       (jscl:oset ,value-parameter ,instance-parameter
                                  ,js-name))
                    forms)))
           forms)
       ,@(loop for slot in method-slot-descriptions
               collect
               (let* ((name (first slot))
                      (options (rest slot))
                      (method-name (intern (concat name-string "-" (string name))))
                      (method-ll (if implicit-instance
                                     `(&rest args)
                                     `(,instance-parameter &rest args)))
                      (js-name (if (assoc :js-name options)
                                   (second (assoc :js-name options))
                                   (js-identifier-from-lisp-identifier (string name)))))
                 `(defun ,method-name ,method-ll
                    ,@validate-class-form
                    (apply ((jscl:oget ,instance-parameter ,js-name "bind") ,instance-parameter) args)))))))

(defparameter *js-array*
'(define-foreign-js-class (js-array (:validate-class t) (:constructor |Array|))
  ((length
    :documentation "The length property of an object which is an instance of type Array sets or returns the number of elements in that array."
    :type number))
  ((push
    :documentation "The PUSH method adds one or more elements to the end of an array and returns the new length of the array.")
   (pop
    :documentation "The POP method removes the last element from an array and returns that element. This method changes the length of the array."))))

(defun test-js-array () (macroexpand-1 *js-array*))

(defparameter *example-1*
  '(define-foreign-js-class
  (event (:implicit-instance *current-event*))
  ((bubbles
    :documentation "A Boolean indicating whether the event bubbles up through the DOM or not."
    :type boolean)
   (cancelable
    :documentation "A Boolean indicating whether the event is cancelable."
    :type boolean)
   (current-target
    :documentation "A reference to the currently registered target for the event. This is the object to which the event is currently slated to be sent; it's possible this has been changed along the way through retargeting."
    :type dom-event-target)
   (default-prevented
    :documentation "Indicates whether or not event.preventDefault() has been called on the event."
    :type boolean)
   (event-phase
    :documentation "Indicates which phase of the event flow is being processed."
    :type number)
   (is-trusted :type boolean)
   (native-event :type dom-event)
   (target
    :documentation "A reference to the target to which the event was originally dispatched."
    :type dom-event-target)
   (timestamp
    :documentation "The time at which the event was created, in milliseconds."
    :type number)
   (type
    :documentation "The name of the event (case-insensitive)."
    :type string))
  ((prevent-default
    :documentation "Cancels the event (if it is cancelable).")
   is-default-prevented
   (stop-propagation
    :documentation "Prevents further propagation of the current event in the capturing and bubbling phases.")
   is-propagation-stopped)))

(defun test-1 () (macroexpand-1 *example-1*))


;; Example 1:
;;   Synthetic React-Dom events (Event class)
;;
;;
;; Example 2:
;;   Read a file with NodeJS

(defparameter *current-event* nil
  "The React event currently processed.")

(define-foreign-js-class
  (event (:implicit-instance *current-event*))
  ((bubbles
    :documentation "A Boolean indicating whether the event bubbles up through the DOM or not."
    :type boolean)
   (cancelable
    :documentation "A Boolean indicating whether the event is cancelable."
    :type boolean)
   (current-target
    :documentation "A reference to the currently registered target for the event. This is the object to which the event is currently slated to be sent; it's possible this has been changed along the way through retargeting."
    :type dom-event-target)
   (default-prevented
    :documentation "Indicates whether or not event.preventDefault() has been called on the event."
    :type boolean)
   (event-phase
    :documentation "Indicates which phase of the event flow is being processed."
    :type number)
   (is-trusted :type boolean)
   (native-event :type dom-event)
   (target
    :documentation "A reference to the target to which the event was originally dispatched."
    :type dom-event-target)
   (timestamp
    :documentation "The time at which the event was created, in milliseconds."
    :type number)
   (type
    :documentation "The name of the event (case-insensitive)."
    :type string))
  ((prevent-default
    :documentation "Cancels the event (if it is cancelable).")
   is-default-prevented
   (stop-propagation
    :documentation "Prevents further propagation of the current event in the capturing and bubbling phases.")
   is-propagation-stopped))

(define-js-class
  (my-object
   :extends #j:Object
   :documentation "A JS object with special capabilities.")
  ((special
    :documentation "Indicates a special object.")))





;;; Test

;; (in-package :jscl)

;; (defparameter *sexp-ffi-example*
;;   '(create-class (%js-vref "MyArray" t) (%js-vref "Array" t) nil))

;; (defun ffi-example ()
;;   (with-compilation-environment
;;     (compile-toplevel *sexp-ffi-example*)))

;; (defun my-compile (sexp)
;;   (with-compilation-environment
;;     (compile-toplevel sexp)))
