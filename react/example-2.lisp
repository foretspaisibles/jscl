;; References:
;;
;; CREATE-REACT-CLASS
;;  https://reactjs.org/docs/react-without-es6.html
;; EVENTS:
;;  https://reactjs.org/docs/events.html

(/debug "loading example.lisp!")

(defpackage :react-example
  (:use :cl :cl-user :jscl :react))

(in-package :react-example)


;;; Example, following ParenScript documentation

(jscl::%create-class
 "TodoList" "React.Component"
 ("constructor" (&rest args)
  (jscl::super nil)
  (setf (jscl::oget jscl::this "items") '("apple" "peach" "pear")))
 ("render" (&rest react-internal)
   (flet ((create-item (text) (create-element "li" text)))
     (create-element "ul" (mapcar #'create-item items)))))

(jscl::fset 'todo-list (jscl::%js-vref "TodoList"))

;; (define-react-component todo-list
;;   ((items :documentation "The list of items displayed by the todo list."
;;           :initform '("Initialise this todo list.")))
;;   nil
;;   (render (&rest react-internal)
;;    (flet ((create-item (text) (create-element "li" text)))
;;      (create-element "ul" (mapcar #'create-item items)))))


(defun test-1 (location)
  (dom-render (create-element #'todo-list) location))

(test-1 "app")
