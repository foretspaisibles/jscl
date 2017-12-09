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

;;; Example, following Ludovico Fischer

(define-react-component save-button
  ((save :documentation "The callback triggered by the Save button."
         :initform #'(lambda (&rest react-internal) nil)))
  nil
  (render (&rest react-internal)
    (create-element "button" :class-name "pb2 ph3" :on-click save "Save")))

(define-react-component alert-box
  ((status :documentation "The status of the last save operaion."
           :initform :idle))
  nil
  (render (&rest react-internal)
    (create-element
     "div"
     :class-name "mv2"
     (ecase status
       (:failure "Save failed")
       (:success "Save successful")
       (:waiting "Saving!")
       (:idle "")))))

(define-react-component counter
  ((count :documentation "The number of words typed so far."
          :initform 0))
  nil
  (render (&rest react-internal)
    (create-element "p" :class-name "mb2" (format nil "Word count: ~D" count))))

(define-react-component
  (progress-bar
   :documentation "A simple component displaying a progress bar.")
  ((completion :documentation "The completion rate for typing words."
               :initform 0.0))
  nil
  (render (&rest react-internal)
    (create-element
     "div"
     :class-name "mv2 flex flex-column"
     (create-element "label"
                     :html-for "progress"
                     :class-name "mv-2"
                     "Progress")
     (create-element "progress"
                     :value completion
                     :id "progress"
                     :class-name "bn"
                     (format nil "~D%" completion)))))

(define-react-component (editor :documentation "A simple editor")
    ((text :documentation "The initial text in the editor."
           :initform "")
     (placeholder :documentation "Prompt for typing any number of words."
                  :initform "Please type in text and see how the words are counted!")
     (on-change :initval (lambda (event) (jscl::/log event))))
  nil
  (render (&rest react-internal)
    (create-element
     "div"
     :class-name "mv2 flex flex-column"
     (create-element "label"
                     :html-for "editor"
                     :class-name "mv-2"
                     "Enter your text:")
     (create-element "textarea"
                     :on-change on-change
                     :value text
                     :placeholder placeholder
                     :id "editor"))))

(define-react-component
  (word-counter
   :documentation "A simple word counter")
  ((goal :documentation "The number of words we expect."
         :initform 5))
  ((text :documentation "The text typed so far."
         :initform ""))
  (render (&rest react-internal)
    (flet ((on-change (event)
             (set-text (dom-element-value (event-target event))))
           (count-words (text)
              (loop with number = 0
                 with in-word = nil
                 for char across text
                 do (cond
                      ((or (char= char #\Space) (not (graphic-char-p char)))
                       (setf in-word nil))
                      ((and (graphic-char-p char) (not in-word))
                       (setf number (1+ number)
                             in-word t))
                      (t nil))
                 finally (return number))))
      (create-element
       "form"
       :class-name "measure pa4 sans-serif"
       (create-element #'editor :text (get-text) :on-change #'on-change)
       (create-element #'counter :count (count-words (get-text)))
       (create-element #'progress-bar :completion (/ (count-words (get-text)) goal))))))

(define-react-component
  (save-manager
   :documentation "I am too lazy to write a meaningful documentation.")
  ((save :documentation "The callback triggered by the save button."
         :initform (lambda (data on-success on-failure)
                    (funcall on-failure "Not implemented")))
   (data :documentation "The data to save"
         :initform "This is the data to save."))
  ((save-status :documentation "The status of the save manager."
                :initform :idle))
  (render (&rest react-internal)
    (flet ((on-save (state)
             (funcall
              save
              data
              #'(lambda (success) (set-save-status :success))
              #'(lambda (failure)
                  (set-save-status :failure)))))
      (create-element
       "div"
       :class-name "flex flex-column mv2"
       (create-element #'save-button :save #'on-save)
       (create-element #'alert-box :save-status (get-save-status))))))

(defun install-wordcount-app (location)
  (dom-render
   (create-element #'word-counter
                   :save #'(lambda (data on-success on-failure)
                             (funcall on-failure "Oups"))
                   :data "This is the real data to save")
   location))

;;; Example, following ParenScript documentation

(define-react-component todo-list
  ((items :documentation "The list of items displayed by the todo list."
          :initform '("Initialise this todo list.")))
  nil
  (render (&rest react-internal)
   (flet ((create-item (text) (create-element "li" text)))
     (create-element "ul" (mapcar #'create-item items)))))

(define-react-component todo-app
  ((placeholder :documentation "Prompt for new items."
                :initform "New item"))
  ((items :documentation "The list of items gathered so far."
          :initform '("Apple" "Peach" "Pear"))
   (text :documentation "The working copy of the next item."
         :initform ""))
  (render (&rest react-internal)
    (labels
        ((input-on-change (event)
           (set-text (dom-element-value (event-target event))))
         (form-on-submit (event)
           (event-prevent-default event)
           (let ((next-items (cons (get-text) (get-items)))
                 (next-text ""))
             (set-text next-text)
             (set-items next-items))))

      (create-element
       "div"
       (create-element "h3" :style '(:background-color "lightgrey") "TODO")
       (create-element #'todo-list :items (get-items))
       (create-element
        "form"
        :on-submit #'form-on-submit
        (if (string= (get-text) "")
            (create-element "input"
                            :type "text"
                            :value ""
                            :placeholder placeholder
                            :on-change #'input-on-change)
            (create-element "input"
                            :type "text"
                            :value (get-text)
                            :on-change #'input-on-change))
        (create-element "input"
                        :type "submit"
                        :style '(:cursor: "pointer")
                        :value "Add"))))))


(defun install-todo-app (location)
  (dom-render (create-element #'todo-app) location))

(install-wordcount-app "wordcount-app")
(install-todo-app "todo-app")
