(in-package #:cl-user)

(defparameter *app-root* (#j:document:querySelector "#app"))

(defun move-console (top)
  (setf (jscl::oget (#j:document:querySelector "#console") "style" "top") top))

(defun move-console-3 (&rest whatever)
  (move-console "3em"))

(defun move-console-10 (&rest whatever)
  (move-console "10em"))


(defun js-identifier-from-lisp-identifier (name &optional capitalize)
  "Convert a Lisp identifier to a JavaScript camelCase identifier."
  (let ((words
          (loop for i = 0 then (+ 1 j)
                as j = (position #\- name :start i)
                collect (subseq name i j)
                while j)))
    (apply #'jscl::concat
           (if capitalize (string-capitalize (car words)) (string-downcase (car words)))
           (mapcar #'string-capitalize (cdr words)))))

(defun make-jsobj-keyname (key)
  "Make a keyname string from KEY."
  (cond
    ((stringp key) key)
    ((symbolp key) (js-identifier-from-lisp-identifier (symbol-name key)))
    (t (error "Cannot convert KEY `~S' to string." key))))

(defun make-jsobj (&rest init-plist)
  "Make a JavaScript object initialised with the given INIT-PLIST."
  (let ((object (jscl::make-new #j:Object)))
    (do ((tail init-plist (cddr tail)))
        ((null tail) object)
      (setf (jscl::oget object (make-jsobj-keyname (first tail)))
            (second tail)))
    object))

(defun ui-title (&key title)
  "Make a title for the UI."
  (let ((node
	  (#j:document:createElement "h1")))
    (setf (jscl::oget node "innerHTML") title)
    node))

(defun ui-text-paragraph (&key text)
  "Make a text paragraph for the UI."
  (let ((node
	  (#j:document:createElement "p")))
    (setf (jscl::oget node "innerHTML") text)
    node))

(defun ui-button (&key label on-click)
  (let ((node
	  (#j:document:createElement "button")))
    (setf (jscl::oget node "innerHTML") label)
    (when on-click
      (setf (jscl::oget node "onclick") on-click))
    node))
  

(defun ui-append (&rest nodes)
  (loop for node in nodes
	do ((jscl::oget *app-root* "shadowRoot" "append") node)))

(defun make-ui ()
  (let ((wrapper
	  (#j:document:createElement "span"))
	(style
	  (#j:document:createElement "style"))
	(title
	  (ui-title :title "Application")))
    ((jscl::oget *app-root* "attachShadow") (make-jsobj :mode "open"))
    ((jscl::oget wrapper "setAttribute") "class" "wrapper")
    (ui-append (ui-button :label "3em"
			  :on-click #'move-console-3))
    (ui-append (ui-button :label "10em"
			  :on-click #'(lambda (&rest whatever) (move-console "10em"))))
    (ui-append style wrapper title)
    (ui-append (ui-text-paragraph :text "La Commission a répondu favorablement à une initiative citoyenne qui avait recueilli 1,4 million de signatures. Elle présentera une proposition législative pour une entrée en vigueur en 2027."))
))



(progn
  (make-ui))
