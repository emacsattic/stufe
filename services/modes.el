(stufe-load-file "register.el")

;; *************************************************
;; * 
;; * Function to associate a mode with a hook
;; *
;; *************************************************
(defmacro stufe-associate-mode-with-hook (emacs-hook
					    stufe-mode)
  "Associate a stufe mode with a hook"
  `(add-hook ,emacs-hook
	     '(lambda ()
		(stufe-menu-create-local "Project")
		(stufe-mode-eval ,stufe-mode))))

;; *************************************************
;; * 
;; * Function to evaluate a mode
;; *
;; *************************************************

(defun stufe-mode-eval (mode-name)
  "Evaluate all the functions in functions-list of a mode"
  (mapcar (lambda (function)
	    (if (stringp function)
		(stufe-mode-eval function)
	      (funcall function)))
	  (stufe-mode-get-fun-list (stufe-get-mode-description 
				      mode-name))))



;; *************************************************
;; * 
;; * Function to associate a list of actions to
;; * an emacs mode
;; *
;; *************************************************


(setq stufe-mode-list (stufe-register-new "modes"))

(defun stufe-get-mode-description (mode-name)
  (stufe-register-get-description mode-name stufe-mode-list))


(defun stufe-register-mode (mode-description)
  (setq stufe-mode-list
	(stufe-register-add-data mode-description
				   stufe-mode-list)))

(defun stufe-copy-mode (mode-origin mode-destination)
  (stufe-register-mode 
   (list mode-destination
	 (stufe-mode-get-fun-list 
	  (stufe-get-mode-description mode-origin)))))


;; *************************************************
;; * 
;; * Functions to access data from a mode 
;; * description
;; *
;; *************************************************

(defun stufe-mode-get-name (mode-description)
  "Return the name of a mode from its description"
  (car mode-description))


(defun stufe-mode-get-fun-list (mode-description)
  "Return the name of a mode from its description"
  (car (cdr mode-description)))










