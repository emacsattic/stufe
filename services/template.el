(stufe-load-file "register.el")


;; *************************************************
;; * 
;; * Function to use a template
;; *
;; *************************************************

(defun stufe-apply-on-template (template-name)
  "Create a new string from a template name asking arguments from the mini-buffer"
  (stufe-apply-args-on-template template-name
				  (stufe-read-template-arguments
				   (stufe-get-template-description template-name))))


(defun stufe-apply-args-on-template (template-name
				       template-args)
  "Create a new string from a template name and a list of its arguments"
  (let* ((template-description (stufe-get-template-description template-name))
	 (template-model (stufe-get-template-model template-description))
	 (template-keywords (stufe-get-template-keywords template-description)))
    (stufe-apply-template-transformation template-model
					   template-keywords
					   template-args)))

(defun stufe-template-args-into-file (template-name
					template-args
					filename
					&optional
					view-file
					no-save
					indent)
  "Create a new file from a template name and a list of its arguments.
Open the file in a buffer if 'open-file-in-buffer' is not null"
  (stufe-string-to-file (stufe-apply-args-on-template template-name
							  template-args)
			  filename
			  view-file
			  no-save
			  indent))

;; *************************************************
;; * 
;; * Function to register templates
;; *
;; *************************************************


(setq stufe-template-list (stufe-register-new "templates"))

(defun stufe-get-template-description (template-name)
  (stufe-register-get-description template-name stufe-template-list))


(defun stufe-register-template (template-description)
  (setq stufe-template-list
	(stufe-register-add-data template-description
				   stufe-template-list)))


(defun stufe-template-set-model (template-name
				   template-model)
  "Set a new model file for a template"
  (let ((template-description (stufe-get-template-description 
			       template-name)))
    (stufe-register-template
     (list template-name
	   (stufe-get-template-keywords template-description)
	   template-model
	   (stufe-get-template-options template-description)))))


;; *************************************************
;; * 
;; * Function to get data from a template 
;; * description
;; *
;; *************************************************

(defun stufe-get-template-name (template-description)
  "Returns the template name"
  (car template-description))


(defun stufe-get-template-keywords (template-description)
  "Returns the template keywords"
  (cadr template-description))


(defun stufe-get-template-options (template-description)
  "Returns template options"
  (cadr (cdr (cdr template-description))))


(defun stufe-get-option-keyword (stufe-template-keyword)
  "Returns the option of a keyword from a template keyword"
  (if (and (arrayp stufe-template-keyword)
	   (> (length stufe-template-keyword) 2))
      (aref stufe-template-keyword 2)
    'nil))


(defun stufe-get-model-keyword (stufe-template-keyword)
  "Returns the model keyword (and not its description) from a template keyword"
  (if (arrayp stufe-template-keyword)
      (aref stufe-template-keyword 1)
    (stufe-get-template-name stufe-template-keyword)))

(defun stufe-get-description-keyword (stufe-template-keyword)
  "Returns the description of a keyword, nil if this keyword is a
template"
 (if (arrayp stufe-template-keyword)
      (aref stufe-template-keyword 0)
    nil))


(defun stufe-get-keyword-function (stufe-template-keyword)
  "Returns the function for this keyword the get its replacement"
  (if (arrayp stufe-template-keyword)
      'stufe-read-keyword-from-minibuffer
    (let ((template-description-length 3))
      (if (eq (length stufe-template-keyword) 
	      template-description-length)
	  'stufe-apply-on-template-desc
	'stufe-apply-on-template))))


;; *************************************************
;; * 
;; * Function to find template model
;; *
;; ************************************************* 

(defun stufe-add-model-path (model-path)
  "Add a new model path"
  (setq stufe-template-model-file-folder
	(cons model-path stufe-template-model-file-folder)))

(defun stufe-get-template-model (template-description)
  "Returns the template model (search in file if necessary)"
  (let ((model-description (cadr (cdr template-description))))
    (if (stufe-template-model-from-file template-description)
	(let ((filename (stufe-find-file-in-folder-list model-description 
							  stufe-template-model-file-folder)))
	  (if filename
	      (stufe-file-to-string filename)
	    (error "Can't find the model \"%s\" for the template \"%s\""
		   model-description
		   (stufe-get-template-name template-description))))
      model-description)))

;; *************************************************
;; * 
;; * Function to deal with template options
;; *
;; ************************************************* 

(defun stufe-template-has-option (template-description template-option)
  "Return true is the template has the option \"template-option\""
  (member template-option 
	  (stufe-get-template-options template-description)))

(defun stufe-template-model-from-file (template-description)
  (stufe-template-has-option template-description
			       'model-from-file))

					       
;; *************************************************
;; * 
;; * Function to build the argument list from the
;; * mini buffer
;; *
;; ************************************************* 

(defun stufe-read-template-arguments (template-name)
  "Return a list or args from the mini-buffer for a template"
  (let* ((template-description (stufe-get-template-description template-name))
	 (read-argument (lambda (template-keyword-description)
			 (funcall (stufe-get-keyword-function 
				   template-keyword-description) 
				  template-keyword-description))))
    (mapcar read-argument 
	    (stufe-get-template-keywords template-description))))


(defun stufe-read-keyword-from-minibuffer (stufe-template-keyword)
  "Read a keyword with its description from the mini-buffer"
  (read-string (concat (stufe-get-description-keyword stufe-template-keyword) ": ")))


;; *************************************************
;; * 
;; * Function to create the string from a model of 
;; * template, the keyword used and their 
;; * corresponding arguments
;; *
;; ************************************************* 

(defun stufe-get-keyword-replacement (template-keyword
					template-argument)
  "Get the corresponding replacement for a keyword"
  (let ((keyword-option (stufe-get-option-keyword template-keyword)))
    (if keyword-option 
	(cond 
	 ((eq keyword-option 'capital) (upcase template-argument))

	 ((eq (car keyword-option) 'default)
	  (if (or (not template-argument)
		  (string= template-argument ""))
	      (cdr keyword-option)
	    template-argument))

	 ((eq (car keyword-option) 'function)
	  (funcall (cdr keyword-option) template-argument))

	 (t template-argument))
      template-argument)))
	
  

(defun stufe-apply-template-transformation (template-model 
					      template-keywords
					      template-arguments)
  "Replace all the model keywords in the model by their substitution"
  (if template-keywords
      (let ((template-transformed (stufe-apply-template-transformation 
				   template-model
				   (cdr template-keywords)
				   (cdr template-arguments))))
;;	(message "Arguments: %s
;;Keywords: %s
;;Model: 
;;%s" 
;;		 (cdr template-arguments)
;;		 (cdr template-keywords)
;;		 template-transformed)
	(stufe-template-replace-words template-transformed
					(stufe-get-model-keyword (car template-keywords))
					(stufe-get-keyword-replacement (car template-keywords)
									 (car template-arguments))))
    template-model))



(defun stufe-template-replace-words (stufe-string 
				       stufe-substring 
				       stufe-word)
  (if (not stufe-word)
      (setq stufe-word ""))
  (stufe-replace-words
   (stufe-replace-words 
    (stufe-replace-words stufe-string
			   stufe-substring
			   stufe-word)
    (upcase stufe-substring)
    (upcase stufe-word))
   (downcase stufe-substring)
   (downcase stufe-word)))
  





