;; *************************************************
;; * 
;; * Functions for the Java mode
;; *
;; *************************************************


(defun stufe-create-new-java-class (classname)
  "Create a new java class"
  (interactive "*sClass name: ")
  (let ((filepath (expand-file-name (downcase classname)
				    (file-name-directory buffer-file-name))))
    (stufe-template-args-into-file "java-class"
				     (list classname "")
				     (concat filepath ".java")
				     'view-file
				     'no-save
				     'indent)))


(defun stufe-create-new-java-function (function-declaration)
  "Create a new java function in Java mode"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "java-member-declaration"
				   (list function-declaration
					 (stufe-make-arguments-documentation
					  function-declaration)
					 (stufe-make-return-value-documentation
					  function-declaration)))
   'indent))


(defun stufe-create-new-java-variable (variable-declaration)
  "Create a new java variable in Java mode"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "java-member-declaration"
				   (list variable-declaration))
   'indent))


(defun stufe-create-new-java-class-member (member-declaration)
  "Create a new variable/function in Java mode"
  (interactive "*sMember declaration: ")
  (if (stufe-is-a-function-declaration member-declaration)
      (stufe-create-new-java-function member-declaration)
    (stufe-create-new-java-variable member-declaration)))
