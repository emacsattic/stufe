;; *************************************************
;; * 
;; * Functions to create a new Java project
;; *
;; *************************************************

;; Java project
(stufe-register-project 
 '("java-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"java-makefile" 
					"makefile"))
    (lambda (project-details)
      (let ((project-name  (stufe-project-get-name project-details))
	    (project-location  (stufe-project-get-location project-details)))
	(stufe-template-args-into-file "java-class"
					 (list project-name 
					       ""
					       (stufe-apply-args-on-template "java-member-declaration" 
									       '("public static void main(String [] args)")))
					 (expand-file-name (concat project-name ".java")
							   project-location)
					 'view 'no-save 'indent))))))
    
;; Add the new item in the projects menu of the stufe menu

(setq stufe-menu-cpp-projects-context
      (stufe-add-menu-item-group "Java Projects" 
				   (stufe-get-project-menu)))


(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			       "New Java application..." 
			       "java-project")

