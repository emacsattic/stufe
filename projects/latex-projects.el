;; *************************************************
;; * 
;; * Functions to create a new LaTeX project
;; *
;; *************************************************

;; C/C++ project
(stufe-register-project 
 '("latex-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"latex-makefile" 
					"makefile"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"latex-main" 
					(concat (stufe-format-standard-name 
						 (stufe-project-get-name project-details))
						".tex")
					't 't)))))


;; Add the menu item

(setq stufe-menu-latex-projects-context
      (stufe-add-menu-item-group "LaTeX Projects" 
				   (stufe-get-project-menu)))

(stufe-project-add-menu-item stufe-menu-latex-projects-context
			       "New LaTeX project..." 
			       "latex-project")



