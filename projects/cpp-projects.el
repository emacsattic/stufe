;; *************************************************
;; * 
;; * Functions to create a new C++ project
;; *
;; *************************************************

;; C/C++ project
(stufe-register-project 
 '("c-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"cpp-makefile" 
					"makefile"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"cpp-main" 
					"main.c"
					't 't)))))
(stufe-register-project 
 '("cpp-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"cpp-makefile" 
					"makefile"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
					"cpp-main" 
					"main.cpp"
					't 't)))))
;; C/C++ shared object project
(stufe-register-project 
 '("c-so-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-cpp-so-project project-details)))))
(stufe-register-project 
 '("cpp-so-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-cpp-so-project project-details)))))

;; Add the new item in the projects menu of the stufe menu

(setq stufe-menu-cpp-projects-context
      (stufe-add-menu-item-group "C/C++ Projects" 
				   (stufe-get-project-menu)))

(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			       "New C++ library project..." 
			       "cpp-so-project")
(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			       "New C++ project..." 
			       "cpp-project")
(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			       "New C library project..." 
			       "c-so-project")
(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			       "New C project..." 
			       "c-project")

;; Function used by the shared object project to create te makefile

(defun stufe-cpp-so-project (project-details)
  "Create a makefile to build shared object file"
  (let ((filepath (expand-file-name "makefile"
				    (stufe-project-get-location project-details))))
    (stufe-template-args-into-file "cpp-makefile"
				     (list (stufe-project-get-name project-details)
					   (stufe-apply-args-on-template "cpp-so-makefile-options"
									   '()))
				     filepath)))
