;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 2000-2002 the Stufe project

;; This file is part of Stufe.

;; Stufe is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Stufe is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
