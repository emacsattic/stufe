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

;; Need the cpp menu
(stufe-require-file "projects/cpp-projects.el")


;; *************************************************
;; * 
;; * Functions to create a new Autotools project
;; *
;; *************************************************

;; Autotools project
(stufe-register-project 
 '("autotools-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-Makefile.am" 
				      "Makefile.am"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-configure.ac"
				      "configure.ac"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-bootstrap" 
				      "bootstrap"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
				      "cpp-main" 
				      "main.cpp"
				      't 't)))))


;; Autotools sub project
(stufe-register-project 
 '("autotools-sub-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-Makefile.am" 
				      "Makefile.am"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
				      "cpp-main" 
				      "main.cpp"
				      't 't)))))


;; Autotools library project
(stufe-register-project 
 '("autotools-library-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-library-Makefile.am" 
				      "Makefile.am"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-library-configure.ac"
				      "configure.ac"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-bootstrap" 
				      "bootstrap"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
				      "cpp-main-so" 
				      (concat (stufe-format-standard-name 
					       (stufe-project-get-name project-details))
					      ".cpp")
				      't 't)))))


;; Autotools library sub project
(stufe-register-project 
 '("autotools-library-sub-project"
   (stufe-projects-new-folder
    (lambda (project-details)
      (stufe-project-template-to-file project-details
				      "autotools-library-Makefile.am" 
				      "Makefile.am"))
    (lambda (project-details)
      (stufe-project-template-to-file project-details 
				      "cpp-main-so" 
				      (concat (stufe-format-standard-name 
					       (stufe-project-get-name project-details))
					      ".cpp")
				      't 't)))))

;; Add the menu item

(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			     "New Autotools project..." 
			     "autotools-project")

(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			     "New Autotools sub project..." 
			     "autotools-sub-project")


(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			     "New Autotools library project..." 
			     "autotools-library-project")

(stufe-project-add-menu-item stufe-menu-cpp-projects-context
			     "New Autotools library sub project..." 
			     "autotools-library-sub-project")



