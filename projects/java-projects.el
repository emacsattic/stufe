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

