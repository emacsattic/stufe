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

(stufe-load-file "register.el")


;; *************************************************
;; * 
;; * Function to add a project in the project menu
;; *
;; *************************************************


(defun stufe-get-project-menu ()
  "Return the project menu context"
  stufe-menu-projects-context)


(defmacro stufe-project-add-menu-item (context label project-name)
  "Add a new item in the project menu to run the project from it"
  `(stufe-add-menu-item ,context ,label
			  '(lambda ()
			     (interactive)
			     (if (stufe-project-need-details
				  (stufe-get-project-description ,project-name))
				 (stufe-project-eval ,project-name
						       (stufe-project-get-name-location))
			       (stufe-project-eval ,project-name)))))


;; *************************************************
;; * 
;; * Functions to register project
;; *
;; *************************************************

(setq stufe-project-list (stufe-register-new "projects"))

(defun stufe-get-project-description (project-name)
  (stufe-register-get-description project-name stufe-project-list))


(defun stufe-register-project (project-description)
  (setq stufe-project-list
	(stufe-register-add-data project-description
				   stufe-project-list)))

;; *************************************************
;; * 
;; * Functions to eval a project
;; *
;; *************************************************

(defun stufe-project-eval (project-name &optional project-details)
  "Evaluate all the functions in functions-list of a project with project-details as argument"
  (let* ((project-description (stufe-get-project-description project-name))
	 (project-funlist (stufe-project-get-fun-list project-description)))
  (if (stufe-project-need-details project-description)
      (mapcar (lambda (function) (funcall function project-details)) project-funlist)
    (mapcar (lambda (function) (funcall function)) project-funlist))))


;; *************************************************
;; * 
;; * Functions to use on template
;; *
;; *************************************************

(defun stufe-project-template-to-file (project-details
                                         template-name
                                         filename  
					 &optional 
					 view-file
					 no-save
					 indent)
  (let ((filepath (expand-file-name filename
				    (stufe-project-get-location project-details))))
  (stufe-template-args-into-file template-name
                                   (list (stufe-project-get-name project-details))
                                   filepath
				   view-file
				   no-save
				   indent)))

;; *************************************************
;; * 
;; * Functions to access data from a project 
;; * description
;; *
;; *************************************************

(defun stufe-project-get-name (project-description)
  "Return the name of a project from its description"
  (car project-description))


(defun stufe-project-get-fun-list (project-description)
  "Return the name of a project from its description"
  (car (cdr project-description)))


(defun stufe-project-get-options (project-description)
  "Return the name of a project from its description"
  (car (cdr (cdr project-description))))


;; *************************************************
;; * 
;; * Functions to deal with options of project
;; *
;; *************************************************

(defun stufe-project-need-details (project-description)
  (if (member 'no-project-details 
	      (stufe-project-get-options project-description))
      '()
    't))


;; *************************************************
;; * 
;; * Functions to access data from project details
;; *
;; *************************************************

(defun stufe-project-get-name (project-details)
  (car project-details))

(defun stufe-project-get-location (project-details)
  (cdr project-details))

;; *************************************************
;; * 
;; * Function to create a project detail when
;; * stufe doesn't have data about it
;; *
;; *************************************************

(defun stufe-create-project-details-from-folder (folder)
  (let ((current-folder (directory-file-name folder)))
    (cons (file-name-sans-versions 
	   (file-name-nondirectory current-folder))
	  current-folder)))


(defun stufe-create-project-details-from-working-folder ()
  (stufe-create-project-details-from-folder
   (if stufe-working-folder
       stufe-working-folder
     (file-name-directory (buffer-file-name)))))


;; *************************************************
;; * 
;; * Functions to define and create a folder
;; * from the name of the project
;; *
;; *************************************************

(defun stufe-project-get-name-location ()
  "Get a location in the hierarchy from a name for the project"
  (let* ((project-name (read-string "Project name: "))
	 (project-location (stufe-choose-new-folder "Project location: "
						      (expand-file-name project-name 
									stufe-projects-default-path))))
   (cons project-name project-location)))


(defun stufe-projects-new-folder (project-details)
  (let ((location (stufe-project-get-location project-details)))
    (if (not (file-exists-p location))
	(make-directory location))))


;; *************************************************
;; * 
;; * Define a working folder for every project in
;; * Emacs
;; *
;; *************************************************

(defun stufe-set-working-folder (working-folder)
  "Set a working folder for projects in Emacs"
  (setq stufe-working-folder
	  (if (string= "" working-folder)
	      nil
	    (if (not (file-directory-p working-folder))
		(if (file-exists-p working-folder)
		    (file-name-directory working-folder)
		  (error "%s: no such directory" working-folder))
	      (if (file-executable-p working-folder)
		  working-folder
		(error "Cannot cd to %s:  Permission denied" working-folder))))))


(defun stufe-interactive-set-working-folder ()
  (interactive)
  (stufe-set-working-folder
   (stufe-choose-new-folder "Set a working folder: "
			      (if stufe-working-folder
				  stufe-working-folder
				(file-name-directory (buffer-file-name))))))


(stufe-add-menu-item (stufe-get-stufe-context)
		       "Set working folder..." 
		       'stufe-interactive-set-working-folder)

(global-set-key [(control shift r)] 'stufe-interactive-set-working-folder)


;; *************************************************
;; * 
;; * Create the project item in the stufe menu
;; *
;; *************************************************

(defvar stufe-projects-menu-context
  nil
  "Context of the projects menu")

(setq stufe-menu-projects-context
      (stufe-add-menu-item-group "Projects"))









