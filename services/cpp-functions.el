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
;; * Functions for the cpp mode
;; *
;; *************************************************


;; *************************************************
;; * 
;; * Functions to manipulate arguments in function
;; * declaration
;; *
;; *************************************************

(defun stufe-argument-get-name (argument-description)
  "Return the name of an argument from its declaration"
  (car argument-description))


(defun stufe-argument-get-type (argument-description)
  "Return the type of an argument from its declaration"
  (cdr argument-description))


(defun stufe-extract-argument-description (argument-declaration)
  "Return an argument description from its declaration"
  (let ((string-split (split-string argument-declaration " ")))
    (cons (car (last string-split))
	  (butlast string-split 1))))


(defun stufe-extract-arguments-description (function-declaration)
  "Extract a list of declaration of arguments from the declaration
 of a function"
  (mapcar 'stufe-extract-argument-description
	  (stufe-build-arguments-list function-declaration)))


(defun stufe-build-arguments-list (function-declaration)
  "Build a list of string of the list of arguments of the function"
  (split-string 
   (substring function-declaration
	      (+ (string-match  "(" function-declaration) 1)
	      (string-match  ")" function-declaration))
   ","))


(defun stufe-build-arguments-string (function-declaration)
  "Build a string of the set of arguments of the function"
  (stufe-rebuild-string (stufe-build-arguments-list function-declaration)
			  ","))

   
(defun stufe-make-arguments-documentation (function-declaration)
  "Generate the documentation of the arguments of a function
declaration"
  (stufe-rebuild-string
   (let* ((arguments-description-list 
	   (stufe-extract-arguments-description function-declaration))
	  (arguments-name-list (mapcar (lambda (argument-description)
					 (stufe-argument-get-name 
					  argument-description))
				       arguments-description-list))
	  (max-word-length (stufe-get-max-length arguments-name-list)))
	 (mapcar (lambda (argument-name)
		   (stufe-apply-args-on-template 
		    "cpp-argument-documentation"
		    (list (cons argument-name max-word-length))))
		 arguments-name-list))
   ""))

;; *************************************************
;; * 
;; * Functions to create the documentation for the 
;; * return of a function
;; *
;; *************************************************


(defun stufe-build-return-value-list (function-declaration)
  (delete "virtual" 
	  (delete "static"
		  (butlast (split-string (car (split-string function-declaration 
							    "(")) " ") 1))))

(defun stufe-build-return-value-string (function-declaration)
  "Build a string of the return definition of the function"
  (stufe-rebuild-string (stufe-build-return-value-list function-declaration) 
			" "))

(defun stufe-build-function-name (function-declaration)
  "Build a list of the return definition of the function"
  (car (last (split-string (car (split-string function-declaration "(")) " "))))
 

(defun stufe-make-return-value-documentation (function-declaration)
  (let ((return-value-list (stufe-build-return-value-list function-declaration)))
    (if return-value-list
	(stufe-apply-args-on-template "cpp-function-return-value-documentation"
					(list (if (member "void" return-value-list)
						  ""
						"...")))
      "")))

;; *************************************************
;; * 
;; * Functions to deals with modifiers of the 
;; * function like const or pure virtual 
;; *
;; *************************************************

(defun stufe-build-modifiers-string (function-declaration)
  (let ((modifiers (cadr (split-string function-declaration ")"))))
    (if modifiers
	(stufe-rebuild-string 
	 (delete "=" 
		 (delete "0"
			 (split-string modifiers " ")))
	 " ")
      "")))

;; *************************************************
;; * 
;; * Functions to create a new member of a class
;; *
;; *************************************************

(defun stufe-get-cpp-class-name ()
  (save-excursion 
    (search-forward "//) ")
    (thing-at-point 'word)))


(defun stufe-is-a-function-declaration (member-declaration)
  "Test if a declaration is a function or not"
  (and (string-match  "(" member-declaration)
       (string-match  ")" member-declaration)))


(defun stufe-create-new-cpp-function (function-declaration)
  "Create a new cpp function in C/CPP code"
  (if (string= (file-name-extension (buffer-name)) "hpp")
      (progn
	(stufe-string-to-current-buffer
	 (stufe-apply-args-on-template "cpp-class-member-declaration"
					 (list function-declaration
					       (stufe-make-arguments-documentation
						function-declaration)
					       (stufe-make-return-value-documentation
						function-declaration)))
	 't)
	(if (or (file-exists-p 
		 (concat (get-file-name (buffer-name)) ".cpp"))
		(get-buffer
		 (concat (get-file-name (buffer-name)) ".cpp")))
	    (save-current-buffer 
	      (let* ((class-name (stufe-get-cpp-class-name)))
		(stufe-switch-cpp-file)
		(goto-char (point-max))
		(let ((case-fold-search-old case-fold-search))
		  (setq case-fold-search nil)
		  (search-backward (format "//) %s" class-name))
		  (setq case-fold-search case-fold-search-old))
		(backward-char 1)
		(stufe-string-to-current-buffer
		 (stufe-apply-args-on-template "function-body-declaration"
						 (list 
						  (stufe-build-return-value-string 
						   function-declaration)
						  class-name
						  (stufe-build-function-name 
						   function-declaration)
						  (stufe-build-arguments-string
						   function-declaration)
						  (stufe-build-modifiers-string
						   function-declaration)))
		 't)
		(stufe-switch-cpp-file)))))
	(error "You can only create a function in a hpp file")))


(defun stufe-create-new-cpp-variable (variable-declaration)
  "Create a new cpp variable in C/CPP code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "cpp-class-member-declaration"
				   (list variable-declaration))
   'indent))
 

(defun stufe-create-new-cpp-class-member (member-declaration)
  "Create a new variable/function in C/CPP code"
  (interactive "*sMember declaration: ")
  (if (stufe-is-a-function-declaration member-declaration)
      (stufe-create-new-cpp-function member-declaration)
    (stufe-create-new-cpp-variable member-declaration)))


;; *************************************************
;; * 
;; * Functions to create a new class
;; *
;; *************************************************
(defun stufe-create-new-cpp-class (classname)
  "Create a new CPP class"
  (interactive "*sClass name: ")
  (let ((filepath (expand-file-name (downcase classname)
				    (file-name-directory buffer-file-name))))
    (stufe-template-args-into-file "cpp-class-cpp"
				     (list classname "")
				     (concat filepath ".cpp")
				     'view-file
				     'no-save
				     'indent)
    (stufe-template-args-into-file "cpp-class-hpp"
				     (list classname "")
				     (concat filepath ".hpp")
				     'view-file
				     'no-save
				     'indent)))


;; *************************************************
;; * 
;; * Functions to create a new c gnu module
;; *
;; *************************************************
(defun stufe-create-new-c-module (modulename)
  "Create a new C module with a GNU headers"
  (interactive "*sModule name: ")
  (let ((filepath (expand-file-name (downcase modulename)
				    (file-name-directory buffer-file-name))))
    (stufe-template-args-into-file "c-module-c"
				     (list modulename "")
				     (concat filepath ".c")
				     'view-file
				     'no-save
				     'indent)
    (stufe-template-args-into-file "c-module-h"
				     (list modulename "")
				     (concat filepath ".h")
				     'view-file
				     'no-save
				     'indent)))


;; *************************************************
;; * 
;; * Functions to switch between cpp<-->hpp
;; *
;; *************************************************
(defun stufe-switch-cpp-file ()
  "Switch between c/cpp<->h/hpp"
  (interactive)
  (let ((file-extension (get-file-extension (buffer-name)))
	(file-name (get-file-name (buffer-name)))
	(file-present (lambda (filename)
			(or (file-exists-p filename)
			    (get-file-buffer filename)))))

    (cond ((string= file-extension ".cpp") 
	   (cond ((file-present (concat file-name ".hpp"))
		  (find-file (concat file-name ".hpp")))
		 ((file-present (concat file-name ".h"))
		  (find-file (concat file-name ".h")))
		 ('t (find-file (concat file-name ".hpp")))))

	  ((string= file-extension ".hpp") (find-file (concat file-name ".cpp")))

	  ((string= file-extension ".h") 
	   (cond ((file-present (concat file-name ".c"))
		   (find-file (concat file-name ".c")))
		  ((file-present (concat file-name ".cpp"))
		   (find-file (concat file-name ".cpp")))
		  ('t (find-file (concat file-name ".c")))))
		  
	  ((string= file-extension ".c") (find-file (concat file-name ".h"))))))

    


