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
;; * Functions to extract data from the description
;; * of an argument
;; *
;; *************************************************

(defun stufe-argument-get-type (argument-description)
  "Return the type of an argument from its declaration"
  (car argument-description))

(defun stufe-argument-get-name (argument-description)
  "Return the name of an argument from its declaration"
  (cadr argument-description))

(defun stufe-argument-get-default (argument-description)
  "Return the default value of an argument from its declaration"
  (let ((list-element (cddr argument-description)))
    (if list-element
	(car list-element)
      'nil)))

(defun stufe-argument-make-description (type-value name-value default-value)
  (list type-value name-value default-value))
					 

;; *************************************************
;; * 
;; * Functions to manipulate arguments in function
;; * declaration
;; *
;; *************************************************

(defun stufe-extract-argument-description (argument-declaration)
  "Return an argument description from its declaration"
  (let* ((string-split (split-string argument-declaration "="))
	 (default-value (if (cdr string-split)
			    (cadr string-split)
			  'nil))
	 (name-value (car (last (split-string (car string-split) " ") 1)))
	 (type-value (stufe-rebuild-string 
		      (butlast (split-string (car string-split) " ") 1)
		      " ")))
    (stufe-argument-make-description type-value name-value default-value)))


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
  (stufe-rebuild-string 
   (mapcar (lambda (argument-description)
	     (format "%s %s" 
		     (stufe-argument-get-type argument-description)
		     (stufe-argument-get-name argument-description)))
	   (stufe-extract-arguments-description function-declaration))
   ", "))

   
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
  "Return the name of the function"
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
  (cadr (split-string (car (split-string function-declaration 
					 ";"))
		      ")")))


(defun stufe-is-function-purely-virtual (function-declaration)
  "Test if a function is purely virtual from its declaration"
  (let ((modifiers-string (stufe-build-modifiers-string function-declaration)))
    (if modifiers-string
	(member "0"
		(split-string (stufe-rebuild-string (split-string modifiers-string "=") 
						    " ")
			      " "))
      nil)))
	


;; *************************************************
;; * 
;; * Functions to create a new member of a class
;; *
;; *************************************************

;; (defun stufe-get-cpp-class-name ()
;;   (save-excursion 
;;     (if (search-forward "//) " (point-max) 't)
;; 	(thing-at-point 'word)
;;       "")))
;; I don't understand everything in the code of the following function
;; but it seems to do the job : I found it at:
;; http://www.theprogrammerstoolbox.com/emacsinit.html
;; so, thanks to David Gallucci
;; The good thing about it is it avoid to have //( tag
(defun stufe-get-cpp-class-name ()
  "If the point is in a class definition, gets the name of the class.  Return
nil otherwise."
  (save-excursion
    (let ((brace (assoc 'inclass (c-guess-basic-syntax))))
      (if (null brace) '()
        (goto-char (cdr brace))
        (let ((class-open (assoc 'class-open (c-guess-basic-syntax))))
          (if class-open (goto-char (cdr class-open)))
          (if (looking-at "^class[ \t]+\\([A-Za-z_][^ \t:{]*\\)")
              (buffer-substring (match-beginning 1) (match-end 1))
            (error "Error parsing class definition!")))))))


(defun stufe-is-a-function-declaration (member-declaration)
  "Test if a declaration is a function or not"
  (and (string-match  "(" member-declaration)
       (string-match  ")" member-declaration)))


(defun stufe-exist-body-file (header-file)
  (let* ((file-extension (file-name-extension header-file))
	 (file-name (file-name-sans-extension
		     (file-name-nondirectory header-file)))
	 (search-body-file (lambda (extension)
			     (or (file-exists-p 
				  (concat file-name extension))
				 (get-file-buffer
				  (concat file-name extension))))))
    (cond 
     ((string= file-extension "hpp") (funcall search-body-file ".cpp"))
     ((string= file-extension "h") (or (funcall search-body-file ".c")
				       (funcall search-body-file ".cpp"))))))


(defun stufe-goto-print-area (searched-element)
  (goto-char (point-max))
  (let ((case-fold-search-old case-fold-search))
    (setq case-fold-search nil)
    (if (search-backward (format "//) %s" searched-element) 0 't)
	(backward-char 1)
      (goto-char (point-max)))
    (setq case-fold-search case-fold-search-old)))
  

(defun stufe-create-new-cpp-function (function-declaration)
  "Create a new cpp function in C/CPP code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "cpp-class-member-declaration"
				 (list function-declaration
				       (stufe-make-arguments-documentation
					function-declaration)
				       (stufe-make-return-value-documentation
					function-declaration)))
   't)
  (if (and (stufe-exist-body-file (buffer-file-name))
	   (not (stufe-is-function-purely-virtual function-declaration)))
      (save-current-buffer 
	(let* ((class-name (stufe-get-cpp-class-name)))
	  (stufe-switch-cpp-file)
	  (stufe-goto-print-area class-name)
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

(defun stufe-create-new-cpp-property (property-declaration)
  "Create a new property in C++ mode"
  (interactive "*sProperty declaration: ")
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "cpp-property"
				 (list (stufe-apply-args-on-template "cpp-class-member-declaration"
								     (list property-declaration))
				       (stufe-java-get-variable-name property-declaration)
				       (stufe-java-get-variable-type property-declaration)
				       (stufe-java-get-standard-variable-name property-declaration)))
   'indent))

(defun stufe-create-new-cpp-variable (variable-declaration)
  "Create a new cpp variable in CPP code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "cpp-class-member-declaration"
				   (list variable-declaration))
   'indent))
 

(defun stufe-create-new-cpp-class-member (member-declaration)
  "Create a new variable/function in CPP code"
  (interactive "*sMember declaration: ")
  (if (or (string= (file-name-extension (buffer-name)) "h")
	  (string= (file-name-extension (buffer-name)) "hpp"))
      (if (stufe-is-a-function-declaration member-declaration)
	  (stufe-create-new-cpp-function member-declaration)
	(stufe-create-new-cpp-variable member-declaration))
    (if (stufe-is-a-function-declaration member-declaration)
	(stufe-create-new-intern-c-function member-declaration)
      (stufe-create-new-intern-c-variable member-declaration))))
  

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
;; * Functions to switch between cpp<-->hpp
;; *
;; *************************************************
(defun stufe-switch-cpp-file ()
  "Switch between c/cpp<->h/hpp"
  (interactive)
  (let ((file-extension (file-name-extension (buffer-file-name)))
	(file-name (file-name-sans-extension
		    (file-name-nondirectory (buffer-file-name))))
	(file-present (lambda (filename)
			(or (file-exists-p filename)
			    (get-file-buffer filename)))))

    (cond ((string= file-extension "cpp") 
	   (cond ((funcall file-present (concat file-name ".hpp"))
		  (find-file (concat file-name ".hpp")))
		 ((funcall file-present (concat file-name ".h"))
		  (find-file (concat file-name ".h")))
		 ('t (find-file (concat file-name ".hpp")))))

	  ((string= file-extension "hpp") (find-file (concat file-name ".cpp")))

	  ((string= file-extension "cxx") (find-file (concat file-name ".hxx")))
	  ((string= file-extension "hxx") (find-file (concat file-name ".cxx")))

	  ((string= file-extension "h") 
	   (cond ((funcall file-present (concat file-name ".c"))
		   (find-file (concat file-name ".c")))
		  ((funcall file-present (concat file-name ".cpp"))
		   (find-file (concat file-name ".cpp")))
		  ('t (find-file (concat file-name ".c")))))
		  
	  ((string= file-extension "c") (find-file (concat file-name ".h"))))))


(defun stufe-switch-cpp-satom-file ()
  "Switch between normal class and _st file"
  (interactive)
  (let* ((file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
	 (file-directory (file-name-directory (buffer-file-name)))
	 (file-target (if (string= (substring file-name 0 3) "_st")
			  (concat file-directory 
				  (substring file-name 3) 
				  (if (string= (file-name-extension (buffer-file-name)) "cxx")
				      ".cpp"
				    ".hpp"))
			(concat file-directory 
				(concat "_st" 
					file-name 
					(if (string= (file-name-extension (buffer-file-name)) "cpp")
					    ".cxx"
					  ".hxx"))))))
  (if (or (file-exists-p file-target) (get-file-buffer file-target))
      (find-file file-target))))
