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
;; * Functions to deal with Java variable
;; *
;; *************************************************


(defun stufe-java-get-variable-name (variable-declaration)
  "Return the name of a variable from its declaration"
  (car (last (split-string (car (split-string (car (split-string variable-declaration 
								 ";"))
					      "="))
			   " "))))


(defun stufe-java-get-variable-type (variable-declaration)
  "Return the name of a variable from its declaration"
  (let ((string-split (split-string (car (split-string (car (split-string variable-declaration 
									  ";"))
						       "="))
				    " ")))
    (nth (- (length string-split) 2) string-split)))


(defun stufe-java-get-standard-variable-name (variable-declaration)
  (upcase-initials 
   (stufe-java-get-variable-name variable-declaration)))


;; *************************************************
;; * 
;; * Functions for the Java mode
;; *
;; *************************************************


(defun stufe-create-new-java-class (classname)
  "Create a new java class"
  (interactive "*sClass name: ")
  (let ((filepath (expand-file-name classname
				    (file-name-directory buffer-file-name))))
    (stufe-template-args-into-file "java-class"
				     (list classname "")
				     (concat filepath ".java")
				     'view-file
				     'no-save
				     'indent)))
(defun stufe-create-new-java-interface (classname)
  "Create a new java interface"
  (interactive "*sInterface name: ")
  (let ((filepath (expand-file-name classname
				    (file-name-directory buffer-file-name))))
    (stufe-template-args-into-file "java-interface"
				   (list classname "")
				   (concat filepath ".java")
				   'view-file
				   'no-save
				   'indent)))

(defun stufe-get-java-class-name ()
  (file-name-sans-extension (file-name-nondirectory buffer-file-name)))


(defun stufe-create-new-java-function (function-declaration)
  "Create a new java function in Java mode"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "java-member-declaration"
				   (list function-declaration
					 (stufe-make-arguments-documentation
					  function-declaration)
					 (if (equal (stufe-get-java-class-name)
						    (stufe-build-function-name function-declaration))
					     ""
					   (stufe-make-return-value-documentation
					    function-declaration))))
   'indent))


(defun stufe-create-new-java-variable (variable-declaration)
  "Create a new java variable in Java mode"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "java-variable-declaration"
				   (list variable-declaration))
   'indent))


(defun stufe-create-new-java-bean-property (property-declaration)
  "Create a new bean property in Java mode"
  (interactive "*sProperty declaration: ")
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "java-bean-property"
				 (list (stufe-apply-args-on-template "java-variable-declaration"
								     (list property-declaration))
				       (stufe-java-get-variable-name property-declaration)
				       (stufe-java-get-variable-type property-declaration)
				       (stufe-java-get-standard-variable-name property-declaration)))
   'indent))


(defun stufe-create-new-java-class-member (member-declaration)
  "Create a new variable/function in Java mode"
  (interactive "*sMember declaration: ")
  (if (stufe-is-a-function-declaration member-declaration)
      (stufe-create-new-java-function member-declaration)
    (stufe-create-new-java-variable member-declaration)))


;; *************************************************
;; * 
;; * Functions to get the package name of a class
;; *
;; *************************************************


(defun stufe-java-get-class-identity (buffer)
  "Return the identifier of the class from a buffer"
  (let ((package-name-position (save-current-buffer 
			       (save-excursion
				 (set-buffer buffer)
				 (goto-char (point-min))
				 (search-forward "package" nil 't))))
	(class-name (file-name-nondirectory 
		    (file-name-sans-extension (buffer-file-name buffer)))))
    (if package-name-position
	(format "%s.%s"
		(let ((package-name (save-current-buffer 
				      (save-excursion
					(set-buffer buffer)
					(goto-char package-name-position)
					(buffer-substring (+ package-name-position 1)
							  (- (search-forward ";") 1))))))
		  (set-text-properties 1 (length package-name)
				       package-name
				       nil)
		  (car (split-string (car (split-string package-name " ")) "\n")))
		class-name)
      class-name)))

;; *************************************************
;; * 
;; * Function to run the current class
;; *
;; *************************************************

(defun stufe-java-compile-current ()
  (interactive)
  (let* ((classidentity (stufe-java-get-class-identity (current-buffer)))
	 (java "java")
	 (makefilepath (stufe-project-makefile-path))
	 (classpathmakefile (stufe-makefile-get-atomic-value
			     makefilepath "CLASSPATH"))
	 (javaoptions (format "-cp %s:%s" 
			      (if classpathmakefile
				  classpathmakefile
				"")
			      (stufe-makefile-get-atomic-value
			       makefilepath "PROJECTPATH")))
	 (options (stufe-makefile-get-value 
		   makefilepath
		   (format "%s_OPTION" classidentity))))
    (stufe-run-make (format "all && %s %s %s %s" java javaoptions classidentity options))))

