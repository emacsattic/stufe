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
;; * Grep functions for Stufe
;; *
;; *************************************************


(defvar stufe-grep-file-pattern
  "*"
  "Default pattern used for the grep command")


(defun stufe-grep-word-recursively (&optional directory expression)
  "Run grep to match the current word with a file pattern recursively"
  (interactive)
  (let ((filename (buffer-file-name))
	(expression-pattern (if expression
				(format "\"%s\"" expression)
			      (format "\"%s\"" (current-word)))))
    (stufe-grep-word-current 
     (format "$(find %s -name \"%s\")" 
	     (if directory
		 directory
	       (if filename
		   (file-name-directory filename)
		 "."))
	     (stufe-rebuild-string (split-string (if filename
						     stufe-grep-file-pattern
						   "*"))				 
				   "\" -or -name \""))
     expression-pattern)))


(defun stufe-grep-word-current (&optional file-pattern expression)
  "Run grep to match the current word with a file pattern"
  (interactive)
  (grep (format "%s -n %s %s" 
		grep-program 
		expression
		(if file-pattern
		    file-pattern
		    stufe-grep-file-pattern))))


(defun stufe-grep-word-in-directory (&optional expression)
  "Grep the current word in a defined folder"
  (interactive)
  (let ((filename (buffer-file-name))
	(expression-pattern (if expression
				expression
			      (current-word))))
    (stufe-grep-word-recursively (stufe-choose-new-folder 
				  (format "Starting grep of '%s' in folder: "
					  expression-pattern)
				  (if filename
				      (file-name-directory filename)
				    "."))
				 expression-pattern)))


(defun stufe-grep-expression-in-directory (expression)
  "Grep a specific expression in a defined folder"
  (interactive "sExpression to grep: ")
  (stufe-grep-word-in-directory expression))


;; Define global key to bind with these functions
(global-set-key [(f2)] 'stufe-grep-word-recursively)
(global-set-key [(shift f2)] 'stufe-grep-word-in-directory)
(global-set-key [(control f2)] 'stufe-grep-expression-in-directory)

;; Add the items in the stufe menu
(defvar stufe-menu-grep-context
  nil
  "Context of the grep menu")

(setq stufe-menu-grep-context
      (stufe-add-menu-item-group "Grep" nil "Projects"))

(stufe-add-menu-item stufe-menu-grep-context
		     "Grep word" 
		     'stufe-grep-word-recursively)

(stufe-add-menu-item stufe-menu-grep-context
		     "Grep word in folder..." 
		     'stufe-grep-word-in-directory)

(stufe-add-menu-item stufe-menu-grep-context
		     "Grep expression in folder..." 
		     'stufe-grep-expression-in-directory)















