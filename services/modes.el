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

(stufe-require-file "services/register.el")

;; *************************************************
;; * 
;; * Function to associate a mode with a hook
;; *
;; *************************************************
(defmacro stufe-associate-mode-with-hook (emacs-hook
					    stufe-mode)
  "Associate a stufe mode with a hook"
  `(add-hook ,emacs-hook
	     '(lambda ()
		(stufe-menu-create-local "Project")
		(stufe-mode-eval ,stufe-mode))))

;; *************************************************
;; * 
;; * Function to evaluate a mode
;; *
;; *************************************************

(defun stufe-mode-eval (mode-name)
  "Evaluate all the functions in functions-list of a mode"
  (mapcar (lambda (function)
	    (if (stringp function)
		(stufe-mode-eval function)
	      (funcall function)))
	  (stufe-mode-get-fun-list (stufe-get-mode-description 
				      mode-name))))



;; *************************************************
;; * 
;; * Function to associate a list of actions to
;; * an emacs mode
;; *
;; *************************************************


(defvar stufe-mode-list 
  (stufe-register-new "modes")
  "List of modes available in Stufe")

(defun stufe-get-mode-description (mode-name)
  (stufe-register-get-description mode-name stufe-mode-list))


(defun stufe-register-mode (mode-description)
  (setq stufe-mode-list
	(stufe-register-add-data mode-description
				 stufe-mode-list)))

(defun stufe-copy-mode (mode-origin mode-destination)
  (stufe-register-mode 
   (list mode-destination
	 (stufe-mode-get-fun-list 
	  (stufe-get-mode-description mode-origin)))))


;; *************************************************
;; * 
;; * Functions to access data from a mode 
;; * description
;; *
;; *************************************************

(defun stufe-mode-get-name (mode-description)
  "Return the name of a mode from its description"
  (car mode-description))


(defun stufe-mode-get-fun-list (mode-description)
  "Return the name of a mode from its description"
  (car (cdr mode-description)))










