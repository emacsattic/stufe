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
;; * Functions to be able to get data from a
;; * stufe makefile
;; *
;; *************************************************

(defun stufe-project-file-existp (filename)
  "Test if a file exists in the current folder or the project folder"
  (let ((folder (if stufe-working-folder
		    stufe-working-folder
		  "./")))
    (file-exists-p (expand-file-name filename folder))))


(defun stufe-find-makefile (folder)
  "Try to find a makefile in a folder"
  (cond 
   ((file-exists-p (expand-file-name "Makefile.am" folder))
    (expand-file-name "Makefile.am" folder))
   ((file-exists-p (expand-file-name "Makefile" folder))
    (expand-file-name "Makefile" folder))
   ((file-exists-p (expand-file-name "makefile" folder))
    (expand-file-name "makefile" folder))
   (t nil)))


(defun stufe-open-makefile ()
  "Open the makefile found in the current folder"
  (interactive)
  (let ((makefile-path (stufe-find-makefile "./")))
    (if makefile-path
	(find-file makefile-path)
      (progn 
	(stufe-guess-project-makefile)
	(find-file "makefile")))))


(defun stufe-current-makefile-path ()
  (let ((current-filename (buffer-file-name)))
    (if current-filename 
	(expand-file-name "makefile" 
		(file-name-directory current-filename))
      nil)))


(defun stufe-project-makefile-path ()
  "Try to find a makefile for the current project"
  (stufe-find-makefile (if stufe-working-folder
			   stufe-working-folder
			 "./")))


(defun stufe-makefile-get-atomic-value (makefile key)
  (car (split-string (stufe-makefile-get-value makefile key))))


(defun stufe-makefile-get-value (makefile key)
  (save-current-buffer
    (stufe-rebuild-string (stufe-makefile-get-values makefile key)
			  " ")))

(defun stufe-makefile-remove-variable-name (line-value &optional current-list equalsign-flag)
  (let* ((equalsign-index (if line-value
			      (string-match "=" (car line-value))))
	 (current-string (if line-value
			     (if equalsign-flag
				 (car line-value)
			       (if equalsign-index
				   (substring (car line-value)
					      (+ equalsign-index 1))
				 '())))))
    (if line-value
	(stufe-makefile-remove-variable-name (cdr line-value)
					     (if current-string
						 (push current-string current-list)
					       current-list)
					     (if current-string 1))
      (reverse current-list))))

(defun stufe-makefile-get-values (makefile key)
  "Get the corresponding value in a makefile for a specified key"
  (let ((makefile-line-value (stufe-makefile-get-line-value makefile 
							    (concat stufe-makefile-variable-prefix 
								    key))))
    (if makefile-line-value
	(stufe-makefile-remove-variable-name 
	 (split-string makefile-line-value "[ \f\t\n\r\v]+"))
      nil)))

(defun stufe-makefile-get-line-value (makefile key)
  "Get the corresponding line of a value in a makefile for a specified key"
  (message "Get value of %s" key)
  (let* ((initial-buffer (get-file-buffer makefile))
	 (makefile-buffer (if (not initial-buffer)
			      (find-file makefile)
			    initial-buffer))
	 (current-string (concat "#" key)))
    (save-current-buffer
      (set-buffer makefile-buffer)
      (goto-char 0)
      (while (and current-string 
		  (> (string-match key current-string)
		     (let ((position (string-match "#" current-string)))
		       (if position
			   position
			 (length current-string)))))
	(if (search-forward key (point-max) t)
	    (setq current-string (thing-at-point 'line))
	  (setq current-string nil)))
      (if (not initial-buffer)
	  (kill-buffer makefile-buffer))
      (if current-string
	  (set-text-properties 0 
			       (length current-string)
			       nil
			       current-string))
      current-string)))
      
