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

(defun stufe-current-makefile-path ()
  (let ((current-filename (buffer-file-name)))
    (if current-filename 
	(expand-file-name "makefile" 
		(file-name-directory current-filename))
      nil)))


(defun stufe-project-makefile-path ()
  (if stufe-working-folder
      (expand-file-name "makefile" 
			(file-name-directory stufe-working-folder))
    (stufe-current-makefile-path)))


(defun stufe-makefile-get-value (makefile key)
  (car (stufe-makefile-get-values makefile key)))

(defun stufe-makefile-get-values (makefile key)
  "Get the corresponding value in a makefile for a specified key"
  (cdr (split-string (stufe-makefile-get-line-value makefile key)
		     "[= \f\t\n\r\v]+")))

(defun stufe-makefile-get-line-value (makefile key)
  "Get the corresponding line of a value in a makefile for a specified key"
  (let* ((initial-buffer (get-file-buffer makefile))
	 (makefile-buffer (if (not initial-buffer)
			      (find-file makefile)
			    initial-buffer))
	 (current-string (concat "#" key)))
    (save-current-buffer
      (set-buffer makefile-buffer)
      (goto-char 0)
      (while (> (string-match key current-string)
		(let ((position (string-match "#" current-string)))
		  (if position
		      position
		    (length current-string))))
	(search-forward key)
	(setq current-string (thing-at-point 'line)))
      (if (not initial-buffer)
	  (kill-buffer makefile-buffer))
      (set-text-properties 0 
			   (length current-string)
			   nil
			   current-string)
      current-string)))
      
