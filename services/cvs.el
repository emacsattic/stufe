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


(stufe-load-file "menu.el")



;; *************************************************
;; * 
;; * Function to access easily to a cvs repository
;; *
;; *************************************************


(defun stufe-cvs-commit (description)
  "Run the 'commit' command of cvs for the current folder"
  (interactive "*sLog description: ")
  (stufe-cvs-run-cvs (format "commit%s"
				 (progn
				   (stufe-cvs-update)
				   (format " -m \"%s\" " description)))))


(defun stufe-cvs-tag (description)
  "Run the 'tag' command of cvs for the current folder"
  (interactive "*sTag: ")
  (stufe-cvs-run-cvs (format "tag -c %s" description)))


(defun stufe-cvs-add-buffer ()
  "Add the current buffer to cvs"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
	(stufe-cvs-run-cvs (format "add %s"
				     (file-name-nondirectory filename))
			     (file-name-directory filename))
      (error "This buffer doesn't visit any file"))))


(defun stufe-cvs-update ()
  "Run the 'update' command for the current folder and update buffers in emacs"
  (interactive)
  (stufe-cvs-run-cvs "update -d")
  (update-all-buffers))


(defun stufe-cvs-run-cvs (options &optional working-folder)
  "Run a cvs command with the argument options as options"
  (save-some-buffers 1)
  (shell-command (format "%scvs %s"
			 (if working-folder 
			     (format "cd %s; " working-folder)
			   (if stufe-working-folder
			       (format "cd %s; " stufe-working-folder)
			     ""))
			 options)))









