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


(stufe-require-file "services/menu.el")


;; *************************************************
;; * 
;; * Functions to run the make command
;; *
;; *************************************************

(defun stufe-make-exec ()
  (interactive)
  (stufe-run-make "tests"))


(defun stufe-make-doc ()
  (interactive)
  (stufe-run-make "doc"))


(defun stufe-make-build ()
  (interactive)
  (stufe-run-make "all"))


(defun stufe-make-clean ()
  (interactive)
  (stufe-run-make "clean"))


(defun stufe-make-rebuild ()
  (interactive)
  (stufe-run-make "-k clean all"))


(setq stufe-compile-function 
      '(lambda (&optional command)
	 (interactive)
	 (save-some-buffers 1)
	 (if command
	     (compile command)
	   (if (stufe-project-makefile-path)
	       (stufe-run-make "")
	     (call-interactively 'compile)))
	 (if (stufe-get-compilation-window)
	     (save-selected-window
	       (select-window (stufe-get-compilation-window))
	       (goto-char (point-max))))))


(defun stufe-is-make-runnable ()
  "Test if the command make is runnable in the current state of the
project"
  (or (stufe-project-file-existp "Makefile")
      (stufe-project-file-existp "makefile")))


(defun stufe-is-configurable ()
  "Test if the project is configurable in the current state of the
project"
  (stufe-project-file-existp "configure"))


(defun stufe-is-bootstrapable ()
  "Test if the project can generate configure file in its current state"
  (stufe-project-file-existp "bootstrap"))


;; (defun stufe-run-make (command)
;;   "Run the make command if a makefile is found"
;;   (interactive)
;;   (if (and (not stufe-working-folder)
;; 	   (not (or (file-exists-p "makefile")
;; 		    (file-exists-p "Makefile"))))
;;       (stufe-guess-project-makefile))
;;   (funcall stufe-compile-function 
;; 	   (stufe-run-in-work-folder stufe-make-command command)))

(defun stufe-run-make (command)
  "Try to run the make command (build a makefile if possible or necessary)"
  (interactive)
  (let ((make-command stufe-make-command)
	(bootstrap-command (if (and (not (stufe-is-configurable)) 
				    (stufe-is-bootstrapable))
			       "sh ./bootstrap && "
			     ""))
	(configurable-command (if (and (not (stufe-is-make-runnable))
				       (or (stufe-is-bootstrapable)
					   (stufe-is-configurable)))
				  "./configure --enable-debug && "
				"")))
    (if (not (or (stufe-is-make-runnable)
		 (stufe-is-configurable)
		 (stufe-is-bootstrapable)))
	(stufe-guess-project-makefile))
    (funcall stufe-compile-function 
 	     (stufe-run-in-work-folder (format "%s%s%s" 
					       bootstrap-command 
					       configurable-command
					       stufe-make-command) command))))



(defun stufe-run-in-work-folder (command argument)
  (format "%s%s %s " 
	  (if stufe-working-folder
	      (format "cd %s; " stufe-working-folder)
	    "")
	  command
	  argument))


(defun stufe-guess-project-makefile ()
  (cond
   ((and stufe-working-folder (not (string= "" stufe-working-folder)))
    (error "Stufe does not create makefiles when the project path is set" major-mode))
   ;; Default makefile for LaTeX mode
   ((eq major-mode 'latex-mode) 
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder
      (file-name-nondirectory
       (file-name-sans-versions
	(file-name-sans-extension (buffer-file-name)))))
     "latex-makefile" 
     "makefile"))
   ;; Default makefile for Java mode
   ((eq major-mode 'java-mode)
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder 
      (file-name-nondirectory
       (file-name-sans-versions
	(file-name-sans-extension (buffer-file-name)))))     
     "java-makefile" 
     "makefile"))
   ;; Default makefile for C++ mode
   ((or (eq major-mode 'c++-mode)
	(eq major-mode 'c-mode))
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder 
      (file-name-nondirectory
       (file-name-sans-versions
	(file-name-sans-extension (buffer-file-name)))))     
     "cpp-makefile" 
     "makefile"))
   (t (error "No makefile associated with %s" major-mode))))



;; *************************************************
;; * 
;; * Functions to get some information about
;; * the current compilation window
;; *
;; *************************************************

(setq stufe-compilation-buffer-name "*compilation*")

(defun stufe-get-compilation-buffer ()
  "Return the buffer used to compile"
  (get-buffer stufe-compilation-buffer-name))

(defun stufe-get-compilation-process ()
  "Return the process used to compile"
  (get-buffer-process (stufe-get-compilation-buffer)))

(defun stufe-get-compilation-window ()
  "Get the current window used to compile"
  (if (stufe-get-compilation-buffer)
      (get-buffer-window (stufe-get-compilation-buffer))))


;; *************************************************
;; * 
;; * Bindings
;; *
;; *************************************************

(global-set-key [(f5)] stufe-compile-function)

(stufe-add-menu-item nil
		     "Run command..." 
		     stufe-compile-function)
