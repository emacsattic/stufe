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
;; * Functions to run the make command
;; *
;; *************************************************

(defun stufe-make-exec ()
  (interactive)
  (stufe-run-make "exec"))


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


(defun stufe-run-make (command)
  "Save all the buffers and run the make command"
  (interactive)
  (if (and (not stufe-working-folder)
	   (not (or (file-exists-p "makefile")
		    (file-exists-p "Makefile"))))
      (stufe-guess-project-makefile))
  (funcall stufe-compile-function 
	   (stufe-run-in-work-folder stufe-make-command command)))


(defun stufe-run-in-work-folder (command argument)
  (format "%s%s %s " 
	  (if stufe-working-folder
	      (format "cd %s; " stufe-working-folder)
	    "")
	  command
	  argument))

(defun stufe-guess-project-makefile ()
  (cond
   ;; Default makefile for LaTeX mode
   ((eq major-mode 'latex-mode) 
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder)
     "latex-makefile" 
     "makefile"))
   ;; Default makefile for C++ mode
   ((or (eq major-mode 'c++-mode)
	(eq major-mode 'c-mode))
    (stufe-project-template-to-file 
     (stufe-create-project-details-from-working-folder)
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

(stufe-add-menu-item (stufe-get-stufe-context)
		       "Run command..." 
		       stufe-compile-function)
