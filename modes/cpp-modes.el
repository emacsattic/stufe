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
;; * Functions to specify the cpp mode
;; *
;; *************************************************

(stufe-require-file "services/register.el")

(defun stufe-run-debugger-c-mode (debug-command)
  (setq stufe-debug-buffer-name "*gud*")
  (format "%s %s"
	  debug-command 
	  (if stufe-working-folder
	      (format "--cd=%s" stufe-working-folder)
	    "")))


(setq stufe-c-debug-command-table
      '(("Debug project" (lambda ()
			   (let* ((debugfolder (stufe-makefile-get-value 
						(stufe-project-makefile-path) 
						"DEBUGFOLDER"))
				  (folder (if (not (string= debugfolder ""))
					      debugfolder
					    (stufe-makefile-get-value 
					     (stufe-project-makefile-path)
					     "PROJECTPATH"))))
			     (gud-call (format "cd %s" folder)))
			   (let* ((debugtarget (stufe-makefile-get-value 
						(stufe-project-makefile-path) 
						"DEBUGTARGET"))
				  (target (if (not (string= debugtarget ""))
					      debugtarget
					    (stufe-makefile-get-value 
					     (stufe-project-makefile-path)
					     "PROJECT"))))
			     (gud-call (format "file %s" target)))
			   (let ((options (stufe-makefile-get-value 
					   (stufe-project-makefile-path) 
					   "OPTION")))
			     (if options
				 (gud-call (format "set args %s" options))))
			   (if stufe-gdb-tty
			       (gud-call (format "tty %s" stufe-gdb-tty)))
			   (mapcar (lambda (breakpoint) 
				     (stufe-debug-add-breakpoint breakpoint))
				   stufe-breakpoint-list)
			   (gud-call "run")))

	("Add breakpoint" (lambda (file line)
			    (gud-call (format "break %s:%s" 
					      (file-name-nondirectory file) 
					      line))))

	("Clear breakpoint" (lambda (file line)
			      (gud-call (format "clear %s:%s" 
						(file-name-nondirectory file)
						line))))
	
	("Print variable" (lambda (variable) 
			    (gud-call (format "print %s" variable))))))


(stufe-register-mode '("c-prog-mode"
		       (
			;; Menu item
			"debug-mode" 

			(lambda ()
			  (stufe-menu-add-menubar-local))

			(lambda ()
			  (stufe-menu-add-item-local "Exec" 
						     'stufe-make-exec))

			"prog-mode" 
			  
			(lambda ()
			  (stufe-menu-add-menubar-local))
			  			  
			(lambda ()
			  (stufe-menu-add-item-local "Generate documentation" 
						     'stufe-make-doc))

			;; Keyboard shortcut
			(lambda ()
			  (stufe-shortcut-add-local [(f5)] 
						    `stufe-make-exec))
			(lambda ()
			  (stufe-shortcut-add-local [(control $)] 
						    'stufe-switch-cpp-file))
			(lambda ()
			  (stufe-shortcut-add-local [(control 0)] 
						    'stufe-switch-cpp-file))

			;; Defines local buffer variables
			(lambda ()
			  ;; Indentation
			  (setq indent-tabs-mode nil)
			  ;; Man pages
			  (make-local-variable 'stufe-default-man-page)
			  (setq stufe-default-man-page "2")
			  ;; Grep patterns
			  (make-local-variable 'stufe-grep-file-pattern)
			  (setq stufe-grep-file-pattern "*.[cChH] *.[ch]pp"))

			;; Initialize debugging
			(lambda ()
			  (make-local-variable 'stufe-debug-buffer-name)
			  (setq stufe-debug-buffer-name "*gud*")

			  (make-local-variable 'stufe-debug-command)
			  (setq stufe-debug-command "gdb")

			  (make-local-variable 'stufe-debug-command)
			  (setq stufe-debug-function 'gdb)

			  (make-local-variable 'stufe-run-debugger)
			  (setq stufe-run-debugger 'stufe-run-debugger-c-mode)

			  (make-local-variable 'stufe-debug-command-table)
			  (setq stufe-debug-command-table stufe-c-debug-command-table)))))

(stufe-register-mode '("c-mode"
			 ( 
			   ;; Menu item
			  (lambda ()
			    (stufe-menu-add-item-local "Create a new module..." 
							 'stufe-create-new-c-module))
			  (lambda ()
			    (stufe-menu-add-item-local "Create a new function..." 
							 'stufe-create-new-c-member))
			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  "c-prog-mode" 

			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  (lambda ()
			    (stufe-menu-add-item-local "Switch c<-->h" 
							 'stufe-switch-cpp-file))
			   
			  ;; Keyboard shortcut
			  (lambda ()
			    (stufe-shortcut-add-local [(control ?c) (control ?o)] 
							'stufe-create-new-c-module))
			  (lambda ()
			    (stufe-shortcut-add-local [(control ?c) (control ?f)] 
							'stufe-create-new-c-member))
			  )))


(stufe-register-mode '("cpp-mode"
			 ( 

			  (lambda ()
			    (stufe-menu-add-item-local "Create a new class..." 
							 'stufe-create-new-cpp-class))
			  (lambda ()
			    (stufe-menu-add-item-local "Create a new member..." 
							 'stufe-create-new-cpp-class-member))
			   (lambda ()
			     (stufe-menu-add-item-local "Create a new property..." 
							  'stufe-create-new-cpp-property))
			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  "c-prog-mode" 
			   
			   ;; Menu item
			  
			  (lambda ()
			    (stufe-menu-add-menubar-local))
			  (lambda ()
			    (stufe-menu-add-item-local "Switch cpp<-->hpp" 
							 'stufe-switch-cpp-file))

			  ;; Keyboard shortcut
			  (lambda ()
			    (stufe-shortcut-add-local [(control ?c) (control ?o)] 
							'stufe-create-new-cpp-class))
			  (lambda ()
			    (stufe-shortcut-add-local [(control ?c) (control ?f)] 
							'stufe-create-new-cpp-class-member))
			  (lambda ()
			    (stufe-shortcut-add-local [(control ?c) (control ?p)] 
						      'stufe-create-new-cpp-property))
			  (lambda ()
			    (stufe-shortcut-add-local [(control *)] 
							'stufe-switch-cpp-satom-file))
			  )))


;; Add the hook to list of the things to do when opening a C++ file
(stufe-associate-mode-with-hook 'c++-mode-hook "cpp-mode")
(stufe-associate-mode-with-hook 'c-mode-hook "c-mode")


