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
;; * Functions to specify the Java mode
;; *
;; *************************************************


(defun stufe-run-debugger-java-mode (debug-command)
  (let ((debug-command (format "%s%s %s %s"
			       (if stufe-working-folder
				   (format "cd %s && " stufe-working-folder)
				 "")
			       debug-command
			       (format "-classpath=%s:%s"
				       (getenv "CLASSPATH")
				       (stufe-makefile-get-value (stufe-project-makefile-path)
								 "PROJECTPATH"))
			       (stufe-makefile-get-value (stufe-project-makefile-path)
							 "PROJECT"))))
    (message debug-command)
    debug-command))



(setq stufe-java-debug-command-table
      '(("Debug project" (lambda ()
			  (mapcar (lambda (breakpoint) 
				    (stufe-debug-add-breakpoint breakpoint))
				  stufe-breakpoint-list)
			  (gud-call "run")))
	
	("Add breakpoint" (lambda (file line)
			    (gud-call (format "stop at %s:%s" 
					      (file-name-nondirectory 
					       (file-name-sans-extension file))
					      line))))
	
	("Clear breakpoint" (lambda (file line)
			      (gud-call "clear " 
					(format "%s:%s"
						(file-name-nondirectory 
						 (file-name-sans-extension file))
						line))))

	("Print variable" (lambda (variable) (gud-call (format "print %s" variable))))))



(stufe-register-mode '("java-mode"
			 ( "prog-mode"

			  ;; Defines local buffer variables
			  (lambda ()
			    ;; Grep patterns
			    (make-local-variable 'stufe-grep-pattern)
			    (setq stufe-grep-pattern "*.java"))

			   ;; Menu item
			   (lambda ()
			     (stufe-menu-add-item-local "Exec" 
							  'stufe-make-exec))

			   "debug-mode" 
			   
			   ;; Menu item
			   (lambda ()
			     (stufe-menu-add-menubar-local))
			   (lambda ()
			     (stufe-menu-add-item-local "Create a new class..." 
							  'stufe-create-new-java-class))
			   (lambda ()
			     (stufe-menu-add-item-local "Create a new member..." 
							  'stufe-create-new-java-class-member))

			   (lambda ()
			     (stufe-menu-add-item-local "Create a new bean property..." 
							  'stufe-create-new-java-bean-property))
			   
			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(control ?c) (control ?o)] 
							 'stufe-create-new-java-class))
			   (lambda ()
			     (stufe-shortcut-add-local [(control ?c) (control ?f)] 
							 'stufe-create-new-java-class-member))

			   (lambda ()
			     (stufe-shortcut-add-local [(control ?c) (control ?p)] 
							 'stufe-create-new-java-bean-property))
			   
			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(f5)] 
							 `stufe-make-exec))

			   ;; Initialize debugging
			   (lambda ()
			     (make-local-variable 'stufe-debug-buffer-name)
			     (setq stufe-debug-buffer-name 
				   (format "*gud-%s*" 
					   (stufe-makefile-get-value 
					    (stufe-project-makefile-path) "PROJECT")))
			     
			     (make-local-variable 'stufe-debug-command)
			     (setq stufe-debug-command "jdb")

			     (make-local-variable 'stufe-debug-command)
			     (setq stufe-debug-function 'jdb)

			     (make-local-variable 'stufe-run-debugger)
			     (setq stufe-run-debugger 'stufe-run-debugger-java-mode)

			     (make-local-variable 'stufe-debug-command-table)
			     (setq stufe-debug-command-table stufe-java-debug-command-table)) )))


;; Add the hook to list of the things to do when opening a Java file
(stufe-associate-mode-with-hook 'java-mode-hook "java-mode")


