;; *************************************************
;; * 
;; * Functions to specify the prog mode used in most
;; * programming mode
;; *
;; *************************************************

;; Define the prog mode

(stufe-register-mode '("prog-mode"
			 (
			  "cvs-mode"

			  (lambda ()
			    (stufe-menu-add-menubar-local))
			  (lambda ()
			    (stufe-menu-add-item-local "Open settings" 
							 'stufe-open-makefile))
			  (lambda ()
			    (stufe-menu-add-item-local "Rebuild" 
							 'stufe-make-rebuild))
			  (lambda ()
			    (stufe-menu-add-item-local "Clean" 
							 'stufe-make-clean))
			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  (lambda ()
			    (stufe-menu-add-item-local "Compile" 
							 'stufe-make-build))

			  ;; Add the shortcut			  
			  (lambda ()
			    (stufe-shortcut-add-local [(f7)] 
							`stufe-make-build))
			  (lambda ()
			    (stufe-shortcut-add-local [(meta f7)] 
							`stufe-make-clean))
			  (lambda ()
			    (stufe-shortcut-add-local [(control f7)] 
							`stufe-make-rebuild))
			  (lambda ()
			    (stufe-shortcut-add-local [(control shift s)] 
							`stufe-open-makefile))
			  )))