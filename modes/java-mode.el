;; *************************************************
;; * 
;; * Functions to specify the Java mode
;; *
;; *************************************************


(stufe-register-mode '("java-mode"
			 ( "prog-mode"

			   ;; Menu item
			   (lambda ()
			     (stufe-menu-add-item-local "Exec" 
							  'stufe-make-exec))
			   
			   ;; Menu item
			   (lambda ()
			     (stufe-menu-add-menubar-local))
			   (lambda ()
			     (stufe-menu-add-item-local "Create a new class..." 
							  'stufe-create-new-java-class))
			   (lambda ()
			     (stufe-menu-add-item-local "Create a new member..." 
							  'stufe-create-new-java-class-member))
			   
			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(control o)] 
							 'stufe-create-new-java-class))
			   (lambda ()
			     (stufe-shortcut-add-local [(control f)] 
							 'stufe-create-new-java-class-member))
			   
			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(f5)] 
							 `stufe-make-exec))
			   )))


;; Add the hook to list of the things to do when opening a C++ file
(stufe-associate-mode-with-hook 'java-mode-hook "java-mode")


