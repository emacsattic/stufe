;; *************************************************
;; * 
;; * Functions to specify the cpp mode
;; *
;; *************************************************


(stufe-register-mode '("c-prog-mode"
			 (
			   ;; Menu item
			  (lambda ()
			    (stufe-menu-add-item-local "Generate documentation" 
							 'stufe-make-doc))
			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  "prog-mode" 
			   
			   ;; Menu item
			   (lambda ()
			     (stufe-menu-add-item-local "Exec" 
							  'stufe-make-exec))

			  (lambda ()
			    (stufe-menu-add-menubar-local))

			   "debug-mode" 

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
			   )))

(stufe-register-mode '("c-mode"
			 ( 
			  (lambda ()
			    (stufe-menu-add-item-local "Switch c<-->h" 
							 'stufe-switch-cpp-file))
			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  "c-prog-mode" 
			   
			   ;; Menu item
			  (lambda ()
			    (stufe-menu-add-menubar-local))
			  (lambda ()
			    (stufe-menu-add-item-local "Create a new module..." 
							 'stufe-create-new-c-module))
			  ;; Keyboard shortcut
			  (lambda ()
			    (stufe-shortcut-add-local [(control o)] 
							'stufe-create-new-c-module))
			  )))


(stufe-register-mode '("cpp-mode"
			 ( 
			  (lambda ()
			    (stufe-menu-add-item-local "Switch cpp<-->hpp" 
							 'stufe-switch-cpp-file))
			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  "c-prog-mode" 
			   
			   ;; Menu item
			  (lambda ()
			    (stufe-menu-add-menubar-local))
			  (lambda ()
			    (stufe-menu-add-item-local "Create a new class..." 
							 'stufe-create-new-cpp-class))
			  (lambda ()
			    (stufe-menu-add-item-local "Create a new member..." 
							 'stufe-create-new-cpp-class-member))
			  
			  ;; Keyboard shortcut
			  (lambda ()
			    (stufe-shortcut-add-local [(control o)] 
							'stufe-create-new-cpp-class))
			  (lambda ()
			    (stufe-shortcut-add-local [(control f)] 
							'stufe-create-new-cpp-class-member))
			  )))


;; Add the hook to list of the things to do when opening a C++ file
(stufe-associate-mode-with-hook 'c++-mode-hook "cpp-mode")
(stufe-associate-mode-with-hook 'c-mode-hook "c-mode")


