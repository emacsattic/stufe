;; *************************************************
;; * 
;; * Debug menu
;; *
;; *************************************************

;; Define the prog mode
(stufe-register-mode '("debug-mode"
			 (
			  ;; Menu item
			  (lambda ()
			    (stufe-menu-add-item-local "Set/unset breakpoints" 
							 'stufe-set-breakpoint))
			  (lambda ()
			    (stufe-menu-add-item-local "Debug project..." 
							 'stufe-debug-project))

			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(ctrl f5)] 
							 `stufe-debug-project))
			   (lambda ()
			     (stufe-shortcut-add-local [(f9)] 
							 `stufe-set-breakpoint))
							
			  )))

