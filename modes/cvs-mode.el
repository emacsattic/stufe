;; *************************************************
;; * 
;; * Cvs menu
;; *
;; *************************************************

;; Define the prog mode
(stufe-register-mode '("cvs-mode"
			 (
			  (lambda ()
			    (stufe-menu-add-item-local "Add buffer" 
							 'stufe-cvs-add-buffer))
			  (lambda ()
			    (stufe-menu-add-item-local "Update" 
							 'stufe-cvs-update))
			  (lambda ()
			    (stufe-menu-add-item-local "Commit..." 
							 'stufe-cvs-commit)))))

