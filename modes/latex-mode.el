;; *************************************************
;; * 
;; * Functions to specify the LaTeX mode
;; *
;; *************************************************


(stufe-register-mode '("latex-mode"

			 ("prog-mode"
			  (lambda ()
			    (stufe-menu-add-item-local "View" 
							 'stufe-make-exec))
			  (lambda ()
			    (stufe-shortcut-add-local [(f5)]
							'stufe-make-exec))
			  (lambda ()
			    (setq tex-start-options-string "")
			    (make-local-variable 'stufe-compile-function)
			    (setq stufe-compile-function 
				  (lambda (command) 
				    (if (tex-shell-running) 
					(tex-kill-job) 
				      (tex-start-shell)) 
				    (tex-start-tex command "")
				    (if (stufe-get-compilation-window)
					(save-selected-window
					  (select-window (stufe-get-compilation-window))
					  (enlarge-window (- compilation-window-height
							     (window-height)))))))
			    (make-local-variable 'stufe-compilation-buffer-name)
			    (setq stufe-compilation-buffer-name "*tex-shell*"))
			  (lambda () 
			    (if stufe-default-ispell-langage
				((ispell-change-dictionary stufe-default-ispell-langage)
				 (flyspell-mode 1)
				 (flyspell-buffer))))
			  )))


;; Add the hook to list of the things to do when opening a C++ file
(stufe-associate-mode-with-hook 'tex-mode-hook "latex-mode")
(stufe-associate-mode-with-hook 'latex-mode-hook "latex-mode")

(stufe-associate-mode-with-hook 'TeX-mode-hook "latex-mode")
(stufe-associate-mode-with-hook 'LaTeX-mode-hook "latex-mode")
