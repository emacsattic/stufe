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
				    (save-some-buffers 1)
				    (if (not (tex-shell-running))
					(tex-start-shell)
				      (save-current-buffer
					(save-excursion 
					  (set-buffer (get-buffer "*tex-shell*"))
					  (end-of-buffer)
					  (if (eq (current-column) 0)
					      (progn (stop-process (tex-shell-running) t)
						     (tex-send-command "bg"))))))
				    (tex-start-tex command "")
				    (if (stufe-get-compilation-window)
					(save-selected-window
					  (select-window (stufe-get-compilation-window))
					  (enlarge-window (- compilation-window-height
							     (window-height)))))))
			    (make-local-variable 'stufe-compilation-buffer-name)
			    (setq stufe-compilation-buffer-name "*tex-shell*"))
			  (lambda() 
			    (flyspell-mode 't))
			  (lambda()
			    (let ((dictionary (stufe-makefile-get-atomic-value 
					       (stufe-project-makefile-path) 
					       "DICTIONARY")))
			      (if dictionary
				  (ispell-change-dictionary dictionary))))

			  )))


;; Add the hook to list of the things to do when opening a LaTeX file
(stufe-associate-mode-with-hook 'tex-mode-hook "latex-mode")
(stufe-associate-mode-with-hook 'latex-mode-hook "latex-mode")

(stufe-associate-mode-with-hook 'TeX-mode-hook "latex-mode")
(stufe-associate-mode-with-hook 'LaTeX-mode-hook "latex-mode")
