;; *************************************************
;; * 
;; * Functions to specify the EmacsLisp mode
;; *
;; *************************************************


(stufe-register-mode '("elisp-mode"
			 ("cvs-mode")))


;; Add the hook to list of the things to do when opening a EmacsLisp file
(stufe-associate-mode-with-hook 'emacs-lisp-mode-hook "elisp-mode")
