;; *************************************************
;; * 
;; * Functions to specify the Perl mode
;; *
;; *************************************************


(stufe-register-mode '("perl-mode"
			 ("cvs-mode")))


;; Add the hook to list of the things to do when opening a Perl file
(stufe-associate-mode-with-hook 'perl-mode-hook "perl-mode")
