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
;; * Functions to specify the Perl mode
;; *
;; *************************************************

(stufe-require-file "services/register.el")


(stufe-register-mode '("perl-mode"
			 ("imenu-mode"

			  ;; Defines local buffer variables
			  (lambda ()
			    ;; Grep patterns
			    (make-local-variable 'stufe-grep-file-pattern)
			    (setq stufe-grep-file-pattern "*.pl"))

			  "cvs-mode")))


;; Add the hook to list of the things to do when opening a Perl file
(stufe-associate-mode-with-hook 'perl-mode-hook "perl-mode")
