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
;; * Functions to use a makefile
;; *
;; *************************************************


(stufe-require-file "services/register.el")


;; Define the makefile mode

(stufe-register-mode '("makefile-mode"
			 (
			  (lambda ()
			    (stufe-menu-add-item-local "Rebuild" 
							 'stufe-make-rebuild))
			  (lambda ()
			    (stufe-menu-add-item-local "Clean" 
							 'stufe-make-clean))

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

			  )))


(stufe-register-mode '("makefile-file-mode"
			 ( "makefile-mode"

			   ;; Menu item
			   (lambda ()
			     (stufe-menu-add-item-local "Exec" 
							  'stufe-make-exec))
			   			   
			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(f5)] 
							 `stufe-make-exec))
			   )))


(stufe-associate-mode-with-hook 'makefile-mode-hook "makefile-file-mode")
