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
;; * Functions to specify the prog mode used in most
;; * programming mode
;; *
;; *************************************************

;; Define the prog mode

(stufe-require-file "services/register.el")


(stufe-register-mode '("prog-mode"
			 (
			  "imenu-mode"

			  "makefile-mode"

			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  "cvs-mode"

			  (lambda ()
			    (stufe-menu-add-menubar-local))

			  (lambda ()
			    (stufe-menu-add-item-local "Open settings" 
							 'stufe-open-makefile))

			  (lambda ()
			    (stufe-shortcut-add-local [(control shift s)] 
							`stufe-open-makefile))
			  )))