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
;; * Debug menu
;; *
;; *************************************************

;; Define the prog mode
(stufe-register-mode '("debug-mode"
			 (
			  ;; Menu item
			  (lambda ()
			    (stufe-menu-add-item-local "Debug project..." 
							 'stufe-debug-project))

			  (lambda ()
			    (stufe-menu-add-item-local "Set/unset breakpoints" 
							 'stufe-set-breakpoint))

			   ;; Keyboard shortcut
			   (lambda ()
			     (stufe-shortcut-add-local [(ctrl f5)] 
							 `stufe-debug-project))
			   (lambda ()
			     (stufe-shortcut-add-local [(f9)] 
							 `stufe-set-breakpoint))
							
			  )))

