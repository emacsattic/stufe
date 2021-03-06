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
;; * Few bindings for ispell
;; *
;; *************************************************


;; Use the french dictionary by default
;; (ispell-change-dictionary "francais")


;; Bindings keys on handy functions
(global-set-key [(meta f3)] 'ispell-word)
(global-set-key [(f3)] 'flyspell-buffer)


;; Items menu
(defvar stufe-menu-spell-check-context
  nil
  "Context of the spell check menu")

(setq stufe-menu-spell-check-context
      (stufe-add-menu-item-group "Spell check"  nil "Update Stufe"))

(stufe-add-menu-item stufe-menu-spell-check-context 
		       "Define current dictionary" 
		       'ispell-change-dictionary)

(stufe-add-menu-item stufe-menu-spell-check-context 
		       "Buffer spell check" 
		       'flyspell-buffer)

(stufe-add-menu-item stufe-menu-spell-check-context 
		       "Word spell check" 
		       'ispell-word)

