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
;; * Grep functions for Stufe
;; *
;; *************************************************


(defvar stufe-grep-file-pattern
  "*.*"
  "Default pattern used for the grep command")


(defun stufe-grep-word-recursively
 (&optional file-pattern)
  "Run grep to match the current word with a file pattern recursively"
  (interactive)
  (stufe-grep-word-current 
   (format "$(find %s -name \"%s\")" 
	   (if stufe-working-folder
	       (concat stufe-working-folder "/")
	     "./")
	   (stufe-rebuild-string (split-string stufe-grep-file-pattern)
				 "\" -or -name \""))))



(defun stufe-grep-word-current (&optional file-pattern)
  "Run grep to match the current word with a file pattern"
  (interactive)
  (grep (format "%s -n %s %s" 
		grep-program 
		(current-word)
		(if file-pattern
		    file-pattern
		    stufe-grep-file-pattern))))

;; Define global key to bind with these functions
(global-set-key [(f2)] 'stufe-grep-word-current)
(global-set-key [(shift f2)] 'stufe-grep-word-recursively)


;; Add the items in the stufe menu
(defvar stufe-menu-grep-context
  nil
  "Context of the grep menu")

(setq stufe-menu-grep-context
      (stufe-add-menu-item-group "Grep"))

(stufe-add-menu-item stufe-menu-grep-context
		       "Grep word" 
		       'stufe-grep-word-current)

(stufe-add-menu-item stufe-menu-grep-context
		       "Grep word recursively" 
		       'stufe-grep-word-recursively)















