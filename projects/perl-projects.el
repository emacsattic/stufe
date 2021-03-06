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
;; * Functions to create a new Perl project
;; *
;; *************************************************

;; Perl project
(stufe-register-project 
 '("perl-script"
   (stufe-projects-new-folder
    (lambda (project-details)
      (let ((filename (concat (stufe-format-standard-name 
			       (stufe-project-get-name project-details))
			      ".pl")))
	(stufe-project-template-to-file project-details 
					"perl-script" 
					filename t)
	(set-file-modes (expand-file-name filename
					  (stufe-project-get-location project-details)) 755))))))


;; Add the menu item

(setq stufe-menu-latex-projects-context
      (stufe-add-menu-item-group "Perl Projects" 
				   (stufe-get-project-menu)))

(stufe-project-add-menu-item stufe-menu-latex-projects-context
			       "New script..." 
			       "perl-script")



