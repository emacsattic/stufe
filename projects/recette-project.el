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
;; * Function pour créer une nouvelle recette de 
;; * cuisine
;; *
;; *************************************************

;; Need the LaTeX menu
(stufe-require-file "projects/latex-projects.el")

(stufe-register-project 
 '("recette-de-cuisine"
   (stufe-nouvelle-recette)
   (no-project-details)))


(defun stufe-create-recette-folder (recette-folder recette-file)
  (make-directory stufe-recettes-default-path)
  (let ((project-details (stufe-create-project-details-from-folder recette-folder
								   recette-file)))
    (stufe-project-template-to-file project-details
				    "latex-makefile" 
				    "makefile")
    (stufe-project-template-to-file project-details
				    "recette-cls" 
				    "recette.cls")))


(defun stufe-nouvelle-recette ()
  (let* ((template-name "recette-de-cuisine")
	 (template-args (stufe-read-template-arguments template-name))
	 (filepath (expand-file-name (concat (stufe-format-standard-name (car template-args))
					     ".tex")
				     stufe-recettes-default-path)))
    (if (not (file-exists-p stufe-recettes-default-path))
	(stufe-create-recette-folder stufe-recettes-default-path
				     (stufe-format-standard-name (car template-args))))
    (stufe-template-args-into-file template-name
				   template-args
				   filepath 't 't)))


;; Add the menu item 

(stufe-project-add-menu-item stufe-menu-latex-projects-context
			     "Nouvelle recette de cuisine..." 
			     "recette-de-cuisine")
