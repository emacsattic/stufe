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
;; * Template for LaTeX project
;; *
;; *************************************************


;; Template to create a LaTeX project makefile
(stufe-register-template 
 '("latex-makefile" 
   (["Name of the project" "%project-name%"])
   "latex_makefile.model"
   (model-from-file)))


;; Template to create a LaTeX article
(stufe-register-template
 '("latex-article" 
   (["Name of the project" "%project-name%"]
    ["Author" "%author%" (function . (lambda (argument)
				       (user-full-name)))])
   "latex_article.model"
   (model-from-file)))



;; Template to create a LaTeX letter
(stufe-register-template
 '("latex-letter" 
   (["Name of the project" "%project-name%"]
    ["Author" "%author%" (function . (lambda (argument)
				       (user-full-name)))])
   "latex_letter.model"
   (model-from-file)))


;; Template to create a LaTeX presentation
(stufe-register-template
 '("latex-presentation" 
   (["Name of the project" "%project-name%"]
    ["Author" "%author%" (function . (lambda (argument)
				       (user-full-name)))])
   "latex_presentation.model"
   (model-from-file)))
