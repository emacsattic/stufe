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
;; * Template pour les recettes de cuisine
;; *
;; *************************************************


;; Template pour la création de nouvelle recette de cuisine
(stufe-register-template 
 '("recette-de-cuisine"
   (["Nom de la recette" "%nom-recette%"]
    ["Type de recette" "%type-recette%"]
    ["Temps de cuisson" "%temp-cuisson%"]
    ["Température de cuisson" "%degre-cuisson%"])
   "recette.model"
   (model-from-file)))

;; Enregistrement du modèle LaTeX
(stufe-register-template 
 '("recette-cls"
   ()
   "recette-cls.model"
   (model-from-file)))
