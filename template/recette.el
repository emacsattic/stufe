;; *************************************************
;; * 
;; * Template pour les recettes de cuisine
;; *
;; *************************************************


;; Template pour la cr�ation de nouvelle recette de cuisine
(stufe-register-template 
 '("recette-de-cuisine"
   (["Nom de la recette" "%nom-recette%"]
    ["Type de recette" "%type-recette%"]
    ["Temps de cuisson" "%temp-cuisson%"]
    ["Temp�rature de cuisson" "%degre-cuisson%"])
   "recette.model"
   (model-from-file)))

;; Enregistrement du mod�le LaTeX
(stufe-register-template 
 '("recette-cls"
   ()
   "recette-cls.model"
   (model-from-file)))
