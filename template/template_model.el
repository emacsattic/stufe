;; *************************************************
;; * 
;; * Template of new template
;; * A template is composed of :
;; *    - its name
;; *    - its keywords
;; *    - its model
;; *
;; *************************************************



(setq test-template '("template-name" 
		      (["Remplacement de tout" "tout"] 
		       ("le" 
			(["Remplacement de mot" "mot"])
			"(mot)")
		       ["Remplacement de monde" "monde"])
		      "Voici la phrase : bonjour tout le monde"))


(stufe-register-template test-template)

;; (stufe-apply-on-template "template-name")



