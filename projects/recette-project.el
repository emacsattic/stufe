;; *************************************************
;; * 
;; * Function pour créer une nouvelle recette de 
;; * cuisine
;; *
;; *************************************************

(stufe-load-file "latex-projects.el")

(stufe-register-project 
 '("recette-de-cuisine"
   (stufe-nouvelle-recette)
   (no-project-details)))


(defun stufe-create-recette-folder (recette-folder)
  (make-directory stufe-recettes-default-path)
  (let ((project-details (stufe-create-project-details-from-folder recette-folder)))
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
	(stufe-create-recette-folder stufe-recettes-default-path))
    (stufe-template-args-into-file template-name
				     template-args
				     filepath 't 't)))


;; Add the menu item 

(stufe-project-add-menu-item stufe-menu-latex-projects-context
			       "Nouvelle recette de cuisine..." 
			       "recette-de-cuisine")
