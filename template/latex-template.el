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


;; Template to create a c/c++ main file
(stufe-register-template
 '("latex-main" 
   (["Name of the project" "%project-name%"]
    ["Author" "%author%" (function . (lambda (argument)
				       (user-full-name)))])
   "latex_main.model"
   (model-from-file)))
