;; *************************************************
;; * 
;; * Template for Java project
;; *
;; *************************************************


;; Template to create a Java project makefile
(stufe-register-template 
 '("java-makefile"
   (["Name of the project" "%project-name%"])
   "java_makefile.model"
   (model-from-file)))


;; Template to create a Java class
(stufe-register-template
 '("java-class" 
   (["Name of the class" "%ClassName%"]
    ["Author" "%Author%" (function . (lambda (argument)
				       (user-full-name)))]
    ["Class body" "%classbody%"])
   "java_class.model"
   (model-from-file)))


;; Template to create a new Java member
(stufe-register-template
 '("java-member-declaration"
   (["Variable declaration" "%declaration%"]
    ["Documentation on the argument of the function" 
     "%argument-documentation%"
     (function . (lambda (argument) 
		   (if argument (concat argument "\n *"))))]
    ["Description of the return value in a function" 
     "%function-return-value%"
     (function . (lambda (argument) 
		   (if argument (concat argument "\n *"))))])
   "/**
 *
 * ...
 * %argument-documentation%%function-return-value%
 **/
%declaration% {

}"))



