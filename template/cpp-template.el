;; *************************************************
;; * 
;; * Template for c/c++ project
;; *
;; *************************************************


;; Template to create a c/c++ project makefile
(stufe-register-template 
 '("cpp-makefile"
   (["Name of the project" "%project-name%"]
    ["Options for shared object project" "%shared-object-options%"])
   "cpp_makefile.model"
   (model-from-file)))


;; Template to create a c/c++ shared object makefile options 
(stufe-register-template
 '("cpp-so-makefile-options" 
   ()
   "so_makefile_options.model"
   (model-from-file)))


;; Template to create a c/c++ main file
(stufe-register-template
 '("cpp-main" 
   ()
   "cpp_main.model"
   (model-from-file)))


;; Template to create a new class file
(stufe-register-template 
 '("cpp-class-hpp" 
   (["Name of the class" "%ClassName%"]
    ["Author" "%Author%" (function . (lambda (argument)
				       (user-full-name)))])
   "cpp_class_hpp.model"
   (model-from-file)))
(stufe-register-template 
 '("cpp-class-cpp" 
   (["Name of the class" "%ClassName%"]
    ["Author" "%Author%" (function . (lambda (argument)
				       (user-full-name)))])
   "cpp_class_cpp.model"
   (model-from-file)))


;; Template to create a new module file
(stufe-register-template 
 '("c-module-h" 
   (["Name of the module" "%ModuleName%"])
   "c_module_gnu_h.model"
   (model-from-file)))
(stufe-register-template 
 '("c-module-c" 
   (["Name of the module" "%ModuleName%"])
   "c_module_gnu_c.model"
   (model-from-file)))


;; Template to create new members in class
(stufe-register-template
 '("cpp-class-member-declaration"
   (["Variable declaration" "%declaration%" 
     (function . (lambda (argument) (stufe-fill-semi-colomn argument)))]
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
%declaration%
"))


(stufe-register-template 
 '("cpp-argument-documentation"
   (["Argument name" "%argument-name%"
     (function . (lambda (argument) 
		   (stufe-fill-until (car argument) (cdr argument))))])
   "
 * @param   %argument-name%  ..."))


(stufe-register-template 
 '("cpp-function-return-value-documentation"
   (["Description of the return value in a function" 
     "%function-return-value%" (default . "void")])
   "
 * @return  %function-return-value%"))


(stufe-register-template 
 '("function-body-declaration"
   (["Returned value" "%return-value%"]
    ["Name of the class" "%class-name%"]
    ["Function name" "%function-name%"]
    ["Arguments of the function" "%arguments%"])
   "

%return-value% %class-name%::%function-name%(%arguments%) 
{
}
"))