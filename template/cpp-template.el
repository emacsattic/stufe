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
;; * Template for c/c++ project
;; *
;; *************************************************


;; Template to create a C project makefile
(stufe-register-template 
 '("cpp-makefile"
   (["Name of the project" "%project-name%"]
    ["Options for shared object project" "%shared-object-options%"])
   "cpp_makefile.model"
   (model-from-file)))

;; Template to create a C++ project makefile
(stufe-register-template 
 '("c-makefile"
   (["Name of the project" "%project-name%"]
    ["Options for shared object project" "%shared-object-options%"])
   "c_makefile.model"
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

(stufe-register-template
 '("cpp-main-so" 
   ()
   "cpp_main_so.model"
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
   "c_module_h.model"
   (model-from-file)))
(stufe-register-template 
 '("c-module-c" 
   (["Name of the module" "%ModuleName%"])
   "c_module_c.model"
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
    ["Arguments of the function" "%arguments%"]
    ["Modifiers of the function" "%modifiers%"])
   "

%return-value% %class-name%::%function-name%(%arguments%)%modifiers%
{
}
"))