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
;; * Template for c project
;; *
;; *************************************************

;; Template for the declaration of an internal function
(stufe-register-template 
 '("c-internal-function-declaration"
   (["Documentation of the function" "%documentation%"]
    ["Variable declaration" "%declaration%"])
   "

%documentation%%declaration%
{
}
"))


;; Template for the declaration of an external function in the body
(stufe-register-template 
 '("c-function-body-declaration"
   (["Documentation of the function" "%documentation%"]
    ["Returned value" "%return-value%"]
    ["Function name" "%function-name%"]
    ["Arguments of the function" "%arguments%"])
   "

%documentation%
%return-value% %function-name%(%arguments%)
{
}
"))


;; Template for the declaration of an internal variable in the body
(stufe-register-template 
 '("c-variable-body-declaration"
   (["Documentation of the function" "%documentation%"]
    ["Variable declaration" "%declaration%" 
     (function . (lambda (argument) (stufe-fill-semi-colomn argument)))])
   "

%documentation%%declaration%
"))


;; Template for the declaration of an external variable in the body
(stufe-register-template
 '("c-extern-variable-body-declaration"
   (["Variable declaration" "%declaration%" 
     (function . (lambda (argument) (stufe-fill-semi-colomn argument)))])
"

%declaration%
"))

;; Template for the declaration of an external member
(stufe-register-template
 '("c-member-declaration"
   (["Documentation of the member" 
     "%documentation%"]
    ["Modifier of the declaration"
     "%modifier%"]
    ["Variable declaration" "%declaration%" 
     (function . (lambda (argument) (stufe-fill-semi-colomn argument)))])
"%documentation%%modifier% %declaration%"))


;; Template for the documentation of a variable or a function in C
(stufe-register-template
 '("c-declaration-documentation"
   (["Documentation on the argument of the function" 
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
"))

;; Template to create a C project makefile
(stufe-register-template 
 '("c-makefile"
   (["Name of the project" "%project-name%"]
    ["Options for shared object project" "%shared-object-options%"])
   "c_makefile.model"
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


