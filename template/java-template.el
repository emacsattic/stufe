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


;; Template to create a Java bean property
(stufe-register-template
 '("java-bean-property" 
   (["Variable declaration" "%declaration%"]
    ["Variable name" "%variable-name%"]
    ["Variable type" "%variable-type%"]
    ["Variable standard name" "%variable-standard-name%"])
   "java_bean_property.model"
   (model-from-file)))


;; Template to create a new Java variable
(stufe-register-template
 '("java-variable-declaration"
   (["Variable declaration" "%declaration%" 
     (function . (lambda (argument) (stufe-fill-semi-colomn argument)))])
   "/**
 *
 * ...
 * 
 **/
%declaration%"))


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
%declaration% 
{

}"))



