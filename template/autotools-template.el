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
;; * Template for Autotools project
;; *
;; *************************************************


;; Template to create a Makefile.am file
(stufe-register-template 
 '("autotools-Makefile.am"
   (["Name of the project" "%project-name%"])
   "autotools_Makefile.am.model"
   (model-from-file)))

;; Template to create a configure.ac file
(stufe-register-template 
 '("autotools-configure.ac"
   (["Name of the project" "%project-name%"])
   "autotools_configure.ac.model"
   (model-from-file)))

;; Template to create a bootstrap file
(stufe-register-template
 '("autotools-bootstrap" 
   ()
   "autotools_bootstrap.model"
   (model-from-file)))


