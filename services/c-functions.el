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
;; * Functions to create a new c function of global 
;; * variable
;; *
;; *************************************************

(defun stufe-create-new-extern-c-function (function-declaration)
  "Create a new extern c function in C code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "c-member-declaration"
				 (list (stufe-apply-args-on-template 
					"c-declaration-documentation"
					(list 
					 (stufe-make-arguments-documentation
					  function-declaration)
					 (stufe-make-return-value-documentation
					  function-declaration)))
				       "extern"
				       function-declaration))
   'indent)
  (if (stufe-exist-body-file (buffer-file-name))
      (save-current-buffer 
	(let* ((class-name (stufe-get-cpp-class-name)))
	  (stufe-switch-cpp-file)
	  (stufe-goto-print-area (file-name-nondirectory (buffer-file-name)))
	  (stufe-string-to-current-buffer
	   (stufe-apply-args-on-template "c-function-body-declaration"
					 (list 
					  ""
					  (stufe-build-return-value-string 
					   function-declaration)
					  (stufe-build-function-name 
					   function-declaration)
					  (stufe-build-arguments-string
					   function-declaration)))
	   't)
	  (stufe-switch-cpp-file)))))


(defun stufe-create-new-extern-c-variable (member-declaration)
  "Create a new extern c variable in C code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "c-member-declaration"
				 (list (stufe-apply-args-on-template 
					"c-declaration-documentation"
					nil)
				       "extern"
				       member-declaration))
   'indent)
  (if (stufe-exist-body-file (buffer-file-name))
      (save-current-buffer 
	(stufe-switch-cpp-file)
	(stufe-goto-print-area (file-name-nondirectory (buffer-file-name)))
	(stufe-string-to-current-buffer
	 (stufe-apply-args-on-template "c-extern-variable-body-declaration"
				       (list member-declaration))
	 't)
	(stufe-switch-cpp-file))))


(defun stufe-create-new-intern-c-function (function-declaration)
  "Create a new intern c function in C code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "c-internal-function-declaration"
				 (list (stufe-apply-args-on-template 
					"c-declaration-documentation"
					(list 
					 (stufe-make-arguments-documentation
					  function-declaration)
					 (stufe-make-return-value-documentation
					  function-declaration)))
				       member-declaration))
   'indent))


(defun stufe-create-new-intern-c-variable (member-declaration)
  "Create a new intern c variable in C code"
  (stufe-string-to-current-buffer
   (stufe-apply-args-on-template "c-variable-body-declaration"
				 (list (stufe-apply-args-on-template 
					"c-declaration-documentation"
					nil)
				       member-declaration))
   'indent))


(defun stufe-create-new-c-member (member-declaration)
  "Create a new variable/function in C code"
  (interactive "*sMember declaration: ")
  (if (string= (file-name-extension (buffer-name)) "h")
      (if (stufe-is-a-function-declaration member-declaration)
	  (stufe-create-new-extern-c-function member-declaration)
	(stufe-create-new-extern-c-variable member-declaration))
    (if (stufe-is-a-function-declaration member-declaration)
	(stufe-create-new-intern-c-function member-declaration)
      (stufe-create-new-intern-c-variable member-declaration))))



;; *************************************************
;; * 
;; * Functions to create a new c module
;; *
;; *************************************************
(defun stufe-create-new-c-module (modulename)
  "Create a new C module with a headers"
  (interactive "*sModule name: ")
  (let ((filepath (expand-file-name (downcase modulename)
				    (file-name-directory buffer-file-name))))
    (stufe-template-args-into-file "c-module-c"
				     (list modulename "")
				     (concat filepath ".c")
				     'view-file
				     'no-save
				     'indent)
    (stufe-template-args-into-file "c-module-h"
				     (list modulename "")
				     (concat filepath ".h")
				     'view-file
				     'no-save
				     'indent)))

    


