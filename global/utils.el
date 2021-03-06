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
;; * Global handy functions
;; *
;; *************************************************


(defun stufe-get-current-line ()
  "Return the current line of the current buffer"
  (+ (count-lines (point-min) (stufe-get-beginning-of-line)) 1))


(defun stufe-get-beginning-of-line ()
  "Return the position of the beginning of the current line"
  (save-excursion
    (beginning-of-line)
    (point)))

(defun stufe-get-end-of-line ()
  "Return the position of the beginning of the current line"
  (save-excursion
    (end-of-line)
    (point)))


(defun stufe-get-window-under-cursor ()
  "Return the window under the mouse cursor"
  (let ((mouse-at (mouse-position)))
    (window-at (car (cdr mouse-at))
	       (cdr (cdr mouse-at)))))


(defun stufe-rebuild-string (stufe-strings stufe-inserted)
  "Rebuild a string from a list of string and a substring by inserting
this substring between each string in the list"
  (if stufe-strings
      (concat (concat (car stufe-strings) 
		      (if (cdr stufe-strings)
			  stufe-inserted
			""))
	      (stufe-rebuild-string (cdr stufe-strings) stufe-inserted))
    ""))


(defun stufe-replace-words (stufe-string stufe-substring stufe-word)
  "Replace in a string a substring by a word"
  (let ((case-fold-search-old case-fold-search)
	(working-string stufe-string)
	(start-string-index 0)
	(stufe-length-word (length stufe-word)))
    (setq case-fold-search 'nil)
    (while (setq start-string-index (string-match stufe-substring 
						  working-string
						  start-string-index))
      (setq working-string 
	    (replace-match stufe-word 't 't working-string))
      (setq start-string-index (+ start-string-index
				  stufe-length-word)))
    (setq case-fold-search case-fold-search-old)
    working-string))


(defun stufe-format-standard-name (filename)
  "Format a filename to be readable by make"
  (stufe-replace-words (stufe-replace-words filename " " "")
			 "'" ""))


(defun stufe-choose-folder (prompt &optional default-filename default-dir)
  "Ask to the user to choose a folder and check 
if the folder has executable, readable and writable 
permissions"
  (read-file-name (concat prompt ": ") default-dir default-filename))


(defun stufe-choose-new-folder (prompt &optional default-dir)
  "Ask the user to choose a (new) folder in the minibuffer"
  (let ((insert-default-directory t))
    (read-file-name prompt default-dir default-dir)))


(defun stufe-file-to-string (filename)
  "Return a file as a string"
  (save-current-buffer
    (prog1
	(progn 
	  (set-buffer (create-file-buffer filename))
	  (insert-file-contents filename)
	  (buffer-string))
      (kill-buffer (current-buffer)))))


(defun stufe-string-to-file (string filename &optional view-file no-save indent)
  "Save the string as a new file in a buffer (the file is not saved on the disk"
  (save-current-buffer
    (find-file filename)
    (insert-string string)
    (if (not no-save)
	(save-buffer))
    (if indent
	(indent-region 0 (point-max) 'nil))
    (if (not view-file)
	(kill-buffer (current-buffer)))))


(defun stufe-find-file-in-folder-list (filename folder-list)
  "Try to find a file in a list of folder"
  (if folder-list
      (let* ((current-folder (car folder-list))
	     (current-filename (expand-file-name filename current-folder)))
	(if (file-readable-p current-filename)
	    current-filename
	  (stufe-find-file-in-folder-list filename (cdr folder-list))))))
    

(defun update-all-buffers ()
  "Update all the buffers in emacs"
  (interactive)
  (defun update-buffer (buffer)
    (let ((file-name (buffer-file-name buffer)))
      (if file-name
	  (if (not (verify-visited-file-modtime buffer))
	      (progn
		(with-current-buffer buffer
		  (revert-buffer t t))
		(format "%s: updated" (buffer-name buffer)))
	    (format "%s: ok" (buffer-name buffer))))))
  (mapcar 'update-buffer (buffer-list)))


(defun stufe-fill-semi-colomn (declaration)
  (if (string= ";" (substring declaration -1 nil))
      declaration
    (concat declaration ";")))


(defun stufe-string-to-current-buffer (string &optional indent)
  "Insert the string in the current buffer and indent"
  (save-excursion 
    (let* ((current-position (point)))
      (insert-string string)
      (if indent
	  (indent-region current-position 
			 (+ (length string) current-position)
			 nil)))))


(defun stufe-get-max-length (list)
  "Return the biggest length of the element in the list"
  (if list 
      (max (length (car list))
	   (stufe-get-max-length (cdr list)))
    0))


(defun stufe-fill-until (string len)
  "Fill a string with space until its length is len"
  (let ((working-string string))
    (while (< (length working-string) len)
      (setq working-string (concat working-string " ")))
    working-string))

      
;; Define the 'butlast' function if not defined
;; (I know... this is a quick bug fix, this is not
;; efficient at all)
;; I don't care because I have list of maximum 4 of length
(if (not (functionp 'butlast))
    (defun butlast (list index)	
      (reverse (nthcdr index (reverse list)))))

(when (not (fboundp 'ignore-errors))
  (defmacro ignore-errors (&rest body)
    "Execute FORMS; if an error occurs, return nil. 
Otherwise, return result of last FORM."
    `(condition-case nil (progn ,@body) (error nil))))

