;; *************************************************
;; * 
;; * Main file of the plugin look at all the files 
;; * to load
;; *
;; *************************************************


(defvar stufe-root nil
  "Root folder used for the research of stufe file")


(defun stufe ()
  "Load the stufe module by loading the file in the current folder"
  (interactive)
  (unless stufe-root (setq stufe-root "~/stufe"))
  (message "Searching for stufe files to load in %s..." stufe-root)
  (setq stufe-file-loaded (list (expand-file-name "stufe.el" stufe-root)))
  (stufe-load-file (expand-file-name "services" stufe-root))
  (stufe-load-location stufe-root)
  (message "done."))

(defvar stufe-file-avoided
  '("." ".." "CVS")
  "List of files not loaded by stufe")


(defconst stufe-file-extension 
  "el"
  "Extension of the files loaded by stufe")


(defun stufe-file-allowed-p (file)
  "Returns true if the file can be loaded by stufe"
  (and (not (member (file-name-nondirectory file) 
		    stufe-file-avoided))
	    (not (member file stufe-file-loaded))
	    (file-readable-p file)))


(defun stufe-get-file-extension (file)
  "Returns the last extension of a file"
  (let ((file-name (file-name-sans-extension file)))
    (if (string= file file-name)
	""
      (substring file (+ (length file-name) 1)))))


(defun stufe-load-file (file)
  "Load a file : the file can be a folder or an elips file"
  (setq stufe-file-loaded (cons file stufe-file-loaded))
  (if (file-directory-p file)
      (stufe-load-location file)
    (when (string= stufe-file-extension 
		   (stufe-get-file-extension file))
      (load-file (if (file-exists-p file)
		     file
		   (expand-file-name file current-location))))))


(defun stufe-load-location (directory)
  "Load recursively all elisp file from the directory passed as argument. 
File already loaded are not reload."
  (let ((directory-files (directory-files directory 'full-name))
	(first-file-load (expand-file-name (concat
					    (file-name-nondirectory directory) ".el") 
					   directory))
	(load-one-file (lambda (file)
			 (if (stufe-file-allowed-p file)
			       (stufe-load-file file)))))
    (setq current-location directory)
    (funcall load-one-file first-file-load)
    (mapcar load-one-file directory-files)))


(stufe)


