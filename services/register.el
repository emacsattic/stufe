;; *************************************************
;; * 
;; * Function to be able to register data 
;; * (ie template and project)
;; *
;; *************************************************


(defun stufe-register-new (register-name)
  "Return a new register data with no data in"
  (list register-name '()))

(defun stufe-register-get-description (register-dataname register-data)
  "Get a data registered from its name, throw an error if not found"
  (let ((data-description (assoc register-dataname 
				     (stufe-register-get-data-registered register-data))))
    (if data-description
	data-description
      (error "\"%s\" not found in \"%s\". Please check it has been registered." 
	     register-dataname
	     (stufe-register-get-data-name register-data)))))


(defun stufe-register-add-data (register-description register-data)
  "Add a new data in the registered data list, replace an old one if it already exists"
  (let ((data-description (assoc (car register-description) 
				     (stufe-register-get-data-registered register-data)))
	(data-registered (stufe-register-get-data-registered register-data)))
    (list (stufe-register-get-data-name register-data)
	  (cons register-description
		(if data-description
		    (delete data-description 
			    data-registered)
		  data-registered)))))



(defun stufe-register-get-data-name (register-data)
  "Return the name of a register"
  (car register-data))


(defun stufe-register-get-data-registered (register-data)
  "Return the list of registered data"
  (car (cdr register-data)))