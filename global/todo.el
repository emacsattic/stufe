;; *************************************************
;; * 
;; * Fonction pour faciliter le debuggage
;; *
;; *************************************************

(setq project-name nil)

(defun get-compilation-buffer ()
  "Renvoie le buffer de compilation (en créer un si nécessaire)"
  (get-buffer-create "*compilation*")
)

(defun get-last-debug-buffer-name ()
  "Retourne le nom du buffer du dernier projet compilé"
  (if project-name
      (concat "*gud-" (concat project-name "*"))
    nil
    )
)

(defun find-debug-window ()
  "Retourne la fenetre de debuggage si elle existe"
  (interactive)
  (if project-name
      (get-buffer-window (get-last-debug-buffer-name))
    nil
    )
)

(defun display-compilation-window ()
  "Affiche à la place de la fenetre de debuggage la fenetre de compilation"
  (if (get-buffer "*compilation*")
      (if (not (get-buffer-window "*compilation*"))
	  (if (find-debug-window)
	      (save-selected-window 
		(set-window-buffer (find-debug-window) (get-compilation-buffer))
		)
	    )
	)
    )
)


(defun is-compile-success ()
  "Se renseigne si la compilation a été un succés ou non"
  (interactive)
  (save-current-buffer
    (set-buffer (get-compilation-buffer))
    (goto-char (point-min))
    (search-forward (magic-debug-sentence) nil t)
    )
)

(defun magic-debug-sentence ()
  "Phrase magique pour retrouver le nom du projet dans la fenetre de compilation"
  " is waiting to be debugged..."
)


(defun update-project-name ()
  "Retrouve le nom du projet dans la fenetre de compilation (si la compilation a été un succés)"
  (interactive)
  (save-current-buffer
    (set-buffer (get-compilation-buffer))
    (goto-char (point-max))
    (let 
	((end-position (search-backward (magic-debug-sentence)))
	 (start-position (+ (search-backward "\n") 1))
	 )
      (setq project-name (buffer-substring start-position end-position))
      )
    )
)


(defun clear-debug-buffer ()
  (interactive)
  (if (get-last-debug-buffer-name)
      (save-current-buffer
	(set-buffer (get-last-debug-buffer-name))
	(erase-buffer)
      ))
)



(defun compile-sentinel (process event)
  (if (is-compile-success)
      (save-selected-window 
	(if (find-debug-window)
	    (select-window (find-debug-window))
	  (select-window (get-buffer-window (get-compilation-buffer)))
	  )
	(update-project-name)
	(gdb (concat "gdb " (if default-folder
				(concat default-folder project-name)
			      project-name)))
	(send-debug-command "break main")
	(send-debug-command "run")
	(send-command-list breakpoint-list)
	(send-debug-command "continue"))
    (message "compile failed")))


(defun make-debug ()
  "Appel la commande 'make debug'"
  (interactive)
  (debug-kill)
  (clear-debug-buffer)
  (run-make "debug")
  (set-process-sentinel (get-buffer-process "*compilation*") 'compile-sentinel)
)

(defun is-debug-actif ()
  (if  (get-last-debug-buffer-name)
      (get-buffer-process (get-last-debug-buffer-name))
    nil
))

(defun send-debug-command (command)
  (if (is-program-running)
      (process-send-string (get-last-debug-buffer-name) (concat command "\n"))
     (message "Debug is not actif")
    )
)

(defun debug-next ()
  (interactive)
  (send-debug-command "next"))

(defun debug-step ()
  (interactive)
  (send-debug-command "step")
)

(defun get-breakpoint ()
  (format "break %s:%s" (buffer-name) (get-current-line)))

(defun get-watch (variable)
  (concat "disp " variable)
)

(defun clear-breakpoint ()
  (interactive)
  (setq breakpoint-list nil)
  (message "La liste des breakpoints a été effacée")
)
(clear-breakpoint)

(defun clear-watch ()
  (interactive)
  (setq watch-list nil)
  (message "La liste des variables à visualiser a été effacée")
)
(clear-watch)

(defun debug-display-watch ()
  (interactive)
  (message (format "%s" watch-list))
  (if (is-program-running)
      (send-command-list watch-list)
    )
)

(defun debug-display-breakpoint ()
  (interactive)
  (message (format "%s" breakpoint-list))
  (if (is-program-running)
      (send-command-list breakpoint-list)
    )
)


(defun debug-watch (variable)
  (interactive "*sNom de la variable : ")
  (let ((watch-variable (get-watch variable)))
    (if (not (member watch-variable watch-list))
	(setq watch-list (cons watch-variable watch-list))
      (setq watch-list (delete watch-variable watch-list))
      )
    )
  (message (format "%s" watch-list))
)

(defun send-command-list (list)
  (if list
      (save-current-buffer
	(send-debug-command (car list))
	(send-command-list (cdr list)))
    )
)


(defun debug-breakpoint ()
  (interactive)
  (if (not (member (get-breakpoint) breakpoint-list))
      (setq breakpoint-list (cons (get-breakpoint) breakpoint-list))
    (setq breakpoint-list (delete (get-breakpoint) breakpoint-list))
    )
  (message (format "%s" breakpoint-list))
)

(defun is-program-running ()
  (save-current-buffer
    (set-buffer (get-last-debug-buffer-name))
    (goto-char (point-max))
    (if (search-backward "Program exited normally." nil t)
	nil
      t
      )
    )
)
  
(defun debug-start-run ()
  (interactive)
  (send-debug-command "continue")
)

(defun launch-debug ()
  (interactive)
  (if (is-debug-actif)
      (if (is-program-running)
	  (debug-start-run)
	(make-debug)
	)
    (make-debug)
    )
)

(defun debug-kill ()
  (interactive)
  (if (is-debug-actif)
      (delete-process (get-last-debug-buffer-name))
    )
  (display-compilation-window)
  (save-current-buffer
    (set-buffer "*compilation*")
    (goto-char (point-max))
    (insert-string "Debug terminated.")
    )
)

;; *************************************************
;; * 
;; * Fonction pour faciliter l'écriture du code
;; *
;; *************************************************

;; Fonction pour creer une nouvelle classe sur deux fichiers C++ différents

(defun create-new-classe (name)
"Creer une nouvelle classe, elle prend le nom en paramètre
puis creer un fichier '%nom de classe%.hpp' pour mettre sa
déclaration et un fichier '%nom de classe%.cpp' pour
écrire le corps des fonctions de la classe"
(interactive "*sNom de la nouvelle classe : ")
(save-current-buffer 
  (save-selected-window
    (create-new-cpp-file name)
    (create-new-header-file name)
    )
  )
)

(defun create-new-header-file (name)
  (let ((capital-name (upcase name))
	(downcase-name (downcase name)))
    (find-file (format "%s.hpp" downcase-name))
    (insert-string (format 
"//( %s.hpp

#ifndef %s_HPP
#define %s_HPP



//( %s

/**
 * ...
 *
 * @author Delphine Dard & Delphine Rider
**/
class %s 
{
public:

}
;

//) %s


#endif

//) %s.hpp
" downcase-name capital-name capital-name
name name name downcase-name ))))


(defun create-new-cpp-file (name)
  (let ((capital-name (upcase name))
	(downcase-name (downcase name)))
    (find-file (format "%s.cpp" downcase-name))
    (insert-string (format 
"//( %s.cpp
//
// @author Delphine Dard & Delphine Rider
//
//


#include \"%s.hpp\"


//( %s


//) %s


//) %s.cpp
" downcase-name  downcase-name
name name downcase-name ))))



(defun get-position (base-string char)
  "Retourne la position d'un caractère dans une chaîne"
  (let ((chaine (car (split-string base-string (format "[%s]" char)))))
    (if (= (length base-string) (length chaine))
	nil
      (length chaine)
      )
    )
  )

(defun get-file-extension (file-name)
  "Retourne l'extension du nom d'un fichier (tout ce qu'il y a après le premier point trouvé)"
  (let ((extension-position (get-position file-name ".")))
    (if extension-position
	(remove-from-char (substring file-name extension-position) "<")
      )
    )
  )

(defun get-file-name (file-name)
  "Retourne le nom propre d'un fichier sans son extension"
  (remove-from-char file-name ".")
  )

(defun file-present (filename)
  (or (file-exists-p filename)
      (get-file-buffer filename)))
  
(defun switch-cpp-buffer ()
  "Switch entre .cpp et .hpp d'une même classe"
  (interactive)
  (let ((extension (get-file-extension (buffer-name)))
	(name (get-file-name (buffer-name)))
	)
    (cond ((string= extension ".cpp") (if (file-present (concat name ".hpp"))
					  (find-file (concat name ".hpp"))
					(find-file (concat name ".h"))))
	   ((string= extension ".hpp") (find-file (concat name ".cpp")))
	   ((string= extension ".h") (if (file-present (concat name ".c"))
					 (find-file (concat name ".c"))
				       (find-file (concat name ".cpp"))))
	    ((string= extension ".c") (find-file (concat name ".h"))))))

(defun write-new-champ (newchamp)
  (save-excursion
    (let ((position (point)))
      (insert-string (format 
"/**
   * 
   * ...
   *
   *
  **/
%s;
" newchamp)
		     )
      (indent-according-to-mode)
      (indent-region position (point) nil)
      )
    )
  )

(defun create-new-champ (new-champ)
  "Créer un nouveau champ en mettant le code correspondant dans les fichiers hpp"
  (interactive "*sDéclaration du nouveau champ : ")
  (if (string= (get-file-extension (buffer-name)) ".hpp")
      (write-new-champ (remove-from-char new-champ ";"))
    (message "This buffer is not good")
    )
  )
  
(defun create-new-fonction (new-fonction)    
  "Créer une nouvelle fonction en mettant le code correspondant dans les fichiers hpp et cpp"
  (interactive "*sDéclaration de la nouvelle fonction : ")
  (if (string= (get-file-extension (buffer-name)) ".hpp")
      (write-new-fonction new-fonction)
    (message "This buffer is not good")
    )
  )

(defun clean-fonction-declaration (liste)
  (let ((current-element (car liste)))
    (cond 
     ((string= current-element "virtual") (clean-fonction-declaration (cdr liste)))
     ((string= current-element "static") (clean-fonction-declaration (cdr liste)))
     (t liste)
     )
    )
  )
(defun get-base-declaration (fonction separator)
  (clean-fonction-declaration (split-string fonction separator))
)

(defun get-fonction-return-type-rec (liste resultat)
  (if (is-fonction-name liste)
      resultat
    (format "%s %s" (car liste) (get-fonction-return-type-rec (cdr liste) resultat))
    )
)
(defun get-fonction-return-type (fonction)
  "Retourne le type de retour d'une fonction depuis sa déclaration"
  (get-fonction-return-type-rec (get-base-declaration fonction " ") "")
)


(defun is-fonction-name (liste)
  (or  (get-position (car liste) "(") 
       (string= "(" (substring (cadr liste) 0 1))
       )
  )
(defun get-fonction-name-rec (liste)
  (let ((position (get-position (car liste) "(")))
    (if (is-fonction-name liste)
	(substring (car liste) 0 position)
      (get-fonction-name-rec (cdr liste))
      )
    )
  )
(defun get-fonction-name (fonction)
  "Retourne le nom d'une fonction depuis sa déclaration"
  (get-fonction-name-rec (get-base-declaration fonction " "))
)


(defun get-fonction-all-parameters (fonction)
  "Retourne une chaine de caractère contenant tous les paramètres de la fonction"
  (let ((parameters (car (cdr (split-string fonction "[()]")))))
    (if (string= parameters "")
	""
      parameters
      )
    )
)
(defun get-commentaire-renvoie (fonction)
  "Retourne 'rien' si le type renvoyé par la fonction est void"
  (cond
   ((equal (get-fonction-return-type fonction) "") "")
   ((equal (get-fonction-return-type fonction) "void ") "\n   * @return rien\n   *")
   (t "\n   * @return ...\n   *")
   )
)

(defun remove-suffixe (argument)
  (remove-from-char (remove-from-char argument "\[") "=")
  )

(defun get-last (liste)
  (cond 
   ((null liste) ())
   ((null (cdr liste)) (car liste))
   (t (get-last (cdr liste)))
   )
  )   

(defun get-identificateur (argument-string)
  (get-last (split-string (remove-suffixe argument-string) "[ *]"))
)


(defun get-first-identificateur (arguments-liste)
  "Retourne le premier identificateur de la liste de parametre"
  (get-identificateur (car arguments-liste))
  )


(defun get-format-identificateur (arguments-liste longueur)
  (let ((identificateur (get-first-identificateur arguments-liste)))   
    (concat identificateur (make-string (- longueur (length identificateur)) ?\  ))
    )
)

(defun get-commentaire-arguments-rec (arguments-liste resultat longueur)
  "Retourne une chaine de caractères de commentaire en C pour la liste des 
arguments d'une fonction"
    (if arguments-liste
	(let ((new-resultat (format "%s\n   * @param %s ..." 
				    resultat 
				    (get-format-identificateur arguments-liste longueur))))
	  (get-commentaire-arguments-rec (cdr arguments-liste) new-resultat longueur)
	  )
      resultat
      )
    )

(defun get-longueur (arguments-liste resultat)
  (if arguments-liste
      (let ((longueur (length (get-first-identificateur arguments-liste))))
	(if (> longueur resultat)
	    (get-longueur (cdr arguments-liste) longueur)
	  (get-longueur (cdr arguments-liste) resultat)
	  )
	)
    resultat
    )
)

(defun get-commentaire-arguments (fonction)
  "Retourne les arguments d'une fonction formatté pour les commentaires"
  (let ((arguments-string (get-fonction-all-parameters fonction)))
    (if (not (equal arguments-string ""))
	(let ((arguments-liste (split-string arguments-string ","))
	      (longueur (get-longueur (split-string arguments-string ",") 0))
	      )
	  (get-commentaire-arguments-rec arguments-liste "" (+ longueur 1))
	)
      ""
      )
    )
  )

(defun remove-from-char (string char)
  (car (split-string string (format "[%s]" char))
  )
)

(defun write-new-fonction (fonction)
  "Ecrit la déclaration de la fonction dans le cpp puis dans le hpp"
  (let (
	(class-name (get-file-name (buffer-name)))
	(fonction-declaration (remove-from-char fonction ";"))
	)
    (write-in-cpp-file class-name fonction-declaration)
    (write-in-hpp-file class-name fonction-declaration)
    )
  )

(defun write-in-hpp-file (class-name fonction)
  "Ecrit la déclaration de la fonction dans le fichier cpp"
  (interactive)
  (let ((file-name (concat class-name ".hpp"))
	(type-renvoie (get-fonction-return-type fonction))
	(commentaire-renvoie (get-commentaire-renvoie fonction))
	(commentaire-arguments (get-commentaire-arguments fonction))
	(fonction-name (get-fonction-name fonction))
	(param-string (get-fonction-all-parameters fonction))
	)
    (find-file file-name)
    (save-excursion
      (let ((position (point)))
	(insert-string (format 
"/**
   * 
   * ...
   * %s
   * %s
  **/
%s;
" commentaire-arguments commentaire-renvoie fonction)
		       )
	(indent-according-to-mode)
	(indent-region position (point) nil)
	)
      )
    )
  )

(defun write-in-cpp-file (class-name fonction)
  "Ecrit la déclaration de la fonction dans le fichier cpp"
  (interactive)
  (let ((file-name (concat class-name ".cpp"))
	(type-renvoie (get-fonction-return-type fonction))
	(fonction-name (get-fonction-name fonction))
	(param-string (get-fonction-all-parameters fonction))
	)
    (find-file file-name)
    (save-excursion
      (goto-char (point-max))
      (search-backward (format "//) %s" class-name))
      (let ((position (point)))
	(insert-string (format 
			"%s %s::%s(%s) 
{

}

" type-renvoie class-name fonction-name param-string)
		       )
	(indent-according-to-mode)
	(indent-region position (point) nil)
	)
      )
    )
  )
    

