;;; elite-federation.el -  Elite Federation functions 

;; Elite for EMACS is based on Elite series by Ian Bell and David Braben.
;; Original Elite, (C) 1984 Ian Bell and David Braben.
;; Elite for EMACS uses code from original Elite and it is (C) 1984 Ian Bell and David Braben.
;; Additional code :
;; Author: Sami Salkosuo (sami@roguemail.net)

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;Commentary
;;Functions for elite federation activity, such as message when new commander is born etc.

(defvar elite-federation-commander-born (list)
  "List of commanders that were born while player was in space")

(defun elite-federation-request-info ()
  "Player can request information from elite federation, like ai commander info etc."
  (let (

	)

    )
)


(defun elite-federation-messages ()
  "messages from elite federation when player dock to station"
  (let (
	(txt)
	(commander)
	(gender)
	)
    (if (> (length elite-federation-commander-born) 0)
	(progn
	  ;;commanders has been born
	  (while elite-federation-commander-born
	    (setq commander (nth (car elite-federation-commander-born) elite-ai-commander-list ))
	    (if (= (elite-ai-commander-gender commander) ELITE_GENDER_MALE)
		(setq gender "he")
	      (setq gender "she")
		)
	    (setq txt (concat
		       ">Incoming message from Elite Federation of Pilots:\n"
		       "All hail! Federation is pleased to welcome "
		       (elite-ai-commander-name commander)
		       ", "
		       (elite-ai-commander-heritage commander)
		       ", among The Few in The Eight Galaxies.\nCommander "
		       (elite-ai-commander-name commander)
		       " emerged from the Tleilax facilities in "
		       (elite-get-system-name (elite-ai-commander-birthplace commander))
		       " and "
		       gender
		       " is eager to roam galaxy with us.\nQ'Apla!\n"
		       )
		  )
	    (elite-message-shell txt)

	    (setq elite-federation-commander-born (cdr elite-federation-commander-born))
	    )
	  
	  )
      )  
    )
)

(provide 'elite-federation)