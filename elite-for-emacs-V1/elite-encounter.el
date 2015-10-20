;;; elite-encounter.el -  Encounters in EmacsElite universe

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

;;; Commentary:

(defvar elite-encounter-preacher nil
  "use faith.el using preacher quotes, default nil")

(defun elite-random-encounter ()
  "More or less random encounter in Elite universe"
  (let (
	(encounter-happened)
	)
    (setq encounter-happened nil)
    ;pilgrims harrass player when there is one or more passenger    
  ;(if t
    (if (and elite-encounter-preacher (and (> (elite-passenger-on-board) 0) (> (elite-rand1 t) 253)))
	(progn	
	  (elite-encounter-do-preacher)
	  (setq encounter-happened t)
	  )      
      )
   
    )
)



(defun elite-encounter-do-preacher ()
  "Encounter in space.. The Preacher"
  (let (
	(msg)
	(tmp)
	)
  
	;pilgrim greets player
	  (elite-space-command-shell ">Preacher approaching..")
	  (sit-for 1 500)
	  (discard-input)
	  (elite-space-message-shell ">Incoming message:\nHail fellow travellers!\nHear my words of wisdom!")
					;offers words of wisdom in a message
					;
	  (setq msg (elite-split-string-for-mini-buffer (faith-quote)))
	  (while msg
	    (setq tmp (car msg))
	    (discard-input)
	    (read-string tmp " <more..>")
	    (discard-input)
	    (setq msg (cdr msg))
	    )
	  (sleep-for 0 250)
	  (discard-input)
	  (read-string "Let the Words be in your Thoughts!")
	  (sleep-for 0 500)
	  (discard-input)
	  (elite-space-message-shell ">Incoming message:\nGo in peace!")
	  (sleep-for 0 500)
	  (discard-input)
	  (elite-space-command-shell ">Preacher disappeared..")
  )
)


(provide 'elite-encounter)