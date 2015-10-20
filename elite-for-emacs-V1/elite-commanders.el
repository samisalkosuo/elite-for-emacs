;;; elite-commander.el -  Commanders for Emacs-Elite

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

(defconst CONDITION_DOCKED "Docked")
(defconst CONDITION_GREEN "Green")
(defconst CONDITION_YELLOW "Yellow")
(defconst CONDITION_RED "Red")

;crimnal record
(defconst CRIMINAL_TRADING 0)
(defconst CRIMINAL_SMUGGLING 1)

(defvar elite-criminal-record
  (list
   "Trading in illegal goods"
   "Smuggling illegal goods"
   )
  "criminal record text."
)



;commander log stats etc...
(defstruct commander-log
  name
  log
)

(defvar commander-visited-systems nil
  "list of systems where player has visited")


(defstruct commander
  id
  name
  gender
  elite-score;one kill, one point
  legal-status
  reputation;reputation, more the better
  credits
  fuel
  missiles
  current-planet
  hyperspace-system
  current-day
  condition;0=docked,1=green,2=yellow,3=red
  current-galaxy  
  cargo-capacity
  mates;list of mates
  current-cargo
  local-market
  lastrand
  fluct
  mission-number
  ;equipment list
  front-laser ;laser type
  rear-laser ;laser type
  right-laser ;laser type
  left-laser ;laser type
  ecm
  large-cargo-bay
  fuel-scoops
  energy-bomb
  energy-unit
  docking-computer
  galactic-hyperdrive
  escape-pod
  ;ship info etc
  ship-class
  ship-id
)

(defvar elite-commander (make-commander)
  "Elite commander data (player).")

(defun elite-create-commander (cmdr-name cmdr-gender)
  "Creates new elite commander."
	  ;create commander
  ;use this when dealing with player cash equipment etc
  (make-commander
   :id -1
   :name cmdr-name
   :gender cmdr-gender
   :elite-score 0
   :legal-status 0
   :reputation 0
   :credits elite-player-cash
   :fuel maxfuel
   :missiles 0
   :current-planet currentplanet
   :current-day 0
   :hyperspace-system currentplanet
   :condition CONDITION_DOCKED
   :current-galaxy galaxynum
   :cargo-capacity holdspace
   :mates (list)
   :current-cargo shipshold
   :local-market localmarket
   :lastrand (1+ lastrand) ;+1 because mysrand is rand-1
   :fluct 0
   :mission-number 0
   :front-laser "EQ_PULSE_LASER"
   :ecm nil
   :large-cargo-bay nil
   :fuel-scoops nil
   :energy-bomb nil
   :energy-unit nil
   :docking-computer nil
   :galactic-hyperdrive nil
   :escape-pod nil
   :ship-class [cl-struct-elite-ship "Cobra MK III" 20 200 201 4]
   :ship-id "AA-000"
   )
  )

(defun elite-update-commander ()
  "Update player commander."
  (setf (commander-current-planet elite-commander) currentplanet)
  (setf (commander-credits elite-commander) elite-player-cash)
  (setf (commander-fuel elite-commander) fuel)
  (setf (commander-current-cargo elite-commander) shipshold)
  (setf (commander-local-market elite-commander) localmarket)
  (setf (commander-lastrand elite-commander) (1+ lastrand))
)

(defvar elite-offender-fugitive-threshold 50
  "Legal status threshold between offender and fugitive")


(defstruct elite-rank
  score
  title
)

(defconst elite-ranks 10)

(defvar elite-rank-rating
  (vector
   (make-elite-rank 
    :score 0 
    :title "Harmless")
   (make-elite-rank 
    :score 4 
    :title "Mostly Harmless")
   (make-elite-rank 
    :score 8 
    :title "Poor")
   (make-elite-rank 
    :score 16 
    :title "Below Average")
   (make-elite-rank 
    :score 32 
    :title "Average")
   (make-elite-rank 
    :score 64 
    :title "Above Average")
   (make-elite-rank 
    :score 128 
    :title "Competent")
   (make-elite-rank 
    :score 1024 
    :title "Dangerous")
   (make-elite-rank 
    :score 4096 
    :title "Deadly")
   (make-elite-rank 
    :score 8192 
    :title "--- ELITE ---")
  ))


;;(elite-commander-write-player-path-file)
(defun elite-commander-write-player-path-file ()
  "Write path file to for external program that show how commanders a roaming in the galaxy"
  (let (
	(commander-path)
	(name)
	(path)
	(system)
	)
    (find-file "~/player.csv")
    (erase-buffer)

    (setq commander-path commander-visited-systems)
    (while commander-path
      (setq system (car commander-path))
      (insert (number-to-string system) ",")
      (insert (elite-get-system-name system) ",")
      (insert	(number-to-string (plansys-x (aref galaxy system ))) ",")
      (insert	(number-to-string (plansys-y (aref galaxy system ))) ",")
      (insert (aref econnames (plansys-economy (aref galaxy system ))) "\n")
      (setq commander-path (cdr commander-path))
      )
    (save-buffer)
    (kill-buffer (current-buffer))
    )
  )

(provide 'elite-commanders)