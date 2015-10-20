;;; elite-commands.el -  Commands for Emacs-Elite

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

(defun elite-display-market ()
  "Displays market for local system."
;  (interactive)
  (let (
	(msg)
	(i)
	(commodity)
	)
  (setq i 0)
  (setq msg "")
  (while (<= i lasttrade)
    (setq commodity (aref commodities i))    

;Food         3.2 19t
    (setq msg (concat 
	       msg 
	       (tradegood-name commodity)
	       " " 
	       (format "%.1f" (/ (aref (markettype-price localmarket) i) 10.0))
	       " "
	       (format "%d%s" (aref (markettype-quantity localmarket) i) (aref unitnames (tradegood-units commodity)))
	       ;in cargo hold
	       (format "/%d%s" (aref shipshold i) (aref unitnames (tradegood-units commodity)))	       
	       "\n"))

    (setq i (1+ i))
    )
  (elite-insert-text msg "market")
  ))

(defun elite-display-ai-commanders ()
  "Displays AI commanders in current system"
  (interactive)
  (let (
	(txt)
	(commanders-in-system)
	(commander)
	(len)
	)
    (setq txt (concat ">Commanders in " (elite-get-system-name currentplanet) "\n" ) )
    (setq len (length txt))
    (if (elite-is-docked)
	(progn
	  (setq commanders-in-system (elite-ai-commander-in-system currentplanet))	  
	  (while commanders-in-system
	    (setq commander (car commanders-in-system))
	    (setq txt (concat txt (elite-ai-commander-name commander) "\n"))
	    (setq commanders-in-system (cdr commanders-in-system))
	    )
	  (if (= len (length txt))
	      (setq txt (concat txt "No other commanders.\n"))
	      )
	  )
      (setq txt (concat txt "You must be docked.\n"))
      )
    (elite-insert-text txt)    
    )
  )

(defun elite-talk-to-commander ()
  "Talk to AI commander. Ask questions etc"
  (interactive)
  (let (
	(target)
	(topics)
	(commanders-in-system)
	(commander)
	(commander-completion)
	(sentence);start of dialog
	(i)
	(txt)
	)
    ;;(setq commanders-in-system (elite-ai-commander-in-system 79))
    (setq txt ">Talk to commander\n")
    (elite-insert-text txt)    
    (setq txt "")
    (setq commander-completion nil)
    (setq commanders-in-system (elite-ai-commander-in-system currentplanet))
    (if (elite-is-docked)
	(progn
	  (if (> (length commanders-in-system) 0)
	      (progn
		(setq i 1)
		(while commanders-in-system
		  (setq commander (elite-ai-commander-name (car commanders-in-system)))
		  (setq commander-completion (append commander-completion (list (list commander i))))
		  (setq commanders-in-system (cdr commanders-in-system))
		  (setq i (1+ i))
		  )
		
		(setq target (completing-read "Who do you wish to talk to: "  commander-completion nil t nil nil commander))
		(setq commanders-in-system (elite-ai-commander-in-system currentplanet))
		;;Topics for conversation
		(setq topics '(
			       ("Have you seen this person?" 1)
			       )
		      )
		(if (= (length commanders-in-system) 1)
		    (progn
		      ;;make child with other commander...
		      (if (= (commander-gender elite-commander) ELITE_GENDER_MALE)
			  (progn			    
			    (if (= (elite-ai-commander-gender (car commanders-in-system)) ELITE_GENDER_FEMALE)
				(progn
				  (setq topics (append topics (list (list "Would you like to have an offspring?" 2))))
				  )
			      )

			    )
			(progn
			    (if (= (elite-ai-commander-gender (car commanders-in-system)) ELITE_GENDER_MALE)
				(progn
				  (setq topics (append topics (list (list "Would you like to have an offspring?" 2))))
				  )
			      )
			  )
			)
		      )
		  )
		
		;(append topics (list (list " " 2)))
		(setq sentence (completing-read (concat "Say to " target ".. ")  topics  nil t))
		(if (equal sentence "Have you seen this person?")
		    (progn
		      (let (
			    (person-name)
			    (system)
			    )
			(setq person-name (read-string "Name of the person: "))
			(setq txt (concat txt elite-commander-name ": Have you seen " person-name "?\n")) 
			(setq txt (concat txt target ": "))
			;;has ai commander seen this person
			(setq system (elite-ai-commander-has-seen-person target person-name))

			(if system
			    (progn
			      (setq txt (concat txt "Yes I have. I saw that one in " (car system) " on day " (car (cdr system)) "."))
			      )
			  (progn
			      (setq txt (concat txt "I do not think that I have.."))
			    )
			  )
			)
		      )
		    )

		(if (equal sentence "Would you like to have an offspring?")
		    (progn
			(setq txt (concat txt elite-commander-name ": " "Would you like to have an offspring?\n"))
		      (if (> (elite-ai-commander-willingness-to-procreate (car commanders-in-system)) elite-ai-commander-willingness-to-procreate-threshold)
			  (progn
			    (setq txt (concat txt target ": Yes, I would. Let us proceed to the facilities....\n"))
			    ;;(elite-message-shell (elite-ai-commander-offspring-with-player (car commanders-in-system)) "\n")
			    (setq txt (concat txt (elite-ai-commander-offspring-with-player (car commanders-in-system)) "\n"))
			    )
			(progn
			  (setq txt (concat txt target ": Thank you, but not at this time... Perhaps later..\n"))
			  )
			)
		      )
		  )
		)
	    (progn
	      (setq txt (concat txt "No other commanders here.\n"))
	      )
	    )
	  )
      (progn
	(setq txt (concat txt "Must be docked.\n"))
	)
      )
    (elite-insert-text txt)
    )
  )

(defun elite-display-commander-info ()
  "Displays commander info."
  (interactive)
  (let (
	(commander-info "")
	(tmp)
	(i)
	(ranks)
	)
    ;(if (and (not (elite-is-docked)) (interactive-p))
	(setq commander-info (concat ">Commander " (commander-name elite-commander) "\n") )
	;)
    (setq commander-info 
	  (concat
	   commander-info
	   "Present System: "
	   (elite-get-system-name currentplanet)
	   "\n"
	   "Hyperspace System: "
	   (elite-get-system-name (commander-hyperspace-system elite-commander))
	   "\n"
	   "Condition: "
	   (commander-condition elite-commander)
	   "\n"
	   ))
;     (if (elite-is-docked)
; 	()
;       (progn
	(setq commander-info 
	      (concat 
	       commander-info 
	       (format "Cash: %.1f CR" (/ (commander-credits elite-commander) 10.0))
	       "\n"
	       (format "Fuel: %.1f Light Years" (/ (commander-fuel elite-commander) 10.0) )
	       "\n"
	       )
	      )	
; 	)
;       )
    (setq commander-info (concat commander-info "Legal Status: "))
    (setq tmp (commander-legal-status elite-commander))
    (if (= tmp 0)
	(setq commander-info (concat commander-info "Clean\n"))
      (progn
	(if (> tmp elite-offender-fugitive-threshold)
	    (setq commander-info (concat commander-info "Fugitive\n"))
	  (setq commander-info (concat commander-info "Offender\n"))
	  )
	))
    (setq commander-info (concat commander-info "Rating: "))
    (setq tmp (commander-elite-score elite-commander))
    (setq i 0)
    (while (< i elite-ranks)
      (if (>= tmp (elite-rank-score (aref elite-rank-rating i)))
	  (progn
	    (setq ranks (list ranks (elite-rank-title (aref elite-rank-rating i))))
	    ;(setq commander-info (concat commander-info (elite-rank-title (aref elite-rank-rating i)) "\n"))
	    )
	  )
      (setq i (1+ i))
      )
    (setq commander-info (concat commander-info (car (cdr ranks)) "\n"))
    
    ;;gender,marital status,etc.
    (setq commander-info (concat commander-info "Gender: "))
    (if (= (commander-gender elite-commander) ELITE_GENDER_MALE)
	(setq commander-info (concat commander-info "Male\n"))
      (setq commander-info (concat commander-info "Female\n"))
    )


    ;equipment here
    (setq commander-info (concat commander-info "Equipment:\n"))
    (if (commander-front-laser elite-commander)
	(setq commander-info (concat commander-info "Front " (elite-equipment-name (commander-front-laser elite-commander)) "\n" )))
    (if (commander-rear-laser elite-commander)
	(setq commander-info (concat commander-info "Rear " (elite-equipment-name (commander-rear-laser elite-commander)) "\n" )))
    (if (commander-right-laser elite-commander)
	(setq commander-info (concat commander-info "Right " (elite-equipment-name (commander-right-laser elite-commander)) "\n" )))
    (if (commander-left-laser elite-commander)
	(setq commander-info (concat commander-info "Left " (elite-equipment-name (commander-left-laser elite-commander)) "\n" )))
    (setq commander-info (concat commander-info "Missiles " (number-to-string (commander-missiles elite-commander)) "/4 "))
    (if (commander-fuel-scoops elite-commander)
	(setq commander-info (concat commander-info "Fuel Scoops\n")))
    (if (commander-ecm elite-commander)
	(setq commander-info (concat commander-info "E.C.M. ")))
    (if (commander-escape-pod elite-commander)
	(setq commander-info (concat commander-info "Escape Pod ")))
    (if (commander-energy-bomb elite-commander)
	(setq commander-info (concat commander-info "Energy Bomb\n")))
    (if (commander-energy-unit elite-commander)
	(setq commander-info (concat commander-info "Energy Unit ")))
    (if (commander-docking-computer elite-commander)
	(setq commander-info (concat commander-info "Docking Computers\n")))
    (if (commander-galactic-hyperdrive elite-commander)
	(setq commander-info (concat commander-info "Galactic Hyperdrive")))
	
    
    (elite-insert-text commander-info "commander-info")

    ;(if (interactive-p)
    (elite-list-commander-missions)
      ;)
    )
  )

(defun elite-display-inventory ()
  "Displays inventory."
  (interactive)
  (let (
	(msg)
	(i)
	(cargo)
	)
    ;(if (and (not (elite-is-docked)) (interactive-p))
	(setq msg ">Inventory\n")
	;)

    (setq i 0)
    (while (< i AlienItems)
      (setq cargo (aref shipshold i))
      (if (> cargo 0)
	  (progn 
	    (setq msg (concat msg  (aref tradnames i) " "  (number-to-string cargo) (elite-get-unit i) "\n"))
	  ))
      (setq i (1+ i))
      )

    (setq msg 
	  (concat
	   msg
	   ;(format "Cash: %.1f CR" (/ (commander-credits elite-commander) 10.0))
	   ;(format "\nFuel: %.1f Light Years" (/ (commander-fuel elite-commander) 10.0) )
	   "Cargo space: " (number-to-string holdspace) "t\n"
	   
	   )
	  )

;     (setq msg 
; 	  (concat
; 	   msg
; 	   ;large cargo bay indicator
; 	   ))

    ;(elite-command-shell msg)
    (elite-insert-text msg "inventory")
    )
  )

(defun elite-current-system-info ()
  "Print current system info."
  (interactive)
  (elite-insert-text (concat ">" (prisys (aref galaxy currentplanet))) "shell")
)

(defun elite-toggle-autofuel ()
  "Toggles autofueling option."
  (interactive)
  (if elite-autofuel
      (progn 
	(setq elite-autofuel nil)
	(elite-insert-text ">Autofuel off" "shell")
	)
    (progn 
	(setq elite-autofuel t)
	(elite-insert-text ">Autofuel on" "shell")
	)
    )
  )

(defun elite-toggle-autosave ()
  "Toggles autosave option."
  (interactive)
  (if elite-autosave
      (progn 
	(setq elite-autosave nil)
	(elite-insert-text ">Autosave off" "shell")
	)
    (progn 
	(setq elite-autosave t)
	(elite-insert-text ">Autosave on" "shell")
	)
    )
  )

(defun elite-system-info ()
  "Prints info on specfied system."
  (interactive)
  (let
      (
       (planet-name (completing-read "Info on system (name): " (append elite-planet-completion-list nil) nil t ""))       
       )
    (elite-insert-text (concat ">" (prisys (aref galaxy (matchsys planet-name)) nil t)) "shell")
  ))

(defun elite-local-systems ()
  "Lists systems within 7 light years."
  (interactive)
  (let (
	(msg)
	(syscount 0)
	(d)
	)
    (if (interactive-p)
	(setq msg ">Local Systems\n")
	)
    ;(setq msg (format "Galaxy number %i" galaxynum))

    (while (< syscount galsize)
      (setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
      (if (<= d maxfuel)
	  (progn 
; 	    (if (interactive-p)
; 		(setq msg (concat msg "."))
; 	      )
	    (if (<= d fuel)
		(setq msg (concat msg  "* "))
	      (setq msg (concat msg "- ")))
	    (setq msg (concat 
		       msg 
		       (elite-short-local-system-info syscount)
		       ;(prisys (aref galaxy syscount ) t)
		       (format " (%.1f LY)" (/ d 10.0) )
		       "\n"))
	    )
	)
	(setq syscount (1+ syscount))
      )
    (setq msg (concat msg (format "Fuel: %.1f Light Years" (/ (commander-fuel elite-commander) 10.0) )
))
    (elite-command-shell msg)
;;     (if (interactive-p)
;; 	(elite-insert-text msg "shell")
;;       (elite-insert-text msg "local-systems")
;;       )

    )
  )

(defun elite-undock ()
  "Undocks from station."
  (interactive)

  (if (elite-is-docked)
      (progn
	(let (
	      (i)
	      (msg)
	      )
	(elite-insert-text ">Leaving station..")
	(setf (commander-condition elite-commander) CONDITION_GREEN)
	(setq elite-position-in-universe 1)
	(elite-ui-space-initialize)
	;undock sequence
; 	(setq i 0)
; 	(setq msg ".")
; 	(while (< i 10)
; 	  (message msg)
; 	  (sit-for 0 200)
; 	  (setq msg (concat msg "."))
; 	  (setq i (1+ i))
; 	  )
	

	(elite-insert-position-indicator "Outside station")
	(elite-ui-refresh)

	(if elite-special-mission-1-refused
	    (progn
	      ;player was dumb enough to refuse navy mission
	      ;too bad...
	        (read-string "After undocking engines explode mysteriously..... Press enter to exit")
		(setq elite-dead-commander t)
		(kill-buffer elite-buffer-name)

	      )
	    )

	))
    (elite-insert-text "Not docked.")
    )

)

(defun elite-dock ()
  "Docks to station."
  (interactive)
  (let (
	(f)
	)
    (if (> elite-position-in-universe 1)
	(elite-insert-text "Can't dock, too far from the station.")
      (progn
	(if (elite-is-docked)
	    (elite-insert-text "Already docked.")
	  (progn
	    (setf (commander-condition elite-commander) CONDITION_DOCKED)
	    (setq elite-position-in-universe 0)
	    (elite-ui-station-initialize)
	    (elite-insert-text ">Docked")
	    (if elite-autofuel
		(progn
		  (setq f (elite-gamefuel (- maxfuel fuel))) 
		  (if (/= f 0)
		      (elite-insert-text (format "Bought %.1f LY fuel" (/ f 10.0)) "shell")
		    (elite-insert-text "Can't buy fuel." "shell")
		    )
		  )
	      )
	    ;;check illegal cargo
	    (let (
		  (tmp)
		  )
	      (setq tmp (elite-illegal-cargo))
	      (if (= tmp 0)
		  (progn
		    (setq tmp (commander-legal-status elite-commander))
		    (if (> tmp 0)
		      (setf (commander-legal-status elite-commander) (1- tmp))
		    )
		  )
	      (setf (commander-legal-status elite-commander) (+ (commander-legal-status elite-commander) tmp))
	      )
	    )
	  (if elite-autosave
	      (elite-save-commander))
	  
	  ;;missions
	  (elite-show-missions)
	  (elite-mission-complete)
	  
	  (elite-display-ai-commanders)
	  
	  (elite-federation-messages)

	  ;;do special mission 1, check start end etc.
	  (elite-do-special-mission-1)
	  
	  (elite-ui-refresh)
	  
	  )
	  )
	)
      )
    )
  )

(defun elite-select-hyperspace-system ()
  "Selects hyperspace system."
  (interactive)
  (let (
	(system-name)
	)
    (setq system-name (car (split-string (completing-read "Select hyperspace system: " (elite-local-systems-completion-list) nil t "") " ")))
    (setf (commander-hyperspace-system elite-commander) (matchsys system-name))
    ;(elite-ui-display-commander-info nil t)
    ;(elite-ui-display-commander-info txt)

    (elite-ui-refresh)
    )
  )


(defun elite-hyperspace-to ()
  "Hyperspaces to specified system."
  (interactive)
  (let (
	(d)
	(dest)
	(f)
	(i)
	(system)
	(systems-in-range)
	(completion-vector)
	)
    
    (setq system (commander-hyperspace-system elite-commander))
    (elite-insert-text (concat ">Hyperspace to " (elite-get-system-name system)) "shell")
    
    (if (elite-is-docked)
	(elite-insert-text "Undock first." "shell")
      (progn   
	(setq dest system)
	(if (= dest currentplanet)
	    (elite-insert-text "You are already there!" "shell")
	  (progn
	    (setq d (distance (aref galaxy dest) (aref galaxy currentplanet)))
	    (if (> d fuel)
		(elite-insert-text "Not enough fuel." "shell")
	      (progn
		(setq fuel (- fuel d))
		(elite-gamejump dest)
		(setq commander-visited-systems (append commander-visited-systems (list currentplanet)))

		(setq elite-day (ceiling (+ elite-day (/ d 10.0))))
		(setf (commander-current-day elite-commander) elite-day)
	    ;(elite-insert-text (stripout (prisys (aref galaxy currentplanet)) "\n") "shell")
		;(elite-insert-text (prisys (aref galaxy currentplanet)) "shell")
		;(elite-insert-text (concat "Current fuel: " (format "%.1f LY" (/ fuel 10.0))) "shell")

		;set position
		(setq elite-position-in-universe (+ 5 (random 10)))
; 		(elite-insert-text (concat 
; 				    (format "Cash:%.1f CR" (/ elite-player-cash 10.0))
; 				    " Current fuel: " (format "%.1f LY" (/ fuel 10.0))) "shell")
		(elite-insert-position-indicator (concat "Distance to station " (number-to-string elite-position-in-universe) "."))
		
		;clear mission list
		(setq elite-current-missions nil)
		;clear people with missions list
		(setq elite-people-with-missions nil)

		;ai commander activity
		;;end ai commander activity before starting new rounf
		(elite-ai-commander-end-activity)
		;start ai commander activity
		(elite-ai-commander-start-activity)
		
		;;(elite-ai-commander-activity t)
		(message "")
		)
	      )
	    )
	  )
	(elite-ui-refresh)
	)
      )
    )
  )

(defun elite-display-equipment ()
  "Displays available equipment in current system."
  (interactive)
  (let (
	(available-equipment)
	(equip-text "")
	(tmp)
	(item)
      )
    (elite-insert-text ">Available equipment." "shell")
    (if (elite-is-docked)
	(progn
	  (setq available-equipment (split-string (elite-available-equipment) "\n"))
	  
	  (while available-equipment
	    (setq item (car available-equipment))
	    (setq tmp (substring item 2 (length item)))
	    (setq equip-text (concat  equip-text 
				      ;"."
				     (substring item 0 1)
				     " "
				     (elite-equipment-name tmp)
				     (format " %.1f CR\n" (/ (elite-equipment-price tmp) 10.0 ))
				     ))
	    (setq available-equipment (cdr available-equipment))
	    )
	  
	  (elite-insert-text equip-text "shell")
	  ;(elite-insert-text equip-text "equipment")
	  )
      (elite-insert-text "Not docked." "shell")
      )

    )
  )

(defun elite-buy-equipment ()
  "Buy equipment."
  (interactive)
  (let (
	(localequip)
	(completion-list (list))
	(completion-array)
	(array-length)
	(item)
	(itemtype)
	(default-completion-item)
	(i)
	(tmp)
	)
    (elite-insert-text ">Buying equipment" "shell")    
    (if (elite-is-docked)
	(progn
    (setq localequip (split-string (elite-available-equipment) "\n"))

    (setq array-length 0)
    (setq i 0)
    (while localequip
      (setq item (car localequip))
      (if (string= "*" (substring item 0 1))
	  (setq array-length (1+ array-length))
; 	  (setq completion-list (append (list completion-list)
; 					(list 					 
; 					 (substring item 2 (length item))
; 					 i)))
	  )	
      (setq localequip (cdr localequip))
      (setq i (1+ i))
      )
    (setq completion-array (make-vector array-length []))
    (setq localequip (split-string (elite-available-equipment) "\n"))
    (setq i 0)
    (while localequip
      (setq item (car localequip))
      (if (string= "*" (substring item 0 1))
	  (progn
	    (setq tmp (elite-equipment-name (substring item 2 (length item))))
	    (aset completion-array i (list tmp i))
	    (setq i (1+ i))
	  ))
      (setq localequip (cdr localequip))
      )



    (if (= array-length 0)
	(progn
	  (elite-insert-text "Can't buy any equipment." "shell")
	  )
      (progn
	(setq completion-list (list completion-list))
	(setq default-completion-item (car (aref completion-array 0)))
	;(setq default-completion-item (car (split-string (car (car (append completion-array nil))) " ")))
;	(setq item (completing-read (concat "Buy equipment (default " default-completion-item "): ") completion-list nil t nil nil default-completion-item))
	(setq item (completing-read (concat "Buy equipment (default " default-completion-item "): ") (append completion-array nil) nil t nil nil default-completion-item))
	;(setq item (car (split-string item " ")))
	(setq itemtype (elite-equipment-type item))
	;equip ship
	(if (string= itemtype "EQ_FUEL")
	    (elite-buy-fuel))
	
	(if (string= itemtype "EQ_MISSILE")
	    (progn
	      (let (
		    (no-of-missiles)
		    )
		(setq no-of-missiles (commander-missiles elite-commander))
		(if (< no-of-missiles 4)
		    (progn
		      ;buy missile
		      (setf (commander-missiles elite-commander) (1+ no-of-missiles))
		      (setq tmp (elite-equipment-price itemtype))
		      (elite-insert-text (concat 
					  "Bought missile "
					  (format "%.1f CR. " (/ tmp 10.0))
					  "Total missiles "
					  (number-to-string (commander-missiles elite-commander))
					  "/4."
					  ) 
					 "shell")
		      (setq elite-player-cash (- elite-player-cash tmp))
		      )
		  (progn
		    (elite-insert-text "Missile pylons full." "shell")
		    )
		  )
		)
	      )
	  )

	(if (string= itemtype "EQ_CARGO_BAY")
	    (progn
	      (setq holdspace (- 35 (elite-cargo)))
	      (setf (commander-cargo-capacity elite-commander) 35)
	      (setf (commander-large-cargo-bay elite-commander) t)
	      (elite-insert-text (concat
				  "Bought " 
				  (elite-equipment-name itemtype)
				  (format " %.1f CR" (/ (elite-equipment-price itemtype) 10.0))
				  )
				 "shell")
	      (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
	      ))

	(if (string= itemtype "EQ_PULSE_LASER")
	    (elite-buy-laser itemtype))
	(if (string= itemtype "EQ_BEAM_LASER")
	    (elite-buy-laser itemtype))
	(if (string= itemtype "EQ_MINING_LASER")
	    (elite-buy-laser itemtype))
 	(if (string= itemtype "EQ_MILITARY_LASER")
	    (elite-buy-laser itemtype))

 	(if (string= itemtype "EQ_ECM")
	    (progn
		(if (commander-ecm elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-ecm elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				 "shell")
		    ))
		  ))
	
 	(if (string= itemtype "EQ_FUEL_SCOOPS")
	    (progn
		(if (commander-fuel-scoops elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-fuel-scoops elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				       "shell")
		    ))
		  ))

 	(if (string= itemtype "EQ_ESCAPE_POD")
	    (progn
		(if (commander-escape-pod elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-escape-pod elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				       "shell")
		    ))
		  ))

 	(if (string= itemtype "EQ_ENERGY_BOMB")
	    (progn
		(if (commander-energy-bomb elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-energy-bomb elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				       "shell")
		    ))
		  ))

 	(if (string= itemtype "EQ_ENERGY_UNIT")
	    (progn
		(if (commander-energy-unit elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-energy-unit elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				       "shell")
		    ))
		  ))

 	(if (string= itemtype "EQ_DOCK_COMP")
	    (progn
		(if (commander-docking-computer elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-docking-computer elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				       "shell")
		    ))
		  ))

 	(if (string= itemtype "EQ_GAL_DRIVE")
	    (progn
		(if (commander-galactic-hyperdrive elite-commander)
		    (elite-insert-text (concat 
					"You already have "
					(elite-equipment-name itemtype)
					".")
				       "shell")
		  (progn
		    (setf (commander-galactic-hyperdrive elite-commander) t)
		    (setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))
		    (elite-insert-text (concat 
					"Bought "
					(elite-equipment-name itemtype)
					"."
					) 
				       "shell")
		    ))
		  ))
	)
      )
    (elite-ui-refresh)
    )
      (elite-insert-text "Not docked." "shell")
      )
    )
  )

(defun elite-buy-laser (itemtype)
  "Helper for buying laser"
  
  (let (
	(mounting)
	(current-laser)
	)
    (setq mounting (elite-select-mounting))
    
    
    (if (string= mounting "Front")
	(setq current-laser (commander-front-laser elite-commander)))
    (if (string= mounting "Rear")	      
	(setq current-laser (commander-rear-laser elite-commander)))
    (if (string= mounting "Right")
	(setq current-laser (commander-right-laser elite-commander)))
    (if (string= mounting "Left")	      
	(setq current-laser (commander-left-laser elite-commander)))

    (if (string= current-laser itemtype)
	(progn
	  (elite-insert-text (concat 
			      "You already have a "
			      (downcase mounting)
			      " "
			      (elite-equipment-name itemtype)
			      ".")
			     "shell")
	  )
      (progn
	(if current-laser
	    (progn
					;some laser available
					;remove it and put price to cahs
	      (setq elite-player-cash (+ elite-player-cash (elite-equipment-price current-laser)))
		    ))
	      ;add new laser type
	(if (string= mounting "Front")
	    (setf (commander-front-laser elite-commander) itemtype))
	(if (string= mounting "Rear")	      
	    (setf (commander-rear-laser elite-commander) itemtype))
	(if (string= mounting "Right")
	    (setf (commander-right-laser elite-commander) itemtype))
	(if (string= mounting "Left")	      
	    (setf (commander-left-laser elite-commander) itemtype))
			
					;deduct cash
	(setq elite-player-cash (- elite-player-cash (elite-equipment-price itemtype)))

	(elite-insert-text (concat 
			    "Bought "
			    (downcase mounting)
			    " "
			    (elite-equipment-name itemtype)
			    "."
			    ) 
			   "shell")

	))
					;check if something in mounting
					;if there is remove it and
    
    )
  )


(defun elite-select-mounting ()
  "Helper function for displaying select mounting question."
  (completing-read "Mounting (default Front): " (list (list "Front" 0) (list "Rear" 1) (list "Right" 2)(list "Left" 3)) nil t nil nil "Front")
  )

(defun elite-buy-goods ()
  "Buy goods."
  (interactive)
  (let (
	(good)
	(good-index)
	(amount)
	(bought)
	(i 0)
	(tradnames-length AlienItems)
	(completion-array (make-vector AlienItems ()))
	)
    (elite-insert-text ">Buying goods" "shell")
    (if (elite-is-docked)
	(progn
    (while (< i tradnames-length)
      (aset completion-array i (list 
				(concat 
				 (aref tradnames i) 
				 " "
				(format "%.1f" (/ (aref (markettype-price localmarket) i) 10.0))
				" CR/"
				(aref unitnames (tradegood-units (aref commodities i)))
				" Quantity: "
				(number-to-string (aref (markettype-quantity localmarket) i))
				(aref unitnames (tradegood-units (aref commodities i)))				
				)
				i))
      (setq i (1+ i))
      )

    (setq good (completing-read "Buy good (default Food): " (append completion-array nil) nil t nil nil (car (split-string (car (aref completion-array 0)) " "))))
    (setq good (car (split-string good " ")))
    (setq amount (read-string "Amount: " nil nil "0"))
    (setq amount (string-to-number amount))
    (if (= amount 0)
	(elite-insert-text "No deal." "shell")      
      (progn
	(setq good-index (elite-trade-good-index good))
	(setq bought (elite-gamebuy good-index amount))
	(if (= (+ bought holdspace) 0)
	    (elite-insert-text "Cargo bay full" "shell")
	  (elite-insert-text (concat
			      (format "Bought %i" bought)
			      (aref unitnames (tradegood-units (aref commodities good-index)))
			      " of "
			      (aref tradnames good-index)
			      (format " %.1f CR" (/ (* bought (aref (markettype-price localmarket) good-index)) 10.0))
			      ) "shell" ))

	))
    (elite-ui-refresh)
    )
      (elite-insert-text "Not docked." "shell")
      )
    ;(string-to-number amount)
  ))

(defun elite-sell-goods ()
  "Sell amount of good."
  (interactive)
  (let (
	(good1)
	(good)
	(amount)
	(i)
	(goods-in-hold " ")
	(completion-list)
	(tmp)
	(profit)
	)
    (elite-insert-text ">Sell goods" "shell")
    (if (elite-is-docked)
	(progn
    (setq i 0)
    (while (< i (length shipshold))
      (if (> (aref shipshold i) 0)
	  (progn
	    (setq goods-in-hold (concat  goods-in-hold " " (number-to-string i)))
	    )
      )
      (setq i (1+ i))
      )
    (setq goods-in-hold (cdr (split-string goods-in-hold " ")))
    (setq completion-list (make-vector (1+ (length goods-in-hold)) 0))
    (aset completion-list (1- (length completion-list)) (list "All" (1- (length completion-list))))
    (setq i 0)
    (while goods-in-hold
      (setq tmp (string-to-number (car goods-in-hold)))
      (aset completion-list i (list
			       (concat				
				(aref tradnames tmp)
				" "
				(format "%.1f" (/ (aref (markettype-price localmarket) tmp) 10.0))
				" CR/"
				(aref unitnames (tradegood-units (aref commodities tmp)))
				" Total in cargo: "
				(number-to-string (aref shipshold tmp))
				" "
				(aref unitnames (tradegood-units (aref commodities tmp)))
				)
			       1
			       ))
      (setq i (1+ i))
      (setq goods-in-hold (cdr goods-in-hold))
      )
    (if (= (length completion-list) 0)
	(elite-insert-text "Nothing to sell." "shell")
      (progn
	;sell all
	(setq good1 (completing-read "Sell good (default All): " (append completion-list nil) nil t nil nil "All"))
	;todo sell all 
	(setq good (car (split-string good1 " ") ))
	(if (string= good "All")
	    (progn
	      (setq i 0)
	      (while (< i (1- (length completion-list)))
		(setq good (car (split-string (car (aref completion-list i)) " ")))
		(setq profit (elite-gamesell (elite-trade-good-index good) 9999))
		(elite-insert-text 
		 (concat
		  "Sold "
		  (number-to-string (nth 0 profit))
		  (nth 1 profit)
		  " of "
		  good
		  " at price "
		  (format "%.1f CR" (/ (nth 2 profit) 10.0))
		  )
		 "shell"
		 )
		(setq i (1+ i))
		)
	      )
	  (progn
	    (setq amount (string-to-number (read-string "Amount: " nil nil "0")))
	    (if (= amount 0)
		(elite-insert-text 
		 (concat
		  "Can't sell nothing of "
		  good
		  "."
		  )
		 "shell")
	      (progn 
		(setq profit (elite-gamesell (elite-trade-good-index good) amount))
		(elite-insert-text 
		 (concat
		  "Sold "
		  (number-to-string (nth 0 profit))
		  (nth 1 profit)
		  " of "
		  good
		  " at price "
		  (format "%.1f CR" (/ (nth 2 profit) 10.0))
		  )
	     "shell"
	     )
		))
	    ))))
    (elite-ui-refresh)
    )
      (elite-insert-text "Not docked." "shell")
      )
    )
  )

(defun elite-buy-fuel ()
  "Convenience function to buy fuel."
  (interactive)
  (let (
	(f)
	)
    (if (elite-is-docked)
	(progn
	  (elite-insert-text ">Buying fuel" "shell")
	  (if (= (- maxfuel fuel) 0 )
	      (elite-insert-text "Fuel tanks already full." "shell")
	    (progn       
	      (setq f (elite-gamefuel (- maxfuel fuel)))
	      (if (= f 0)
		  (elite-insert-text "Can't buy any fuel" "shell")
		(elite-insert-text (format "Buying %.1fLY fuel %.1f CR" (/ f 10.0) (/ (* elite-fuelcost f) 10.0)) "shell")
		)
	      )
	    )
	  (elite-ui-refresh)
	  )
      (progn
	(if  (= elite-position-in-universe (1- elite-sun-position))
	    (progn
	      (if (commander-fuel-scoops elite-commander)
		  (progn
		     ;scoop fuel
		    (setf (commander-fuel elite-commander) maxfuel)
		    (elite-insert-text ">Scooping fuel." "shell")
		    (setq fuel maxfuel)
		    (elite-insert-text "Fuel tanks full." "shell")
		    )
		(elite-insert-text "You do not have fuel scoops." "shell")
		)
	      )
	   (elite-insert-text "Not docked and too far from sun." "shell")
	  )
	)
      )
    )
  )

(defun elite-jump-toward-planet ()
  "When in space jumps toward planet."
  (interactive)
  (let (
	(pos)
	(msg)
	)
    (if (> elite-position-in-universe 1)
	(progn
	  (setq pos (1- elite-position-in-universe))
	  (setq msg (concat "Flying towards planet. Distance " (number-to-string pos) "."))
	  ;read sensors
	  (elite-read-sensors pos)
	  (elite-random-encounter)
	  (if (> pos 2)
	      (elite-encounter))
; 	  (if (= pos 4)
; 	      (setq msg (concat msg " Two jumps to safe zone.")))
; 	  (if (= pos 3)
; 	      (setq msg (concat msg " A little further...")))
	  (if (= pos 2)
	      (setq msg (concat msg " Safe zone.")))
	  (if (= pos 1)
	      (setq msg (concat msg " Ready to dock.")))
	  (setq elite-position-in-universe pos)
	  )
      (setq msg "Can't go any further. Dock now.")
      )
    (if elite-dead-commander
	(kill-buffer elite-buffer-name)
      (progn
	(elite-insert-position-indicator msg)
	(elite-space-ui-refresh)
	)
      )
    )
  )

(defun elite-jump-toward-sun ()
  "When in space jumps toward sun."
  (interactive)
  (let (
	(pos)
	(msg)
	)
    (if (< elite-position-in-universe (1- elite-sun-position))
	(progn
	  (setq pos (1+ elite-position-in-universe))
	  (setq msg (concat "Flying towards sun. Distance " (number-to-string (- elite-sun-position pos)) "."))

	  ;read sensors
	  (elite-read-sensors pos)
	  (elite-random-encounter)
	  (if (> pos 2)
	      (elite-encounter))
; 	  (if (= pos (- elite-sun-position 4))
; 	      (setq msg (concat msg " It is getting hotter.")))
	  (if (= pos (- elite-sun-position 3))
	      (setq msg (concat msg " Feel the heat.")))
	  (if (= pos (- elite-sun-position 2))
	      (setq msg (concat msg " The heat is on!")))
	  (if (= pos (- elite-sun-position 1))
	      (setq msg (concat msg " Scoop fuel or leave.")))
	  (setq elite-position-in-universe pos)
	  )
;      (setq msg (concat (commander-ship-class elite-commander) " self-preservation controls took over. Self-destruction not allowed."))
      (setq msg "Self-destruction not allowed.")
      )
    (if elite-dead-commander
	(kill-buffer elite-buffer-name)
      (progn
	(elite-insert-position-indicator msg)
	(elite-space-ui-refresh)
	)
      )
    )
  )



(defun elite-galactic-jump ()
  "Jumps to next galaxy.
  Preserve planetnum (eg. if leave 7th planet arrive at 7th planet)"
  (interactive)
  (let (
	(do-jump)
	)
  ;todo equipment galactic drive
  (elite-insert-text ">Galactic hyperspace" "shell")
  (if (elite-is-docked)
	(elite-insert-text "Undock first." "shell")
    (progn
      (if elite-special-mission-1-naval-galactic-hyperdrive 
	  (progn
	    ;naval galactic hyperdrive
	    ;never 'expires', player can choose destination
	    (let (
		  (galnum)
		  )
	      (setq galnum (string-to-number (completing-read "Select galaxy: " (list (list "1" 0) (list "2" 1) (list "3" 2)(list "4" 3) (list "5" 4)(list "6" 5) (list "7" 6) (list "8" 7) ) nil t nil nil "Front")))
	      (if (= galnum galaxynum)
		  (elite-insert-text "You are already here.")
		(progn
		  ;jump to galaxy
		  (setq galaxynum galnum)
		  (elite-do-galactic-jump galnum)
		  )
		)

	      )
	    )
      (if (commander-galactic-hyperdrive elite-commander)
	  (progn 
	    ;jump to next galaxy
	    (setq galaxynum (1+ galaxynum))
	    (if (= galaxynum 9)
		(setq galaxynum 1))
	    (elite-do-galactic-jump galaxynum)

	    )
	(progn
      ;no galactic drive
	  (elite-insert-text "No galactic hyperdrive." "shell")
	  )
	)
      )

      )
    )
  )
)

(defun elite-do-galactic-jump (galnum)
  (let (

	)
    (elite-insert-text (concat "Jumping to galaxy " (number-to-string galnum) ".") "shell")
    (message "Sensors scanning galaxy...")
    (sit-for 1)
    (setq galaxy (aref elite-galaxies-in-universe (1- galnum)))
					;(buildgalaxy galaxynum)
    (elite-set-planet-completion-list)
    (message "Sensors scanning galaxy...done")
    
					;galactic hyperspace takes 10 days
    (setq elite-day (ceiling (+ elite-day 10)))
    (setf (commander-current-day elite-commander) elite-day)
					;set position
    (setq elite-position-in-universe (+ 5 (random 10)))
    (elite-insert-position-indicator (concat "Distance to station " (number-to-string elite-position-in-universe) "."))
					;clear mission list
    (setq elite-current-missions nil)
    
    (elite-space-ui-refresh)
    (setf (commander-galactic-hyperdrive elite-commander) nil)
    (setf (commander-current-galaxy elite-commander) galaxynum)
    
    (setq localmarket (genmarket (randbyte) (aref galaxy currentplanet)))
    
    )
  )  


(defun elite-list-galaxy ()
  "List all systems in galaxy"
  (interactive)
  (let (
	(msg)
	(d)
	(lyd)
	(syscount)
	(systems)
	(system-destinations (list))
	)
    (setq syscount 0)
    (setq systems (make-vector galsize 0))
    (while (< syscount galsize)
      (setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
      (setq lyd (/ d 10.0))
      (aset systems syscount lyd)
      (setq system-destinations (plist-put system-destinations lyd syscount))
      (setq syscount (1+ syscount))
      )
    ;(setq systems (vector 1 2 3 4))
    (setq systems (append systems nil))
    (setq systems (sort systems '<))
    (setq syscount 0)

    (while systems
      
      ;(setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
      (setq d (car systems))
      (setq syscount (plist-get system-destinations d))
      (setq msg (concat 
		 msg 
		 (elite-short-local-system-info syscount)
					;(prisys (aref galaxy syscount ) t)
		 ;(format " (%.1f LY)" (/ d 10.0) )
		 (format " (%.1f LY)" d )
		 (format " (%i,%i)" (plansys-x (aref galaxy syscount )) (plansys-y (aref galaxy syscount )))
		 "\n"))
      (setq systems (cdr systems))

      ;(setq syscount (1+ syscount))
      )
    (elite-insert-text (concat ">Systems in galaxy\n"
			       "Galaxy number "
			       (number-to-string galaxynum)
			       "\n"
			       msg)
		       )
    )
  )

(defun elite-path ()
  "Returns path from current system to another system."
  (interactive)
  (let 
      (
       (temp-currentplanet)
       (destination-name)
       (destination)
       (msg)
       (i)
       (syscount)
       (local-systems)
       (local-system-distances-to-destination)
       (temp1)
       (temp2)
       (d)
       (number-of-local-systems)
       (path)
       (completed)
       (exclude-systems);exclude systems that are visited
       (travel-time)
       )
    (setq completed nil)
    (setq path nil)
    (setq local-systems nil)
    (setq destination-name (completing-read "Destination: " (append elite-planet-completion-list nil) nil t ""))
    (setq destination (matchsys destination-name))
    (setq temp-currentplanet currentplanet)
    (setq path (append path (list currentplanet)))
    (setq exclude-systems (list))
    (while (not completed)
    
    (setq number-of-local-systems 0)
    ;local systems
    (setq syscount 0)
    (while (< syscount galsize)
      (setq d (distance (aref galaxy syscount ) (aref galaxy temp-currentplanet)))
      (if (and (> d 0) (not (member syscount exclude-systems)))
	  (if (<= d maxfuel)
	      (progn
		(setq local-systems (plist-put local-systems number-of-local-systems syscount))
		(setq number-of-local-systems (1+ number-of-local-systems))
		)
	    )
	(progn
	  ;exclude current systems
	  (setq exclude-systems (append exclude-systems (list syscount) nil))
	  )
	)
      (setq syscount (1+ syscount))
      )
    ;loop throuh local systems and jump to system which is closest to destination
    (setq local-system-distances-to-destination (make-vector number-of-local-systems 0))
    (setq i 0)
    (setq temp1 nil)
    (setq temp2 nil)
    (while (< i number-of-local-systems)
      (setq d (distance (aref galaxy (plist-get local-systems i)) (aref galaxy destination)))
      (if (= d 0)
	  (setq completed t))
      (setq temp1 (append temp1 (list d) nil))
      (setq temp2 (plist-put temp2 d (plist-get local-systems i)))
      (setq i (1+ i))
      )
    (setq temp1 (sort temp1 '<))
    (setq path (append path (list (plist-get temp2 (car temp1))) nil))
    (setq temp-currentplanet (plist-get temp2 (car temp1)))
    )
    
    ;print path
    (setq msg "")
    (setq travel-time 0)
    (setq temp-currentplanet currentplanet)
    (while path      
      (setq syscount (car path))
      
      (setq travel-time (+ travel-time (ceiling (/ (distance (aref galaxy syscount) (aref galaxy temp-currentplanet)) 10.0))))
      (setq temp-currentplanet syscount)

      (setq d (distance (aref galaxy syscount) (aref galaxy currentplanet)))
      (setq msg (concat 
		 msg 
		 (elite-short-local-system-info syscount)
					;(prisys (aref galaxy syscount ) t)
		 ;(format " (%.1f LY)" (/ d 10.0) )
		 (format " (%.1f LY)" (/ d 10.0))
		 (format " (%i,%i)" (plansys-x (aref galaxy syscount )) (plansys-y (aref galaxy syscount )))
		 "\n"))
      (setq path (cdr path))
      )
    (setq msg (concat 
	       ">Path from "
	       (elite-get-system-name currentplanet)
	       " to "
	       destination-name
	       "\n"
	       msg
	       )
	  )
    (setq msg (concat msg 
		      "Total travel time: "
		      (number-to-string travel-time)
		      " days."))
    
;;     (with-electric-help
;;      '(lambda () (insert msg) (goto-char 0) "*Elite Path*")
;;      )
    (elite-insert-text msg)
    ;(message (concat "dx: " dx " dy: " dy))
    )
  )

(defun elite-galaxy-map ()
  "Shows galaxy map."
;  (interactive)
  (let (
	(buffer)
	(i)
	(planet)
	(current-planet)
	)
    (setq buffer (get-buffer-create "*Elite Galaxy Map*"))
    (set-buffer buffer)
    (erase-buffer);just in case
    (setq i 0)
    (setq current-planet (aref galaxy currentplanet))
    ;goto middle of screen
    (while (< i galsize)
      (setq planet (aref galaxy i))
      
      (setq i (1+ i))
      )
    )
  )

;additional commands

(defvar elite-additional-commands nil)
;(defvar elite-additional-commands (list (list "people" 1)  ))

(defun elite-additional-command ()
  "Function for entering additional commands. Used to extends
   commands for example for special missions."
  (interactive)
  (let (
	(cmd)
	)
    ;(setq destination-name (completing-read "Destination: " (append elite-planet-completion-list nil) nil t ""))
    (setq cmd (completing-read "Command: " (append elite-additional-commands nil) nil t ""))
    
    (if (string= cmd "mission-1")
	(elite-special-mission-1-status-screen)
	)

    (if (string= cmd "elite-federation-pilots")
	(elite-ai-commander-info)
	)

    )
  )

;functions and variables for loading/saving commander

(defvar elite-commanders nil
  "All elite commanders")

(defvar elite-commander-missions nil
  "elite commander missions")

;(setq  elite-commanders nil)

(defun elite-set-commander-list()
  "inititializes commander property list"
  (let (
	(buffer)
	)
;    (setq elite-commanders (list))
    (setq buffer (find-file elite-commander-save-file)) 
    (eval-buffer buffer)
    (kill-buffer buffer)
    )
  )

(defun elite-commander-name-value (name)
  "Returns sum of ascii codes in name"
  (let (
      (tmp)
      (value)
      (i)
      )
    (setq value 0)
    (setq i 0)
    (setq tmp (append name nil))
    (while tmp
      (setq value (+ value (* i (car tmp))))
      (setq tmp (cdr tmp))
      (setq i (1+ i))
      )
    (max value)
    )
  )

(defvar elite-galaxy-in-saved-file nil
  "Saved galaxy number in save file.")

(defun elite-save-commander ()
  "Saves commander. Only when docked."
  (interactive)
  (let (
	(tmp)
	(value)
	)
  (if (elite-is-docked)
      (progn
	
	;;end ai commander activity before saving
	(elite-ai-commander-end-activity t)

	;save commander
	(find-file elite-commander-save-file)
	(erase-buffer)
	
 	(insert "(setq elite-commanders '")
 	(prin1 (plist-put elite-commanders (elite-commander-name-value elite-commander-name) elite-commander) (current-buffer))
 	(insert ")\n")
	(insert "(setq elite-commander-missions '")
	(prin1 (plist-put elite-commander-missions (elite-commander-name-value elite-commander-name) elite-current-missions) (current-buffer))
	(insert ")\n")
	(insert "(setq elite-accepted-missions '")
	(if elite-accepted-missions
	    (prin1 elite-accepted-missions (current-buffer))
	  (insert "()")
	  )
	(insert ")\n")
	
	(save-buffer)
	(kill-buffer (current-buffer))

	;save commander specific data to another file
	(find-file (concat elite-commander-save-directory "/.elite-commander_" elite-commander-name))
	(erase-buffer)

	;set galaxy systems and completion list
; 	(insert "(setq elite-galaxies-in-universe '")
; 	(prin1 elite-galaxies-in-universe (current-buffer))
; 	(insert ")\n")
	
; 	(insert "(setq elite-galaxy-in-saved-file '")
; 	(prin1 galaxynum (current-buffer))
; 	(insert ")\n")
; 	(insert "(setq galaxy '")
; 	(prin1 galaxy (current-buffer))
; 	(insert ")\n")


; 	(insert "(setq elite-planet-completion-list '")
; 	(prin1 elite-planet-completion-list (current-buffer))
; 	(insert ")\n")

	;set autofuel and autosave
	(insert "(setq elite-autosave '")
	(prin1 elite-autosave (current-buffer))
	(insert ")\n")
	(insert "(setq elite-autofuel '")
	(prin1 elite-autofuel (current-buffer))
	(insert ")\n")

	;insert people in universe
	(insert "(setq elite-people-names '")
	(prin1 elite-people-names (current-buffer))
	(insert ")\n")
	(insert "(setq elite-people-in-universe '")
	(prin1 elite-people-in-universe (current-buffer))
	(insert ")\n")
	;people who are out of the loop
	(insert "(setq elite-people-out-of-loop '")
	(prin1 elite-people-out-of-loop (current-buffer))
	(insert ")\n")

	;save special mission 1 info
	(insert "(setq elite-special-mission-1-accepted '")
	(prin1 elite-special-mission-1-accepted (current-buffer))
	(insert ")\n")
	(insert "(setq elite-special-mission-1-completed '")
	(prin1 elite-special-mission-1-completed (current-buffer))
	(insert ")\n")
	(insert "(setq elite-special-mission-1-traitors '")
	(prin1 elite-special-mission-1-traitors (current-buffer))
	(insert ")\n")
	(insert "(setq elite-special-mission-1-traitor-status '")
	(prin1 elite-special-mission-1-traitor-status (current-buffer))
	(insert ")\n")
	(insert "(setq elite-special-mission-1-log-entries '")
	(prin1 elite-special-mission-1-log-entries (current-buffer))
	(insert ")\n")
	(insert "(setq elite-special-mission-1-naval-galactic-hyperdrive '")
	(prin1 elite-special-mission-1-naval-galactic-hyperdrive  (current-buffer))
	(insert ")\n")

	;save additional commands
	(insert "(setq elite-additional-commands '")
	(prin1 elite-additional-commands (current-buffer))
	(insert ")\n")

	;;save ai commander info
	(insert "(setq elite-ai-commander-list '")
	(prin1 elite-ai-commander-list (current-buffer))
	(insert ")\n")
	
	;;save new ai commander list
	(insert "(setq elite-federation-commander-born '")
	(prin1 elite-federation-commander-born (current-buffer))
	(insert ")\n")
	

	;;save ai commander path (where ai commander roams)
	(insert "(setq elite-ai-commander-path '")
	(prin1 elite-ai-commander-path (current-buffer))
	(insert ")\n")

	;;save player path
	(insert "(setq commander-visited-systems '")
	(prin1 commander-visited-systems (current-buffer))
	(insert ")\n")



	(save-buffer)
	(kill-buffer (current-buffer))



	(message (concat "Commander " (commander-name elite-commander) " saved"))	
	)
    (message "Can not save. You must be docked.")
    )
  )
  )

(defun elite-load-commander (name)
  "Loads commander. return t if name is not valid commander name and new 
   commander should be created"
  (let (
	(buffer)
	(tmp)
	(tmp2)
	)
    ;(message (concat "Loading commander " name "..."))
    
;      (setq buffer (find-file elite-commander-save-file))
;      (elite-set-commander-list)
;      (eval-buffer buffer)
;    (setplist 'elite-commanders '("Sami" "test"))
;    (setq elite-commanders (plist-put elite-commanders elite-commander-name elite-commander))
;    (setq elite-commanders (plist-put elite-commanders "Sami" elite-commander))
    (setq tmp (plist-get elite-commanders (elite-commander-name-value name)))
    (if (not (null tmp))
	(progn
	  (setq tmp2 nil)
	  (setq elite-commander tmp)
	  (setq elite-current-missions (plist-get elite-commander-missions (elite-commander-name-value name)))

	  ;load commander specific data
	  (let (
		(buffer)
		)
	
	    (setq buffer (find-file (concat elite-commander-save-directory "/.elite-commander_" name))) 
	    (eval-buffer buffer)
	    (kill-buffer buffer)
	    )

	  )
      (setq tmp2 t)
      )

    (not (not tmp2));return value

;    (kill-buffer buffer) ;(append "Sami" nil)

    ;(message "Loading commander...done")
    )
  )

(defun elite-quit ()
  "Quits elite"
  (interactive)
  (setq cursor-in-echo-area nil)
  (if (not elite-dead-commander)
      (if (elite-is-docked)
	  (if (y-or-n-p "Exiting.. Save Commander? ")
	      (elite-save-commander)
	    )
	(progn
	  (read-string "Not docked. Can't save Commander. Press C-g to go back to game. Enter to exit.")
	  ))
    )
  
  (if (bufferp (get-buffer elite-special-mission-1-status-buffer-name))
      (kill-buffer elite-special-mission-1-status-buffer-name)
    )
  
  (if (and elite-space-buffer (bufferp (get-buffer elite-space-buffer)))
      (kill-buffer elite-space-buffer)
    )
  (if (and elite-space-message-buffer (bufferp (get-buffer elite-space-message-buffer)))
      (kill-buffer elite-space-message-buffer)
    )
  (if (and elite-space-widget-buffer (bufferp (get-buffer elite-space-widget-buffer)))
      (kill-buffer elite-space-widget-buffer)
    )
  (if (and elite-space-sensor-buffer (bufferp (get-buffer elite-space-sensor-buffer)))
      (kill-buffer elite-space-sensor-buffer)
    )
  
  (if (and elite-station-message-buffer (bufferp (get-buffer elite-station-message-buffer)))
      (kill-buffer elite-station-message-buffer)
    )
  (if (and elite-market-buffer (bufferp (get-buffer elite-market-buffer)))
      (kill-buffer elite-market-buffer)
    )
  (if (and elite-widget-buffer (bufferp (get-buffer elite-widget-buffer)))
      (kill-buffer elite-widget-buffer)
    )
  
  (if (not (one-window-p))
      (delete-other-windows)
    )
  (kill-buffer (get-buffer elite-buffer-name))
  
  (set-frame-height (selected-frame) elite-original-screen-height)
  (set-frame-width (selected-frame) elite-original-screen-width)
;;   (set-cursor-color elite-original-cursor-color)
;;   (set-foreground-color elite-original-foreground-color)
;;   (set-background-color elite-original-background-color)
					;(set-frame-name elite-original-frame-title)
					;(set-cursor-color "cyan")
  (scroll-bar-mode (quote elite-original-scroll-bar-mode))
  (setq cursor-in-non-selected-windows elite-original-cursor-in-non-selected-windows)

  
  (message "")

)

(provide 'elite-commands)
