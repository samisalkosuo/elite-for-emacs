;;; elite-combat.el -  Functions for handling combat and other
;                      encounters in space

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


(defconst   FLG_PULSE_LASER      1)
(defconst   FLG_BEAM_LASER     2 )
(defconst   FLG_MILITARY_LASER   4)
(defconst   FLG_ANGRY       8)
(defconst   FLG_ECM     16)
(defconst   FLG_HOSTILE     32);todo categories, pirate,bounty hunter,military,police,etc
(defconst   FLG_TRADER     64)
(defconst   FLG_FRIENDLY     128);add functionality, other ships send messages
(defconst   FLG_PREACHER     256)

;enemy actions
(defconst ENEMY_LASER 1)
(defconst ENEMY_EVASIVE 2)
(defconst ENEMY_NOTHING 3)
(defconst ENEMY_MISSILE 4)

(defstruct laser
  type
  energy;output energy from single shot
)

(defvar elite-lasers
  (vector
   (make-laser
    :type "EQ_PULSE_LASER"
    :energy 10)
   (make-laser
    :type "EQ_BEAM_LASER"
    :energy 20)
   (make-laser
    :type "EQ_MINING_LASER"
    :energy 10)
   (make-laser
    :type "EQ_MILITARY_LASER"
    :energy 30)
   )
  "Elite laser energies."
)

(defun elite-get-laser-energy (type)
  "Returns laser output energy."
  (let (
	(i)
	(laser)
	(energy)
	)
    (setq i 0)
    (while (< i 4)
      (setq laser (aref elite-lasers i))
      (if (string= (laser-type laser) type)
	  (setq energy (laser-energy laser)))
      (setq i (1+ i))
      )
    )
  )


(defstruct elite-univ-object
  ;universe object ship etc. and its properties
  type;this is type elite-ship
  flags
  target;objects target 0=player
  bravery;how good is this object in combat
  profession
)

(defvar elite-universe (make-vector 1 [])
  "Elite universe.Array of objects when player is in space.
   maximum 1 objects in univers")

(defun elite-encounter ()
  "Encounter in space..."
  ;see elite new kind swat.c check_for_other, random_encounter
  (let (
	(gov)
	(rnd)
	(object)
	(shiprnd 0)
	(i)
	(flags 0)
	(bravery)
	(profession)
	(type)
	)
    (setq gov (plansys-govtype (aref galaxy currentplanet)))
    (setq rnd (elite-rand1 t))
    (if (and  (/= gov 0) (or (>= rnd 90) (< (logand rnd 7) gov))  )
	()
      (progn
	;encounter
; 			universe[newship].flags = FLG_ANGRY;
; 			if ((rand255() > 200) || (type == SHIP_CONSTRICTOR))
; 				universe[newship].flags |= FLG_HAS_ECM;
; 		
; 			universe[newship].bravery = ((rand255() * 2) | 64) & 127;
	;encountered ship      
	(setq type (aref elite-ships (random (length elite-ships))))
	(setq profession (elite-get-ship-profession (elite-ship-class type)))
	(if (= profession SHIP_PIRATE)
	    (progn 
	      ;pirate is angry, todo logic for angru not angry
	      ;piratre is hostile, todo logic for hostile not hostile
	      (setq flags (logior flags FLG_HOSTILE))
	      (setq flags (logior flags FLG_ANGRY))
	      )
	  )

	(if (= profession SHIP_TRADER)
	    (progn 
	      ;trader is angry if rand is over 200
	      (if (> (elite-rand1 t) 200)
		  (setq flags (logior flags FLG_ANGRY))
		(setq flags (logior flags FLG_FRIENDLY))
		)
	      )
	  )

	;set flags
	
	;enemy laser
	(setq rnd (elite-rand1 t))
	(if (> rnd 240)
	    (setq flags (logior flags FLG_MILITARY_LASER))
	  (progn
	    (if (> rnd 220)
		(setq flags (logior flags FLG_BEAM_LASER))
	      (setq flags (logior flags FLG_PULSE_LASER))
	      )
	    )
	  )
	(if (> (elite-rand1 t) 200)
	    (setq flags (logior flags FLG_ECM))
	    )

	;TODO:set bravery
	;(setq bravery (logand (logior (* 2 (elite-rand1 t)) 64) 127))
	;bravery indicates behaviour, larger the the number more agressive
	;enemy is==> fires more lasers
	;value is 0-255
	(setq bravery (elite-rand1 t))
	(setq object (elite-make-univ-object type flags 0 bravery profession))
	(aset elite-universe 0 object)

	(elite-combat)

	)
      )
    )
  )



(defun elite-make-univ-object (type flags target bravery profession)
  "Makes elite universe object"
  (make-elite-univ-object
   :type type
   :flags flags
   :target target
   :bravery bravery
   :profession profession
   )
)


(defun elite-combat ()
  "Function for combating other ships"
  (let (
	(msg)
	(i)
	(object)
	(type)
	(name)
	(actions)
	(action)
	(enemy-energy)
	(my-energy)
	(bravery)
	(enemy-action)
	(flags)
	(target-missile)
	(missile-fired)
	(missile-locked 0)
	(tmp)
	(cargo-canisters)
	)
    ;use flags to get enemy status etc see wap elite combat
    ;user interface example 
    ;Enemy ship
    ;f fight
    ;c continue
    ;f==>
    ;f fire laser
    ;target missile
    ;ecm
    ;evasive maneuvers
    ;nothing

    ;enemy action
    ;evasive,(if missiles target missilem ,fire missile),fire laser
    ;set weight to actions based on bravery, larger the value more likely
    ;enemy fires laser
    ;random action from possible actions
    ;

    (setq msg ">Ship approaching ")

    ;only one ship at a time
    (setq object (aref elite-universe 0))
    (setq type (elite-univ-object-type object))
    (setq msg (concat msg (elite-ship-class type)))
    (setq flags (elite-univ-object-flags object))

    ;show ship in screen
    (elite-space-ui-enemy-ship (elite-ship-class type))
    
    ;if hostile,condition RED
    (if (/= 0 (logand FLG_HOSTILE flags))
	(progn	
	  (setf (commander-condition elite-commander) CONDITION_RED)
	  (setq msg (concat msg " Condition " (upcase (commander-condition elite-commander))))
	  )
      (progn
	(if (= (elite-univ-object-profession object) SHIP_ASTEROID)
	    (setf (commander-condition elite-commander) CONDITION_GREEN)
	  (setf (commander-condition elite-commander) CONDITION_YELLOW)
	  )
	(setq msg (concat msg " Condition " (upcase (commander-condition elite-commander))))
	)
      )
    (if (= (elite-univ-object-profession object) SHIP_TRADER)
	(if (/= (logand FLG_ANGRY flags) 0)
	    (setq msg (concat msg "\n..Angry trader"))
	  (setq msg (concat msg "\n..Friendly trader"))
	  )
      )

    (elite-insert-text msg)

    ;(elite-mode-set-space-mode-line)
    (goto-char (point-max))

    ;completing read with combat options
    (if (or (string= (commander-condition elite-commander) CONDITION_RED) (y-or-n-p "Fight? "))
	(progn
	  (let (
		(output)
		(enemy-output)
		(msg)
		(cargo-canisters);cargo canisters left when enemy destroyed
		)
	    ;fight
	    (elite-insert-text "Combat..")
	    (setq enemy-energy (elite-ship-energy type))
	    (setq bravery (elite-univ-object-bravery object))
	    (setq my-energy (elite-ship-energy (commander-ship-class elite-commander)))

	    (setq target-missile nil)
	    (while (and (> my-energy 0) (> enemy-energy 0))
	      
	      (setq msg "")
	      (if target-missile
		  ;lock missile
		  (progn
		    (setq missile-locked (1+ missile-locked))
		    )
		  )
	    ;enemy actions
	    ;laser
	    ;evasive
	    ;(if (and (/= (logand FLG_HOSTILE flags) 0) (/= (logand FLG_ANGRY flags) 0) (> (elite-rand1 t) 127))	    
	      
	      (setq enemy-action (elite-combat-enemy-action flags))

	      ;(elite-enemy-laser (elite-univ-object-flags object))

	    ;set completion string
	      (if target-missile
		  (progn
		    (if (= missile-locked 2)
			(progn
			  (elite-insert-text "Missile locked")
			  (setq actions (list (list "Fire Laser" 0) (list "Evasive Maneuvers" 1) (list "Fire missile" 3) (list "Nothing" 4)))
			  )
		      (progn
			(setq actions (list (list "Fire Laser" 0) (list "Evasive Maneuvers" 1) (list "Target missile" 3) (list "Nothing" 4)))
			)
		      )		    
		  )
		(progn
		  (if (> (commander-missiles elite-commander) 0)
		      (setq actions (list (list "Fire Laser" 0) (list "Evasive Maneuvers" 1) (list "Target missile" 3) (list "Nothing" 4)))
		    (setq actions (list (list "Fire Laser" 0) (list "Evasive Maneuvers" 1) (list "Nothing" 3)))
		    )
		  )
		)
	      (setq action (completing-read "Action: "  actions nil t))

	      ;;ai activity
	      (elite-ai-commander-continue-activity)
	      
	      (if (string= action "Fire Laser")
		  (progn
		    (setq output (elite-fire-laser))
		    (setq msg ">Firing laser...")
		    ;if enemy evades possibility to miss
		    (if (= enemy-action ENEMY_EVASIVE)
			(progn
			  (if (> (elite-rand1 t) 220)
			      (progn
				;enemy hit
				(setq enemy-energy (- enemy-energy output))		    
				(if (<= enemy-energy 0)
				    (progn
				      (setq msg (concat msg "Enemy destroyed."))
				      (setq tmp (elite-enemy-destroyed (elite-ship-class type)))
				      (if tmp
					  (elite-space-message-shell tmp)
					)
				      ;(setq msg (concat msg (elite-enemy-destroyed (elite-ship-class type))))
				      )
				  (setq msg (concat msg "Enemy hit. Energy " (number-to-string enemy-energy) "/" (number-to-string (elite-ship-energy type)) "."))
				  )
				)
			    (progn
			      ;enemy evades
			      (setq msg (concat msg "Missed."))			      
			      )
			    )
			  )
		      (progn
			(setq enemy-energy (- enemy-energy output))		    
			(if (<= enemy-energy 0)
			    (progn
			      (setq msg (concat msg "Enemy destroyed."))
			      (setq tmp (elite-enemy-destroyed (elite-ship-class type)))
			      (if tmp
				  (elite-space-message-shell tmp)
				)
			      ;(setq msg (concat msg (elite-enemy-destroyed (elite-ship-class type))))
			      ;(elite-enemy-destroyed (elite-ship-class type))
			      )
			  (setq msg (concat msg "Enemy hit. Energy " (number-to-string enemy-energy) "/" (number-to-string (elite-ship-energy type)) "."))
			  )
			)
		      )
		    )
		)

	      (if (string= action "Evasive Maneuvers")
		  (progn
		    (setq msg (concat msg ">Evasive maneuvers.."))

		    )
		)

	      (if (string= action "Target missile")
		  (progn
		    (setq target-missile t)
		    (setq msg (concat msg ">Targeting missile.."))
		    )
		  )
	      
	      (if (string= action "Fire missile")
		  (progn
		    (setq target-missile nil)
		    (setq missile-locked 0)
		    (setf (commander-missiles elite-commander) (1- (commander-missiles elite-commander)))
		    ;fire missile
		    (setq msg (concat msg ">Firing missile.."))
		    ;check ecm
		    (if (/=  (logand FLG_ECM flags) 0)
			(progn
			  (setq msg (concat msg "\nEnemy has ECM. Missile destroyed."))
			  )
		      (progn
			(setq enemy-energy 0)
			(setq msg (concat msg (elite-enemy-destroyed (elite-ship-class type))))
			;(elite-enemy-destroyed (elite-ship-class type))
			(setq msg (concat msg "\nEnemy destroyed."))
			
			)
		      )

		    )
		  )

	      (if (or (string= action "Nothing") (string= action ""))
		  (progn
		    (setq msg (concat msg ">Do nothing.."))
		    (if (< my-energy (elite-ship-energy (commander-ship-class elite-commander)))
			(progn
			  (if (commander-energy-unit elite-commander)
			      (progn
				(setq my-energy (+ my-energy 20))
				)
			    (progn
			      (setq my-energy (+ my-energy 10))
			      )
			    )
			  (if (> my-energy (elite-ship-energy (commander-ship-class elite-commander)))
			      (setq my-energy (elite-ship-energy (commander-ship-class elite-commander)))
			    )
			  (setq msg (concat msg " Energy " (number-to-string my-energy) "/" (number-to-string (elite-ship-energy (commander-ship-class elite-commander))) "."))

			  )
		      )
		    )
		)
	      
	      (if (> enemy-energy 0)
		(progn
		  (if (= enemy-action ENEMY_LASER)
		      (progn
			(setq msg (concat msg "\nEnemy fires laser..."))
			(setq enemy-output (elite-enemy-laser flags))
					;if my action evade, enemy misses
			(if (string= action "Evasive Maneuvers")
			    (progn
			      (if (> (elite-rand1 t) 222);lower the number more probable that enemy hits
				  (progn
				    (setq my-energy (- my-energy enemy-output))
				    (setq msg (concat msg "Hit by enemy.. Energy " (number-to-string my-energy) "/" (number-to-string (elite-ship-energy (commander-ship-class elite-commander))) "."))
				    )
				(progn
				  (setq msg (concat msg "Enemy misses.."))
				  )
				)
			    )
			  (progn
			    (if (> (elite-rand1 t) 100);lower the number more probable that enemy hits
				(progn
				  (setq my-energy (- my-energy enemy-output))
				  (setq msg (concat msg "Hit by enemy.. Energy " (number-to-string my-energy) "/" (number-to-string (elite-ship-energy (commander-ship-class elite-commander))) "."))
				  )
			      (progn
				(setq msg (concat msg "Enemy misses.."))
				)
			      )
			    )
			  )
			
			;enemy hits
		      ;(if (> (elite-rand1 t) 200)

			)
		    )
		  (if (= enemy-action ENEMY_EVASIVE)
		      (setq msg (concat msg " Enemy evades.."))
		    )
		  (if (= enemy-action ENEMY_NOTHING)
		      (progn
			(if (< enemy-energy (elite-ship-energy type))
			    (setq enemy-energy (+ enemy-energy 10))
			  )
			(setq msg (concat msg " Enemy does nothing. Energy " (number-to-string enemy-energy) "/" (number-to-string (elite-ship-energy type)) "."))
			)
		    )
		  )
		)
	      (elite-insert-text msg)
	    	      
	      )

	    (if (<= my-energy 0)
		(progn
		  (elite-destruction)		    
		  )
	      )
	    )

	  ;clear screen
	  (elite-space-ui-refresh)

	  ;scoop cargo
	  (setq cargo-canisters (ceiling (/ (elite-ship-hull-mass type) 10.0)))
	  (if (or (= (elite-univ-object-profession object) SHIP_TRADER) (and (> (elite-rand1 t) 80) (> cargo-canisters 0)))
	      (progn
		(let
		    (
		     (content)
		     (i)
		     )
		    (setq content (make-vector cargo-canisters 0))
		    (setq i 0)
		    (while (< i cargo-canisters)
		      (aset content i (random (- lasttrade 3)))
		      (setq i (1+ i))
		      )
		    (elite-insert-text (concat (number-to-string cargo-canisters) " cargo canisters"))
		    
		    (if (> holdspace 0)
			(progn
			  (if (commander-fuel-scoops elite-commander)
			      (progn
				(if (y-or-n-p "Scoop cargo? ")
				    (progn
				      (setq i 0)
				      (while (and (> holdspace 0) (< i cargo-canisters))
					(aset shipshold (aref content i) (1+ (aref shipshold (aref content i))))
					(elite-insert-text (concat "1 tonne of " (aref tradnames (aref content i)) " scooped." ))
					(if (string= (elite-get-unit (aref content i)) "t")
					    (setq holdspace (1- holdspace))				      
					  )
					;scoop all cargo
					(setq i (1+ i))
					)				
				      (setf (commander-current-cargo elite-commander) shipshold)
				      (setf (commander-cargo-capacity elite-commander) holdspace)
				      (message "")
				      )
				  )
				)
			    (elite-insert-text "No fuel scoops. Can't scoop cargo.")
			    )
			  )
		      (elite-insert-text "Cargo bay full. Can't scoop cargo.")
		      )
		    )
		)
	    )
	  )
      (progn
	;no fight
	)
      )
    (setf (commander-condition elite-commander) CONDITION_GREEN)
    )
  )

(defun elite-enemy-destroyed (ship-class)
  "Enemy destroyed in combat"
  (let (
	(txt nil)
	)
    (setf (commander-elite-score elite-commander) (1+ (commander-elite-score elite-commander)))
  ;after competent status elite federation congratulates
    (if (= (commander-elite-score elite-commander) 128)
	(setq txt (concat
			  ">Message from the Elite Federation of Pilots:"
			  "\nCongratulations! You just earned the rank Competent."
			  )
	      )
	 )
    (if (= (commander-elite-score elite-commander) 1024)
	(setq txt (concat 
			  ">Message from the Elite Federation of Pilots:"
			  "\nCongratulations! You just earned the rank Dangerous."
			  )
	      )
	 )
    (if (= (commander-elite-score elite-commander) 4096)
	(setq txt (concat 
			  ">Message from the Elite Federation of Pilots:"
			  "\nCongratulations! You just earned the rank Deadly."
			  )
	      )
	 )
    (if (= (commander-elite-score elite-commander) 8192)
	(setq txt (concat 
			  ">Message from the Elite Federation of Pilots:"
			  "\nCongratulations! You just earned the rank ELITE."
			  )
	      )
	 )

    
      (if (= (logand (commander-elite-score elite-commander) 255) 0)
	  (setq txt ">Right On Commander!")
	)
      (elite-enemy-destroyed-animation ship-class)      
      txt
      )
    )
  

(defun elite-fire-laser ()
  "Fires laser. Return laser energy output"
  (let ( 
      (i)
      (my-laser)
      (laser)
      (output)
      )
    (setq i 0)
    (while (< i (length elite-lasers))
      (setq my-laser (commander-front-laser elite-commander))
      (setq laser (laser-type (aref elite-lasers i)))
      (if (string= my-laser laser)
	  (setq output (laser-energy (aref elite-lasers i)))
	  )
      (setq i (1+ i))
      )
    (max output)
    )
  )

(defun elite-enemy-laser (flags)
  "Enemy fires. Return enemy fire output."
  (let (
	(output)
	)

    (if (= (logand flags FLG_PULSE_LASER) FLG_PULSE_LASER)
	(progn
	  (setq output (laser-energy (aref elite-lasers 0)))
	  ;(setq output (laser-energy laser))
	  )
      )
    
    (if (= (logand flags FLG_BEAM_LASER) FLG_BEAM_LASER)
	(progn
	  (setq output (laser-energy (aref elite-lasers 1)))
	  ;(setq laser (laser-type (aref elite-lasers 1)))
	  ;(setq output (laser-energy laser))
	  )
      )
    
    (if (= (logand flags FLG_MILITARY_LASER) FLG_MILITARY_LASER)
	(progn
	  (setq output (laser-energy (aref elite-lasers 3)))
	  ;(setq output (laser-energy laser))
	  )
      )
    
    (max output)
  
    )
  )

(defun elite-combat-enemy-action (flags)
  (let (
	(enemy-action)
	)

  ;enemy fires laser if random greater than 127
       ;(if (> (elite-rand1 t) 127)
  (if (> (elite-rand1 t) 70)
      (progn
		  ;if hostile, fire laser
	(if (/= (logand FLG_HOSTILE flags) 0)
	    (progn
			  ;(if (> (elite-rand1 t) 230)			  
			   ;   (setq enemy-action ENEMY_MISSILE)
	      (setq enemy-action ENEMY_LASER)
					; )
	      )
	  (progn
					;enemy not hostile, if angry fire laser
					;if not evasive action
	    (if (and (> (elite-rand1 t) 127) (/= (logand FLG_ANGRY flags) 0))
		(setq enemy-action ENEMY_LASER)
	      (progn
					;maybe other actions
		(if (> (elite-rand1 t) 127)
		    (setq enemy-action ENEMY_EVASIVE)
		  (setq enemy-action ENEMY_LASER)
		  )
		)
	      )
	    )
	  )
	)
    (progn
      
      
      (if (> (elite-rand1 t) 127)
	  (setq enemy-action ENEMY_EVASIVE)
	(progn
	  (setq enemy-action ENEMY_NOTHING)
	  
	  )
	)
      )
    )
  enemy-action
  )
  )


(defvar elite-dead-commander nil
  "dead commander")

(defun elite-destruction ()
  "Death.."
  ;make death screen/animation...  
  (read-string "Destruction..... Press enter to exit")
  (setq elite-dead-commander t)
  (elite-quit)
  ;(kill-buffer elite-buffer-name)
  ;(elite-kill-buffer t)
)


(provide 'elite-combat)