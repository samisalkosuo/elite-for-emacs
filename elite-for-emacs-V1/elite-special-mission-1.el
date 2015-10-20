;;; elite-special-mission-1.el -  Code for first special mission

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
;first implemented special mission:
; player must have elite rating competent
; mission is initiated after player is competent and encounters first navy officer
; Navy officer asks player to hunt down all navy traitors and return them to navy headquarter for "interrogation"
; navy headquarters is on XEER, galaxy 1
; one of the traitors may have somethin 
; reward is 10000 CR per traitor
; after all traitor have been delivered to XEER player receiver commendation from the navy:
; a medal, title etc.

(defvar elite-special-mission-1-accepted nil
  "if t player has accepted mission and is currently trying to finish it")

(defvar elite-special-mission-1-completed nil
  "t if player has completed mission 1")

(defvar elite-special-mission-1-traitors nil
  "list of navy traitors")

;traitor status
    ;status at large, captured (in ship), captured (held by Navy)
(defconst SPECIAL_MISSION_1_TRAITOR_AT_LARGE "At large");at large
(defconst SPECIAL_MISSION_1_TRAITOR_SHIP "Captured (in ship)");captured by player and currently in ship
(defconst SPECIAL_MISSION_1_TRAITOR_NAVY "Captured (held by Navy)");delivered to Navy

(defvar elite-special-mission-1-traitor-status nil)

(defvar elite-special-mission-1-refused nil)

(defvar elite-special-mission-1-naval-galactic-hyperdrive nil)

(defvar elite-special-mission-1-briefing-text
;(setq elite-special-mission-1-briefing-text
  "Excellent. First of all, I must ask that you do not repeat any of this to anyone.\nAs you know Galactic Navy is the main defense force of The Eight Galaxies territories and the only military force that operates in space.\nIn recent years we have encountered quite a few strange phenomena in The Eight Galaxies territory and we are not certain who or what is behind these phenomena. They seem to appear randomly and some phenomena have been observed by ordinary traders, perhaps you have also seen something.. We do not encourage to report anything and we have instructed all local authorities not to release any information regarding these phenomena.\nFurthermore, approximately the same time as phenomena first appeared we have encountered unusual behaviour among our best officers. This, more than anything else, has been a shock to us. You may also be shocked to know that unusual behaviour has been always treason.\nWe believe that we have caught most of the traitors. Those we have not caught are the elite force of the Navy and they know our operations in extreme detail and that is why we must turn to people like you. This is the first time in Navy's history when we are forced to rely on mercenaries. We are confident that traitors do not anticitipate this action. We are, after all, The Galactic Navy.\nYour mission is to locate all traitors and deliver them to Navy Headquarters in XEER. For each traitor you will be rewarded 10000 CR.\nIf you accept, we will provide a list of traitors and their homeplanets. After that it is up to you to locate them and deliver them to our custody.\nKeep in mind that if you don't accept we have to make sure that you do not repeat any of this to anyone."
)

(defun elite-do-special-mission-1 ()
  "This function is called when docked and this check if 
   player can get this special mission"
  (let (
	(people-in-system)
	(profession)
	(navy-officer)
	(person)
	(msg)
	)
    ;check elite status
    ;if legal status is clean, elite rating competent
    ;and reputation must be over 10
    ;(if (and (not elite-special-mission-1-accepted) (not elite-special-mission-1-completed) )
    (if (and (not elite-special-mission-1-accepted) (not elite-special-mission-1-completed) (>= (commander-elite-score elite-commander) 128) (= (commander-legal-status elite-commander) 0) (> (commander-reputation elite-commander) 10) )
	(progn
	  (setq people-in-system (elite-get-people-in-system currentplanet))
	  ;check if there is navy officer
	  ;if there are several navy officer, last navy officer asks if player
	  ;wants to take the mission
	  (setq navy-officer nil)
	  (setq navy-officer (car people-in-system))	  
	  (while people-in-system
	    (setq person (car people-in-system))
	    (setq profession (elite-person-profession person))
	    (if (= profession PERSON_NAVY_OFFICER)
		(setq navy-officer person)
		)
	    (setq people-in-system (cdr people-in-system))
	    )
	  (if navy-officer
	      (progn
		;there was a navy officer in the system
		(elite-message-shell (concat 
				    ">Incoming message: This is the Galactic Navy Officer "
				    (elite-person-name navy-officer)
				    ". We have a mission for you."
				    )
				   )
		;ask if player is interested
		(if (y-or-n-p "Are you interested? ")
		    (progn
		      (message "")
		      ;player is interested, continue briefing		     
		      (elite-message-shell (concat ">" elite-special-mission-1-briefing-text) t)
		      (if (y-or-n-p "Do you accept? ")
			  (progn
			    (message "")
			    ;player accepts mission
			    (setq elite-special-mission-1-accepted t)
			    ;add mission 1 status command to additional commands
			    (setq elite-additional-commands 
				  (append elite-additional-commands (list
				   (list "mission-1" 1);shows buffer where is status of special mission 1
				   ))
				  )

			    (elite-message-shell ">Excellent. We expected nothing less.\nWe provide you a list of traitors and an additional program to your ship's computer. You may consult this program to see current mission status.")
			    
			    (setq elite-special-mission-1-traitors (elite-get-people-by-profession PERSON_NAVY_TRAITOR))
			    (setq elite-special-mission-1-traitor-status (make-vector (length elite-special-mission-1-traitors) SPECIAL_MISSION_1_TRAITOR_AT_LARGE))
			    ;(setq elite-special-mission-1-log-entries nil)
			    (setq elite-special-mission-1-log-entries (append elite-special-mission-1-log-entries (list (list (number-to-string elite-day) (number-to-string galaxynum) (elite-get-system-name currentplanet) (concat "Mission accepted from Officer " (elite-person-name navy-officer)) )) nil))

			    )
			(progn
			  (message "")
			  ;player does not acccept
			  (elite-message-shell "Very well...\nYou will meet an accident after you undock. Good day.")
			  (setq elite-special-mission-1-refused t)
			  
			  )
			)
		      )
		  (progn
		    (message "")
		    ;;player is not interested
		    (elite-message-shell "Very well...\nYou did not meet me.")
		    
		    )
		  )
		)
	    )
	  )
      )
    
    (if (and elite-special-mission-1-accepted (not elite-special-mission-1-completed))
	(progn
	  ;show people in system
	  (if (and elite-special-mission-1-accepted (not elite-special-mission-1-completed))
	      (elite-insert-text (concat
				  ">People in "
				  (elite-get-system-name currentplanet)
				  "\n"
				  (elite-list-people-in-system currentplanet)
				  )
				 )
	    )
	  (elite-special-mission-1-log)
	  )
    )

    (if (and (string= (elite-get-system-name currentplanet) "XEER") elite-special-mission-1-accepted (not elite-special-mission-1-completed))
	(progn
	  ;in xeer
	  ;check if traitors are in hold
	  (let (
		(traitor-status)
		(traitor)
		(gender)
		(status)
		(i)
		)
	    (setq traitor-status (append elite-special-mission-1-traitor-status nil))
	    (setq i 0)
	    (while traitor-status
	      (setq status (car traitor-status))
	      (if (string= status SPECIAL_MISSION_1_TRAITOR_SHIP)
		  (progn
		    ;traitor captured in ship
		    (aset elite-special-mission-1-traitor-status i SPECIAL_MISSION_1_TRAITOR_NAVY)
		    (setq traitor (nth i elite-special-mission-1-traitors))
		    (if (= (elite-person-gender traitor) 1)
			(setq gender "him")
		      (setq gender "her")
		      )
		    (elite-message-shell (concat 
					">Incoming message from Galactic Navy: Thank you for capturing traitor "
					(elite-person-name traitor)
					". We will interview the traitor and we suggest that you do not remember " 
					gender
					" in the future."
					" Payment have been made to your account."))		    
		    (setq elite-player-cash (+ elite-player-cash 100000))
		    )
		  )
	      
	      (setq i (1+ i))
	      (setq traitor-status (cdr traitor-status))
	      )

	    ;check if mission is completed
	    (setq traitor-status (append elite-special-mission-1-traitor-status nil))

	    (setq elite-special-mission-1-completed t)
	    (while traitor-status
	      (setq status (car traitor-status))
	      (if (not (string= status SPECIAL_MISSION_1_TRAITOR_NAVY))
		  (progn
		    (setq elite-special-mission-1-completed nil)
		    )
		  )
	      (setq traitor-status (cdr traitor-status))
	      )

	    ;mission completed
	    (if elite-special-mission-1-completed
		(progn
		  (elite-message-shell (concat ">Incoming message from Galactic Navy: All traitors are now in our custody. You have done well. \nAs a reward for your outstanding services we will give you Naval Galactic Hyperdrive. With our hyperdrive you can jump to any galaxy and you can use it indefinitely. Farewell, Commander" (commander-name elite-commander) "." ))
		  (setq elite-special-mission-1-naval-galactic-hyperdrive t)
		  )
		)
	    )
	  )
      )
    ) 
  )

(defvar elite-special-mission-1-log-entries (list)
    ;(setq elite-special-mission-1-log-entries nil)
  )

(defun elite-special-mission-1-log ()
  (let (
	(entry)
	)
;    (setq entry (concat (number-to-string elite-day) "\t" (number-to-string galaxynum) "\t" (elite-get-system-name currentplanet) "\t" (elite-list-people-in-system currentplanet ",")))
    (setq entry (list (number-to-string elite-day) (number-to-string galaxynum) (elite-get-system-name currentplanet)  (elite-list-people-in-system currentplanet ",")))

    (setq elite-special-mission-1-log-entries (append elite-special-mission-1-log-entries (list entry) nil))
    )
  ) 

(defun elite-special-mission-1-capture (mission)
  "When player accepts passenger delivery mission this function checks if traitor was captured.
   return t if captured."
  (let (
	(name)
	(traitors)
	(traitor)
	(i)
	(captured)
	)
    (setq captured nil)

					;check if mission is passenger delivery
    (if (= (elite-mission-type mission) MISSION_PASSENGER_DELIVERY)
	(progn
					;if mission is passenger delivery 
					;check if player has accepted special mission 1
	  (if elite-special-mission-1-accepted
	      (progn
		(setq name (elite-get-name-from-description (elite-mission-description mission)))
					;check if passenger is navy traitor
		(setq traitors elite-special-mission-1-traitors)
		(setq i 0)
		(while traitors
		  (setq traitor (car traitors))
		  (if (string= name (elite-person-name traitor))
		      (progn
					;traitor apprehended
			(elite-insert-text "Traitor captured.")
			(elite-message-shell ">Incoming message: Well done. Now bring traitor to XEER.")
			(aset elite-special-mission-1-traitor-status i SPECIAL_MISSION_1_TRAITOR_SHIP)
			(setq captured t)
			;delete traitor from person list
			;(aset (aref elite-people-in-universe (1- galaxynum)) (1- galaxynum) (vconcat (delete traitor (append (aref elite-people-in-universe (1- galaxynum)) nil ) ) ))

			(setq elite-people-out-of-loop (append elite-people-out-of-loop (vector traitor) nil))
			)


		    )
		  (setq i (1+ i))
		  (setq traitors (cdr traitors))
		  )		
		)			
	      )
	  )
	)
    captured
    )
)


(defvar elite-special-mission-1-status-buffer-name "*Mission 1 Status*")

(defun elite-special-mission-1-show-status ()
  ""
  (interactive)
  (let (
	(txt)
	(i)
	(traitors)
	(traitor)
	)
    (setq txt ">Special mission 1:\n")
    (if elite-special-mission-1-accepted
	(progn
	  ;;show status if mission accepted
	  (if elite-special-mission-1-completed
	      (progn
		(setq txt (concat txt "Mission completed\n"))
		(insert "Ship equipped with Naval Galactic Hyperdrive. Allows unrestricted travel to any galaxy.\n")
		)
	    )
	  (setq txt (concat txt "Traitors (name;galaxy;home planet;status):\n"))
	  (setq i 0)
	  (setq traitors elite-special-mission-1-traitors)
	  (while traitors

	    (setq traitor (car traitors))
	    (setq traitor-name (elite-person-name traitor))
	    (setq txt (concat txt traitor-name ";"))
	    (setq txt (concat txt (number-to-string (1+ (elite-person-galaxy traitor))) ";"  ))
	    (setq txt (concat txt  (plansys-name (aref (aref elite-galaxies-in-universe (elite-person-galaxy traitor)) (elite-person-home-planet traitor))) ";"))
	    (setq txt (concat txt (aref elite-special-mission-1-traitor-status i) "\n" ))

	    (setq traitors (cdr traitors))
	    (setq i (1+ i))
	    )

	  ;;mission log
	  ;;shows visiten systems after accepting mission until mission is completed
	  ;;shows also people in system
	  ;;day  galaxy system people
	  (let (
		(entries)
		(entry)
		(entry2)
		(people-column)
		)
	    (setq txt (concat txt  "\nMission log, visited systems (day;galaxy;system;people):\n"))

	    (setq entries elite-special-mission-1-log-entries)
	    (while entries
	      (setq entry (car entries))
	      (while entry
		(setq entry2 (car entry))
		(if (cdr entry)
		    (setq txt (concat txt entry2 ";"))
		  (progn
		    (if (null entry2)
			(setq txt (concat txt  "\n"))
		      (progn
			(setq txt (concat txt (substring entry2 0 (1- (length entry2))) "\n"))
			)
		      )
		    )
		  )
		(setq entry (cdr entry))
		)


	      (setq entries (cdr entries))
	      )
	    )

	  (setq txt (concat txt "\n\nMission briefing\n\n"))
	  (setq txt (concat txt elite-special-mission-1-briefing-text "\n"))

	  (elite-insert-text txt)
	  )
      (elite-command-shell ">Special mission 1: not accepted")
      )
    )
)

(defun elite-special-mission-1-status-screen ()
  "shows status screen"
  (let (
	(traitors)
	(traitor)
	(traitor-name)	
	(buffer)
	(tmp)
	(i)

	(galaxy-column)
	(planet-column)
	(status-column)
	)
    ;show screen
    ;traitor name   home galaxy home planet status
    ;status at large, captured (in ship), captured (held by Navy)

    (setq buffer (get-buffer-create elite-special-mission-1-status-buffer-name))
    (switch-to-buffer elite-special-mission-1-status-buffer-name)
    ;(setq buffer-read-only nil)
    (erase-buffer)

    (elite-mode-set-empty-mode-line)
    ;set table
    (if elite-special-mission-1-completed
	(progn
	  (insert "Mission completed Mission completed Mission completed Mission completed Mission completed Mission completed\n\n")
	  (insert "Ship equipped with Naval Galactic Hyperdrive. Allows unrestricted travel to any galaxy.\n\n")
	  )
	)
    (insert "Name\t\t\t\t")
    (setq galaxy-column (current-column))
    (insert "Home galaxy\t\t\t")
    (setq planet-column (current-column))
    (insert "Home planet\t\t")
    (setq status-column (current-column))
    (insert "Status\n\n")
    
    (setq i 0)
    (setq traitors elite-special-mission-1-traitors)
    (while traitors
      
      (setq traitor (car traitors))
      (setq traitor-name (elite-person-name traitor))
      (insert traitor-name)
      (while (< (current-column) galaxy-column)
	(insert " ")
	)
      (insert (number-to-string (1+ (elite-person-galaxy traitor))))
      (while (< (current-column) planet-column)
	(insert " ")
	)

      (insert (plansys-name (aref (aref elite-galaxies-in-universe (elite-person-galaxy traitor)) (elite-person-home-planet traitor))))
      (while (< (current-column) status-column)
	(insert " ")
	)

      (insert (aref elite-special-mission-1-traitor-status i))
      (insert "\n")
      (setq traitors (cdr traitors))
      (setq i (1+ i))
      )
    ;mission log
    ;shows visiten systems after accepting mission until mission is completed
    ;shows also people in system
    ;day  galaxy system people
    (let (
	  (entries)
	  (entry)
	  (entry2)
	  (people-column)
	  )
      (insert "\n\nMission log, visited systems\n\n")
      (insert "Day\tGalaxy\tSystem\t\t")
      (setq people-column (current-column))
      (insert "People\n\n")

      (setq entries elite-special-mission-1-log-entries)
      (while entries
	(setq entry (car entries))
	(while entry
	  (setq entry2 (car entry))
	  (if (cdr entry)
	      (insert entry2 "\t")
	    (progn
	      (while (< (current-column) people-column)
		(insert " ")
		)
	      (if (null entry2)
		  (insert "\n")
		(progn
		  (insert (substring entry2 0 (1- (length entry2))) "\n")
		  )
	      )
	      )
	    )
	  (setq entry (cdr entry))
	  )


	(setq entries (cdr entries))
	)      
      )
    

   ;insert briefing
    (insert "\n\nMission briefing\n\n")
    (insert elite-special-mission-1-briefing-text)
    (goto-char 0)
    (goto-char 130)
    ;(setq buffer-read-only t)

    ;(read-string "Press enter to close this screen...")
    ;(kill-buffer buffer)
  )
)


(provide 'elite-special-mission-1)