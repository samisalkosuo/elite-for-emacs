;;; elite-missions-passenger-delivery.el -  Passenger delivery mission

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
;delivery types TODO
; -person wants to go from current planet to home planet
; -person wants to leave home planet and go somewhere else
; -person has trip to another system and when trip is over
;   person wants to return to home planet

(defvar elite-people-with-missions (list)
  "list of people who already have mission in current system")

(defun elite-generate-passenger-delivery ()
  "Generates passenger delivery mission"
  (let (
	(mission-struct)
	(people-in-system)
	(person)
	(destination)
	(destination-home)
	(price)
	(txt)
	(i)
	 )
    ;get people in current planet
    ;(setq elite-people-with-missions nil)
    (setq people-in-system (elite-get-people-in-system currentplanet))
    (if people-in-system
	(progn
;	  (while people-in-system
	    (setq person (nth (random (length people-in-system)) people-in-system))
	    (setq i 0)
	    (while (and (elite-person-has-mission person) (< i 10))
	      (setq person (nth (random (length people-in-system)) people-in-system))
	      (setq i (1+ i))
		)
	    (setq elite-people-with-missions (append elite-people-with-missions (vector person) nil))

	    (if (and (/= (elite-person-current-planet person) currentplanet) (> (elite-rand1 t) 200))
		(progn
		  ;person wants to go home
		  (setq destination (elite-person-home-planet person))
		  (setq destination-home t)
		  (setq txt "I require transport to my home planet,")
		  )
		(progn
		  (setq destination-home nil)
		  (setq destination (random galsize))
		  (while (= destination currentplanet)
		    (setq destination (random galsize))
		    )
		  (setq txt (nth (random (length elite-passenger-delivery-texts)) elite-passenger-delivery-texts))
		  )
		)
	    (setq price (distance (aref galaxy currentplanet) (aref galaxy destination)))
; 	    (while (> price 70)
; 	      (setq destination (random galsize))
; 	      (setq price (distance (aref galaxy currentplanet) (aref galaxy destination)))
; 
; 	      )

	    (setq mission-struct (make-elite-mission
				  :type MISSION_PASSENGER_DELIVERY
				  :payment (* price 10);payment is 10 CR/LY
				  :source (elite-get-system-name currentplanet)
				  :destination (plansys-name (aref galaxy destination))
				  :legal t
				  )
		  )
	    (setf (elite-mission-description mission-struct) 
		  (concat 
		   "This is "
		   (elite-person-name person)
		   ". "
		   txt
		   " "
		   (elite-mission-destination mission-struct)
		   ". I'll pay "
		   (format "%.1f CR." (/ (elite-mission-payment mission-struct) 10.0))
		   )
		  )
	    mission-struct
	    )
      )    
    )
  )

(defun elite-passenger-delivery-complete (mission elapsed-days)
  "Checks if mission is completed.
   This is called after docking and if mission destination 
   is current system."
  (let (
	(payment)
	(mission-text)
	(name)
	(home)
	)
    (setq mission-text (elite-mission-description mission))
    (setq name (elite-get-name-from-description mission-text))
    (setq home (elite-update-person-current-planet name))
    (if (> elapsed-days 180)
	(progn
	  ;180 days is enough for passenger to wait in players ship
	  (elite-message-shell (concat ">Incoming message from " name ": Finally. I am late and you won't get any money from me."))
	  (setq payment 0)
	  (setf (commander-reputation elite-commander) (- (commander-reputation elite-commander) 10))
	  )
      (progn
	(if home
	    (elite-message-shell (concat ">Incoming message from " name ": Home sweet home.. Thank you. Payment have been transferred to your account."))
	  (elite-message-shell (concat ">Incoming message from " name ": Thank you. Payment have been transferred to your account."))
	)

	(setq payment (elite-mission-payment mission))
					;+5 reputation for successful passenger transport
	(setf (commander-reputation elite-commander) (+ 5 (commander-reputation elite-commander)))
	)
      )
    (setq elite-player-cash (+ elite-player-cash payment))
    )
  )
  
(defvar elite-passenger-activity-texts
;(setq elite-passenger-activity-texts
  (list
   "Are we there yet?"
   "Where are we?"
   "What's taking so long!"
   "You snake! Why don't you crawl faster to my destination!"
   "I am tired of looking at you."
   "Booring!!"
   "Ho-hum."
   "Aaagh! I'm loosing my mind here!"
   "I like this. Do you need a co-pilot?"
   "Have you been in Pattijoki?"
   "You know.. Oulu is quite nice town.."
   "Have I told you about Freiburg?"
   "Do you know my ancestor had a Harley Davidson?"
   "Who are you?"
   "What do you want?"
   "Where are you going?"
   "Who are you? What do you want?"
   "This is inhuman. Nobody told me traveling would be like this."
   "\nThree Rings for the Elven-kings under the sky,\nSeven for the Dwarf-lords in their halls of stone,\nNine for Mortal Men doomed to die,\nOne for the Dark Lord on his dark throne.\nIn the Land of Mordor where the Shadows lie.\nOne Ring to rule them all, One Ring to find them,\nOne Ring to bring them all and in the darkness bind them\nIn the Land of Mordor where the Shadows lie."
   )
  )

(defun elite-passenger-activity (mission)
  
  (let (
	(mission-text)
	(name)
	)
    (setq mission-text (elite-mission-description mission))
    (setq name (elite-get-name-from-description mission-text))
    (if elite-use-windows
	(progn
	  (elite-message-shell (concat 
			">Message from " 
			name
			": "
			(nth (random (length elite-passenger-activity-texts)) elite-passenger-activity-texts)
			))
	  )
      (progn
	(elite-insert-text (concat 
			">Message from " 
			name 
			": "
			(nth (random (length elite-passenger-activity-texts)) elite-passenger-activity-texts)
			)
		       )
	))
    )
  )

(defun elite-get-name-from-description (description)
  (let (
	(desc)
	(first-name)
	(last-name)
	)
    (setq desc (split-string description))
    (setq first-name (nth 2 desc))
    (setq last-name (substring (nth 3 desc) 0 (1- (length (nth 3 desc)))))
    (concat first-name " " last-name)
    )
  )

(defun elite-person-has-mission (person)
  
  (let (
	(i)
	(len)
	(desc)
	(has-mission)
	(out)
	(tmp)
	)
    (setq i 0)
    (setq has-mission nil)
    (setq len (/ (length elite-current-missions) 2))
    (while (< i len)
      (setq desc (elite-mission-description (plist-get elite-current-missions i)))
      (if (string= (elite-get-name-from-description desc) (elite-person-name person))
	  (setq has-mission t)
	  )
      (setq i (1+ i))
      )
    
    (if (elite-person-out-of-loop person)
	(setq has-mission t)
	)
    
    has-mission
    )
)

(defun elite-passenger-on-board ()
  "return true if player has passenger on board"
  (let (
	(missions)
	(total-missions)
	(mission)
	(on-board)
	(i)
	(j)
	)
    (setq on-board 0)
    (setq missions (plist-get elite-accepted-missions (elite-commander-name-value elite-commander-name)))
    (if missions
	(progn
	  (setq total-missions (/ (length missions) 2))
	  (setq i 0)
	  (setq j 0)
	  (while (< i total-missions)
	    (setq mission (elite-accepted-mission-mission (plist-get missions i)))
	    (if (= (elite-mission-type mission) MISSION_PASSENGER_DELIVERY)
		(setq on-board (1+ on-board))
	      )
	    (setq i (1+ i))
	    )
	  )
      )
    on-board
    )
  )

(provide 'elite-missions-passenger-delivery)