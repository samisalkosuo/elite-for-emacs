;;; elite-missions.el -  Elite missions

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

;funtions for missions

;mission list
(defconst MISSION_DELIVERY 0)
(defconst MISSION_PASSENGER_DELIVERY 1)
(defconst MISSION_ASSASSINATION 2);assassinate some person
(defconst MISSION_KIDNAP 3);kidnap person and deliver him/her to some planet
(defconst MISSION_MISSING_PERSON 4);agree to find some person, when found send message to person who asked to find person
(defconst MISSION_DESTROY_PRODUCTION 5);destroy major production facilities in a system, affect system economy
(defconst MISSION_ASSASSINATE_HEAD_OF_STATE 6);assassinate head of state, affect system government

;property list where key is government type and value
;is vector of available missions
;(setq elite-mission-list
(defvar elite-mission-list
  (list
   0 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;anarchy
   1 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;feudal
   2 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;multi-gov
   3 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;dictatorship
   4 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;communist
   5 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;confederacy
   6 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;democracy
   7 (list MISSION_DELIVERY MISSION_PASSENGER_DELIVERY) ;corporate state
   )
  "Elite mission list")
;(plist-get elite-mission-list 1)

;current mission format
;property list 0 "mission text"
(defvar elite-current-missions (list)
  "list of current missions in system")

(defstruct elite-accepted-mission
  mission
  day;day when accepted
)

;property list of commander missions
;key is commander
;value is vector of accepted missions
(defvar elite-accepted-missions (list)
  "Property list of commander missions.")

(defstruct elite-mission
  type
  description; generated mission description
  payment; mission payment upon delivery
  source ;system where this mission was received
  destination ;target destination
  legal ;mission legality t/nil
)

(defun elite-show-missions ()
  "Show  available missions. Called when docked."
  (let (
	)
    (if (not elite-current-missions)
	(elite-generate-missions)
      )
    (elite-insert-available-missions)

    )
  )

(defun elite-mission-complete ()
  "Completes any mission when entering station"
  ;complete mission..
  (let (
	(missions)
	(total-missions)
	(i)
	(j)
	(destination)
	(mission)
	(new-missions)
	(tmp)
	)
    (setq missions (plist-get elite-accepted-missions (elite-commander-name-value elite-commander-name)))
    (if missions
	(progn
	  (setq new-missions nil)
	  (setq total-missions (/ (length missions) 2))
	  (setq i 0)
	  (setq j 0)
	  (while (< i total-missions)
	    (setq mission (elite-accepted-mission-mission (plist-get missions i)))

	    (if (string= (elite-get-system-name currentplanet) (elite-mission-destination mission))
		(progn
                   ;mission accomplished
		  (let (
			(payment)
			(max-days)
			(elapsed-days)	
			)
		    (setq elapsed-days (- elite-day (elite-accepted-mission-day (plist-get missions i))))

		    (if (= (elite-mission-type mission) MISSION_PASSENGER_DELIVERY)
			(elite-passenger-delivery-complete mission elapsed-days)
		      )

		    (if (= (elite-mission-type mission) MISSION_DELIVERY)
			(progn
			  ;;max-days is days for delivery when client is satisfied
			  ;;if this is exceeded client is angry
			  ;;max-days is 2 + distance days
			  (setq max-days (+ 2 (ceiling (/ (distance (aref galaxy (matchsys (elite-mission-source mission))) (aref galaxy (matchsys (elite-mission-destination mission)))) 10.0 ))))

			  (if (not (elite-mission-legal mission))
			      (progn
			        ;set commander legal status if delivery item was illegal
				;
				(setq tmp (elite-rand1 t))
				
				;government type affects to police efficiency when detecting 
				;illegal deliveries
				(if (>= (plansys-govtype (aref galaxy currentplanet)) 2)
				    (setq tmp (+ tmp (* (plansys-govtype (aref galaxy currentplanet)) 5)))
				  )
				
				(if (> tmp 230)
				    (progn
				      (elite-message-shell ">Incoming message: This is the Galactic Police. Your delivery was illegal. Item has been confiscated and your employer has been apprehended.")
				      (setf (commander-legal-status elite-commander) (+ (commander-legal-status elite-commander) 20))
				      (setq payment 0)
				      )
				  (progn
				    (elite-message-shell ">Incoming message: Thank you for delivery. Payment have been transferred to your account.")
				    (setq payment (elite-mission-payment mission))
				    ;+1 reputation for successful delivery
				    (setf (commander-reputation elite-commander) (1+ (commander-reputation elite-commander)))
				    )
				  )
				)
			    (progn
			  (if (<= elapsed-days max-days)
			      (progn
				(elite-message-shell ">Incoming message: Thank you for delivery. Payment have been transferred to your account.")
				(setq payment (elite-mission-payment mission))
					;+1 reputation for successful delivery
				(setf (commander-reputation elite-commander) (1+ (commander-reputation elite-commander)))
				
				)
			    (progn
			      (elite-message-shell ">Incoming message: You are late. I will not pay anything.")
			      (setq payment 0)
			  ;-2 reputation for unsuccessful delivery
			      (setf (commander-reputation elite-commander) (- (commander-reputation elite-commander) 2))
			      )
			    )
			  )
			    )


			  (setq elite-player-cash (+ elite-player-cash payment))
			  )		      
		      )
		    )
		  )
	      (progn
		;mission destination is not current system

		(if (= (elite-mission-type mission) MISSION_PASSENGER_DELIVERY)
		    (progn
		      ;passenger indicates that he/she is alive
		      (if (> (elite-rand1 t) 200)
			(elite-passenger-activity mission)
			)
		      )
		  )   
		
		;(elite-insert-text "mission>")
		(setq new-missions (plist-put new-missions j 
					      (make-elite-accepted-mission
					       :day (elite-accepted-mission-day (plist-get missions i))
					       :mission mission
					       )
					      )
		      )
		(setq j (1+ j))

		)
	      )
	    (setq i (1+ i))
	    )
	  (setq elite-accepted-missions (plist-put elite-accepted-missions (elite-commander-name-value elite-commander-name) new-missions))
	  )
      )
    )
  )

(defun elite-insert-available-missions ()
  ""
  (let (
	(total-missions)
	(msg)
	(i)
	)
    (setq total-missions (/ (length elite-current-missions) 2))

    (setq msg (concat ">Available missions in " (elite-get-system-name currentplanet) "\n"))
    (if (= total-missions 0)
	(setq msg (concat msg "No missions at this time."))
      (progn
	(setq i 0)
	(while (< i total-missions)
	  (setq msg (concat 
		     msg 
		     (number-to-string (1+ i))
		     ". "
		     (elite-mission-description (plist-get elite-current-missions i))
		     "\n"
		     )
		)
	  (setq i (1+ i))
	  )

	
	)
      )
    (elite-insert-text msg)
        
    )  
  )

(defun elite-generate-missions ()
  "Generates available missions in current system."
  (let
      (
       (govtype)
       (techlevel)
       (economy)
       (mission-list)
       (mission-type)
       (i)
       (j)
       (kp)
       (k)
       (total-missions)
       (tmp)
       )
    (setq govtype (plansys-govtype (aref galaxy currentplanet)))
    (setq techlevel (plansys-techlevel (aref galaxy currentplanet)))
    (setq economy (plansys-economy (aref galaxy currentplanet)))

    (setq mission-list (plist-get elite-mission-list govtype))
    (setq i 0)
;      (if (<= govtype 1)
; 	(setq total-missions (random 3))
;       (setq total-missions (random (* 2 govtype)))
; 	)

    ;fugitives get maybe only one mission
    (if (> (commander-legal-status elite-commander) elite-offender-fugitive-threshold)
	(progn
	  (if (> (elite-rand1 t) 130)
	      (setq total-missions 1)
	    (setq total-missions 0)
	    )
	)
      (progn
	(setq total-missions (random (+ economy govtype)))

        ;if reputation is below zero only one mission is offered
	(if (< (commander-reputation elite-commander) 0)
	    (setq total-missions 1)
	  )
        ;if reputation is above 200 twice normal missions is offered
	(if (> (commander-reputation elite-commander) 200)
	    (setq total-missions (* 2 total-missions))
	  )
	)
      )

    (setq j 0)
    ;people in current system
    (setq kp (length (elite-get-people-in-system currentplanet)))
    (setq k 0)

    ;(setq total-missions 10);testing

    
    (while (< i total-missions)
      (setq mission-type (nth (random (length mission-list)) mission-list))
      ;(setq mission-type MISSION_PASSENGER_DELIVERY);testing
      
      (if (= mission-type MISSION_PASSENGER_DELIVERY)
	  (progn
	    (if (< k kp)
		(progn
		  (setq tmp (elite-generate-passenger-delivery))
		  (setq k (1+ k))
		  )
	      (setq tmp nil)
	      )
	    )
	)

      (if (= mission-type MISSION_DELIVERY)
	  (setq tmp (elite-generate-delivery))
	)
      (if tmp
	  (progn
	    (setq elite-current-missions (plist-put elite-current-missions j tmp))
	    (setq j (1+ j))
	    )
	)
      (setq i (1+ i))
      )
    )
  )


(defun elite-available-missions ()
  "Shows list of available missions"
  (interactive)
  (if (elite-is-docked)
      (elite-insert-available-missions)
    (elite-insert-text ">Available missions\nNot docked.")
    )
)

(defun elite-select-mission ()
  "Shows list of available missions"
  (interactive)
  (if (elite-is-docked)
      (progn
	(let (
	      (mission)
	      (missions)
	      (total-missions)
	      (accepted-missions)
	      (accepted-mission);accepted mission struct
	      (tmp)
	      (i)
	      )
	  ;get missions in completions list
	  (setq total-missions (/ (length elite-current-missions) 2))    
	  (if (= total-missions 0)
	      (elite-insert-text ">Select missions\nNo available missions..")
	    (progn
	      (elite-insert-text ">Select missions")
	      (setq i 0)
	      (setq missions (make-vector total-missions ()));completion list
	      (while (< i total-missions)
		;(aset missions i (list (elite-mission-description (plist-get elite-current-missions i)) i))
		(aset missions i (list (concat (number-to-string (1+ i)) ". " (elite-mission-description (plist-get elite-current-missions i))) i))
		(setq i (1+ i))
		)
	      (setq mission (completing-read "Select mission: " (append missions nil) nil t ))
	      ;(elite-insert-text mission)
	      (if (and (not (string= mission "")) (y-or-n-p (concat mission " Accept? ")))
		  (progn
		    (setq tmp (cdr (split-string mission)))
		    (setq mission "")
		    (while tmp
		      (setq mission (concat mission (car tmp) " "))
		      (setq tmp (cdr tmp))
		      )
		    (setq mission (substring mission 0 (1- (length mission))))
		    (setq tmp (elite-get-mission-struct mission))

		    (if (elite-special-mission-1-capture tmp);check mission 1
			(progn
			  (elite-remove-mission tmp)
			  )
		      (progn
		    ;add mission to accepted mission list
			  (setq accepted-mission (make-elite-accepted-mission
						  :day elite-day
						  :mission tmp
						  )
				)
			  
			  (setq accepted-missions (plist-get elite-accepted-missions (elite-commander-name-value elite-commander-name)))
			  (setq accepted-missions (plist-put accepted-missions (/ (length accepted-missions) 2) accepted-mission))
			  (setq elite-accepted-missions (plist-put elite-accepted-missions (elite-commander-name-value elite-commander-name) accepted-missions))
			  
			  (elite-remove-mission tmp)
			  (elite-insert-text "Mission accepted")
			  )
		      )
		    )
		(progn
		  )
		)
	      (message "")
	      )
	    )
	  )
	)
    (elite-insert-text ">Select missions\nNot docked.")
    )
  
)

(defun elite-list-commander-missions ()
  "Lists mission that commander have accepted"
  ;(interactive)
  (let (
	(msg)
	(accepted-mission)
	(accepted-missions)
	(total-accepted-missions)
	(i)
	)
    (setq accepted-missions (plist-get elite-accepted-missions (elite-commander-name-value elite-commander-name)))
    (if accepted-missions
	(progn
	  (setq msg ">Commander missions\n")
	  ;(elite-insert-text msg)
	  (setq total-accepted-missions (/ (length accepted-missions) 2))
	  (if (= total-accepted-missions 0)
	      (setq msg "No missions.")
	    (progn
	      (setq i 0)
	      (while (< i total-accepted-missions)
		(setq accepted-mission (elite-accepted-mission-mission (plist-get accepted-missions i)))
		(setq msg 
		      (concat
		       msg
		       "Day accepted "
		       (number-to-string (elite-accepted-mission-day (plist-get accepted-missions i)))
		       ": "
		       (elite-mission-description accepted-mission)
		       "\n"
		       )
		      )
		(setq i (1+ i))
		)
	      )
	    )
	  (elite-insert-text msg)
	  )
      )
    )
  )

(defun elite-get-mission-struct (description)
  "return mission struct of mission which description
   is parameter description"
  (let (
	(i)
	(total-missions)
	(mission)
	)
    (setq i 0)
    (setq total-missions (/ (length elite-current-missions) 2))
    (while (< i total-missions)
      (setq mission (plist-get elite-current-missions i))
      (if (string= (elite-mission-description mission) description)
	  (setq i (+ i 1000))
	(setq i (1+ i))
	)
      )
    (prin1 mission)
    )
  )

(defun elite-remove-mission (mission-to-be-removed)
  "Removes mission from current mission list"
  (let (
	(i)
	(j)
	(total-missions)
	(mission)
	(new-current-missions)
	)
    (setq j 0)
    (setq i 0)
    (setq total-missions (/ (length elite-current-missions) 2))
    (while (< i total-missions)
      (setq mission (plist-get elite-current-missions i))
      (if (string= (elite-mission-description mission) (elite-mission-description mission-to-be-removed))
	  (
					;don't add mission-to-be-removed to new list)
	   )
	(progn
	  (setq new-current-missions (plist-put new-current-missions j mission))
	  (setq j (1+ j))
	  ;add mission to new list
	  )
	)
	(setq i (1+ i))
      )
    (setq elite-current-missions new-current-missions)

    )
  )

(defun elite-initialize-missions ()
  "initialize missions (variables etc)."
  
	 ;value   govtypes          economy 
	 ;0       anarchy           Rich Ind
	 ;1       Feudal            Average Ind
	 ;2       Multi-gov         Poor Ind
         ;3       Dictatorship      Mainly Ind
	 ;4       Communist         Mainly Agri
	 ;5       Confederacy       Rich Agri
	 ;6       Democracy         Average Agri
	 ;7       Corporate State   Poor agri
  ;set deliverable items
  (setq elite-deliverable-items
	(vector
	 ;                            name        govtype economy techlevel baseprice everywhere legal
	 (elite-make-deliverable-item "documents" 0       7        0         500      t         t)
	 (elite-make-deliverable-item "components" 0       7        0         500     t         t)
	 (elite-make-deliverable-item "industrial documents" 2       3        0   1000 t         t)
	 (elite-make-deliverable-item "secret government documents" 2       7        0   1000 t         nil)
	 (elite-make-deliverable-item "experimental narcotics" 0       7        0   1000 nil     nil)
	 (elite-make-deliverable-item "weapon prototype" 2       3        6   1000 t     nil)
	 )
	)

  (setq elite-delivery-texts
	(list
	 "Deliver"
	 "Urgent! Deliver"
	 "Immediate delivery needed. Deliver"
	 "Trustworthy courier wanted. Deliver"
	 "Courier wanted. Deliver"
	 )
	)

  (setq elite-passenger-delivery-texts
	(list
	 "I require transport to"
	 "Fast transport needed, destination"
	 )
	)

  )

(defvar elite-passenger-delivery-texts nil
  "Texts for passenger delivery"
  )

;delivery missions
(defvar elite-deliverable-items nil
  "available delivery items")

(defvar elite-delivery-texts nil
  "available delivery items")

(defstruct elite-deliverable-item
  name
  govtype;govtype where item is available, if govtype 6 available in 6 and 7.
  economy;economy where item is available, if econ is 3 then is availeble in systems where 
          ;econ 3,2,1,0. Economy 7 means that item is avalaible everywhere
  techlevel;techlevel where item isavailable
  baseprice;base price
  everywhere; if anywhere is t then this item is available in all systems with gov,econ,tech equal
          ;or greater than specified
          ;if nil then item is availabe only in system with gov and economy equals to specified
  legal
)

(defun elite-make-deliverable-item (name govtype economy techlevel baseprice everywhere legal)

  (make-elite-deliverable-item
   :name name
   :govtype govtype
   :economy economy
   :techlevel techlevel
   :baseprice baseprice
   :everywhere everywhere
   :legal legal
   )
)


(defun elite-generate-delivery ()
  "Generates delivery mission"
  ;todo: 06.04.2001 delivery from one person to another.
  (let
      (
       (item)
       (mission-struct)
       (mission-text)
       (valid)
       (planet)
       (systems)
       (destination)
       (i)
       (payment)
       (price)
       )
    ;get deliverable item
    (setq planet (aref galaxy currentplanet))
    (setq valid nil)
    (setq i 0)
    (while (and (not valid) (< i 10))
      (setq item (aref elite-deliverable-items (random (length elite-deliverable-items))))

      (if (elite-deliverable-item-everywhere item)
	  (setq valid (and
		       (>= (plansys-govtype planet) (elite-deliverable-item-govtype item))
		       (<= (plansys-economy planet) (elite-deliverable-item-economy item))
		       (>= (plansys-techlevel planet) (elite-deliverable-item-techlevel item))
		       )
		)
	(setq valid (and
		     (= (plansys-govtype planet) (elite-deliverable-item-govtype item))
		     (= (plansys-economy planet) (elite-deliverable-item-economy item))
		     (>= (plansys-techlevel planet) (elite-deliverable-item-techlevel item))
		     )
	      )
	)
      
      ;if commander is fugitive only illegal missions are offered
      (if (> (commander-legal-status elite-commander) elite-offender-fugitive-threshold)
	  (setq valid (and valid (not (elite-deliverable-item-legal item))))
	)
      (setq i (1+ i))
      )
    (if (< i 10)
	(progn	  
	  (setq systems (elite-systems-within-radius 140))
	  (setq destination (aref galaxy (aref systems (random (length systems)))))
	  ;illegal deliverias 2*baseprice
; 	  (if (elite-deliverable-item-legal item)
; 	      (setq price (elite-deliverable-item-baseprice item))
; 	    (setq price (* 2 (elite-deliverable-item-baseprice item)))
; 	    )
	  (setq price (elite-deliverable-item-baseprice item))

	  (setq mission-struct (make-elite-mission
				:type MISSION_DELIVERY
				:payment price
				:source (elite-get-system-name currentplanet)
				:destination (plansys-name destination)
				:legal(elite-deliverable-item-legal item)
				)
		)
	  (setf (elite-mission-description mission-struct) 
		(concat 
		 (nth (random (length elite-delivery-texts)) elite-delivery-texts)
		 " "
		 (elite-deliverable-item-name item)
		 " to "
		 (elite-mission-destination mission-struct)
		 ". Will pay "
		 (format "%.1f CR." (/ (elite-mission-payment mission-struct) 10.0))
		 )
		)
	  mission-struct
	  ;(setq elite-current-missions (plist-put elite-current-missions key mission-struct))
	  )
      )
    )
  )

(provide 'elite-missions)