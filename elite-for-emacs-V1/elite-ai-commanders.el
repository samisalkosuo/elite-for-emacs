;;; elite-ai-commanders.el -  Elite for EMACS AI Commanders 

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
;;
;; Features now and future
;; -AI roaming in galaxy
;; -trading
;; -list ai commander info
;; -AI buys equipment
;; -display list of commanders in current system when docked
;; -talk to ai commander
;;    -ask out, romantic dinner?
;;      -if three times out with same companion ==> engaged
;;      -if six times out with same companion ==> married
;; -if on the same station ==> play othello with ai commander for money
;; -ai commander combat
;; -AI roaming in other galaxies
;; -AI does missions
;; -can ask ai has seen person
;; -if male and female on the same station ==> new ai commander, not always,love relationships??
;;    -family events notification on screen..
;; -if several commanders on same stations 
;;    ==> something happens...
;; -modify buy goods rules  
;;

(defvar elite-ai-commander-list nil
  "List of ai commanders")

;;(setq elite-ai-commander-list nil)
;;(elite-ai-initialize)
;;(elite-ai-commander-start-activity)
;;(elite-ai-commander-continue-activity)
;;(elite-ai-commander-end-activity)
;;(elite-ai-test-activity 150 t)
;;(length elite-ai-commander-list)
;;(elite-ai-commander-display-info)
;;(length elite-ai-commander-list)
;;(setq elite-ai-commander-list nil)
;;(elite-ai-commander-write-path-file)

(defun elite-ai-test-activity(len &optional init)
  (let (
	(i)
	;;(len)
	)
    (if init
	(progn
	  (setq elite-ai-commander-list nil)
	  (setq elite-ai-commander-path nil)
	  (elite-ai-initialize)
	  )
      )
    (setq i 1)
    ;;(setq len (length elite-ai-commander-list))
    (while (<= i len)
      (message (concat "Testing.." (number-to-string i) "/" (number-to-string len)))
      (elite-ai-commander-start-activity)      
      (elite-ai-commander-end-activity)
      ;;      (elite-ai-commander-activity)
      (setq i (1+ i))
      )
)
)

(defun elite-ai-initialize ()
  "Initializes ai commanders"
  (let (
	(i)
	(len)
	)
    (setq i 0)
    (elite-ai-set-commander-names)

    (setq elite-additional-commands 
	  (append elite-additional-commands (list
					     (list "elite-federation-pilots" 1);shows buffer where is status of special mission 1
					     ))
	  )

    (setq len (length elite-ai-commander-names))
    (while (< i len)
      (setq elite-ai-commander-list (append elite-ai-commander-list (vector (elite-ai-commander-generate i)) nil))
      (setq i (1+ i))
      )
    )
  )

(defvar elite-ai-commander-activity-in-progress nil
  )

(defvar elite-ai-commander-activity-current-commander 0
  "index of commander in commander list")

(defun elite-ai-commander-start-activity ()
  "Starts commander activity"
  (let (

	)
    ;;(message "Start activity")
    (setq elite-ai-commander-activity-in-progress t)
    (setq elite-ai-commander-activity-current-commander 0)
    )
  )

(defun elite-ai-commander-continue-activity (&optional end-activity saving)
  "Ends  commander activity"
  (let (
	(commander)
	(len)
	(i 0)
	(tmp)
	(msg)
	)
    (if elite-ai-commander-activity-in-progress
	(progn
	  ;;(message "Continue activity")
	  (if end-activity
	      (setq len (length elite-ai-commander-list))
	    (setq len (+ 1 elite-ai-commander-activity-current-commander))		  
	    )
	  
	  (if saving
	      (setq tmp "Saving")
	    (setq tmp (concat "Hyperspacing to " (elite-get-system-name currentplanet)))
	    )
	  (while (< elite-ai-commander-activity-current-commander len)
	    ;;(setq commander (nth elite-ai-commander-activity-current-commander elite-ai-commander-list))
	    ;;do commander activity
	    (if end-activity
		(progn
		  (setq msg (concat tmp (make-string i ?. )))
		  ;;(setq msg (concat  "Hyperspacing to " (elite-get-system-name currentplanet) ". Initializing engines" (make-string i ?. )))
		  (if (>= (length msg) (frame-width))
		      (progn
			(setq i 0)
			;;(setq msg (concat "Hyperspacing to " (elite-get-system-name currentplanet) (make-string i ?. )))
			(setq msg (concat tmp (make-string i ?. )))
			)
		    )
		  (message msg)
		  (setq i (1+ i))
		  )
	      )
	    (if (< elite-ai-commander-activity-current-commander (length elite-ai-commander-list))
		(elite-ai-commander-activity elite-ai-commander-activity-current-commander)
	      )

	    (setq elite-ai-commander-activity-current-commander (1+ elite-ai-commander-activity-current-commander))
	    )
	  (if (>= elite-ai-commander-activity-current-commander (length elite-ai-commander-list))
	      (setq elite-ai-commander-activity-in-progress nil)	      
	      )
	  )
	)
    )
  )

(defun elite-ai-commander-end-activity (&optional save)
  "Ends  commander activity"
  (let (

	)
    ;;(elite-ai-commander-continue-activity t)
    (if save
	(elite-ai-commander-continue-activity t t)
      (elite-ai-commander-continue-activity t)
    )
    
    ;;check if there are male and female on the same station ==> new ai commander, but now always
    (elite-ai-commander-rendezvous)

    ;set willingness to procreate
    (elite-ai-commander-set-willingness-to-procreate)

    (setq elite-ai-commander-activity-in-progress nil)
    )
  )


(defvar elite-ai-commander-path nil
  "list of lists for all commanders showing path of the systems. ((commander1 (2 3 4)) (commander12 (2 3 4)))")

(defun elite-ai-commander-activity (current-commander-index &optional display-hyperspace-message)
  "AI commander activity (trading etc) happens when 
   player jumps to hyperspace"
  (let (
	(commanders)
	(commander)
	(local-system-number)	
	(local-system-name)	
	(local-system)	
	(local-market)
	(system-log-persons)
	(next-system)
	(markets)
	(visited)
	(fuel-bought)
	(msg)
	(i)
	(len)
	(tmp)
	(people-in-system)
	
	)
    (setq markets nil)
    ;;order of thing for AI
    ;; do trading
    ;; accept missions
    ;; jump to next planet
    ;; do combat
    ;; dock
    ;; buy fuel
    (setq i 0)
;;     (setq commanders elite-ai-commander-list)
;;     (setq len (length commanders))
    ;(while commanders
      (if display-hyperspace-message
	  (progn
	    (setq msg (concat  "Hyperspacing to " (elite-get-system-name currentplanet) ". Initializing engines.." (number-to-string i) "/" (number-to-string len)))
;; 	    (if (>= (length msg) (frame-width))
;; 		(progn
;; 		  (setq i 0)
;; 		  (setq msg (concat "Hyperspacing to " (elite-get-system-name currentplanet) (make-string i ?. )))
;; 		  )
;; 	      )
	    (message msg)
	    )
	)
      (setq i (1+ i))

      ;;(setq commander (car commanders))
      (setq commander (nth current-commander-index elite-ai-commander-list))

      (if (= (elite-ai-commander-status commander) AI_COMMANDER_STATUS_ACTIVE)
	  (progn
	    ;;(setf (commander-current-day elite-commander) elite-day)
	    (setq local-system-number (elite-ai-commander-current-planet commander))


	    ;;add planet to list of visited systems
	    (let (
		  (path)
		  )
	      ;;(setq path nil)
	      ;;(elite-ai-test-activity 2 nil)
	      ;;(length elite-ai-commander-path)
	      ;;(nth 65 elite-ai-commander-path)
	      ;;(setq path (append path (list 66 )) )
	      ;;(setq path (cadr (assoc "Darth Vader" elite-ai-commander-path)))
	      ;;(setcdr (assoc "Darth Vader" elite-ai-commander-path) path)
	      ;;(setq path (cadr (assoc "Darth Vader" elite-ai-commander-path)))
	      ;;(setq elite-ai-commander-path (append elite-ai-commander-path (list (list (elite-ai-commander-name commander) path))))

	      (setq path (cadr (assoc (elite-ai-commander-name commander) elite-ai-commander-path)))
	      (setq path (append path (list local-system-number )) )
	      (if (not (assoc (elite-ai-commander-name commander) elite-ai-commander-path))
		  (setq elite-ai-commander-path (append elite-ai-commander-path (list (list (elite-ai-commander-name commander) path))))
		(setcdr (assoc (elite-ai-commander-name commander) elite-ai-commander-path) (list path))
	      )


;;	      (setq elite-ai-commander-path (assq-delete-all (elite-ai-commander-name commander) elite-ai-commander-path))

	      )

	    (setq local-system (aref galaxy local-system-number) )
	    ;;(commander-fluct elite-commander)
	    (if (= local-system-number currentplanet)
		(setq local-market localmarket)
	      (progn
		(if (cadr (assoc local-system-number markets))
		    (setq local-market (cadr (assoc local-system-number markets)))
		  (setq local-market (genmarket (commander-fluct elite-commander) local-system))
		  )
		)
	      )
	    ;;(setq local-market (genmarket 0 local-system)) ;;for testing, lave market
	    ;;(setq markets (append markets (list (list local-system-number (1- local-system-number)))))
	    ;;(car (cdr (assoc 7 markets)))

	    ;;sell stuff
	    (setq local-market (elite-ai-commander-sell commander local-market))

	    ;;buy equipment
	    (elite-ai-commander-buy-equipment commander)

	    ;;buy fuel
	    ;;todo: if commander can't buy fuel, sell any items
	    (setq fuel-bought (elite-ai-commander-buy-fuel commander))
	    ;;       (if (= fuel-bought 0)
	    ;; 	  (progn
	    ;; 	    (setq local-market (elite-ai-commander-sell commander local-market t))
	    ;; 	    (setq fuel-bought (elite-ai-commander-buy-fuel commander))
	    ;; 	    )
	    ;; 	  )
	    ;;buy stuff
	    (setq local-market (elite-ai-commander-buy commander local-system local-market))

	    ;;set visited planet
	    (setq visited (elite-ai-commander-visited-systems commander))
	    (setf (elite-ai-commander-visited-systems commander) (append (cdr visited) (list local-system-number)))

	    ;;hyperspace to next planet
	    (setq next-system (elite-ai-commander-hyperspace-to-next-planet commander))
	    (if  (not next-system)
		(progn

		  ;;(message (concat "Stuck at " (elite-get-system-name local-system-number)))
		  ;;(sit-for 2)
		  (setq tmp AI_COMMANDER_STATUS_STUCK)
		  ;;(setf (elite-ai-commander-status commander) AI_COMMANDER_STATUS_STUCK)
		  ;;(setf (elite-ai-commander-credits commander) 1000)
		  ;;(setf (elite-ai-commander-fuel commander) 70)
		  ;;(setq next-system (elite-ai-commander-hyperspace-to-next-planet commander))
		  )
	      (progn
		(setq tmp AI_COMMANDER_STATUS_ACTIVE)
		)
	      )
	    (setf (elite-ai-commander-status commander) tmp)
	    (setq markets (append markets (list (list local-system-number local-market))))

	    ;;list of people in current system
	    ;;(setq local-system-number 4)
	    (setq people-in-system (elite-get-people-in-system local-system-number))
	    (setq local-system-name (elite-get-system-name local-system-number))
	    (setq system-log-persons (elite-ai-commander-system-log-persons commander))
	    (while people-in-system
	      ;;(setq system-log-persons nil)
	      (setq tmp (elite-person-name (car people-in-system)))
	      (setq system-log-persons (append system-log-persons (list (list tmp local-system-name elite-day)) nil))
	      ;;(assoc local-system-name system-log-persons)
	      (setq people-in-system (cdr people-in-system))
	      )
	    (setf (elite-ai-commander-system-log-persons commander) system-log-persons)
	    ;;(assoc "key" '(("key" . "value")))

	    )
	)


      ;;(elite-ai-commander-planet-visited commander (elite-ai-commander-current-planet commander))
    ;;  (setq commanders (cdr commanders))
     ;; )

    )
  )


(defun elite-ai-commander-hyperspace-to-next-planet (commander)
  "Find commanders' next planet and do hyperjump"
  (let (
	(hyper-to-planet nil)
	(i)
	)
;;     (setq commander-list elite-ai-commander-list)
;;     (while commander-list
    ;;(setq commander (car commander-list))
      (setq hyper-to-planet (elite-ai-commander-find-next-planet commander))
      ;;if hyper-to-planet nil
      (if hyper-to-planet
	  (progn
	    ;;hyperspace to planet
	    (setf (elite-ai-commander-fuel commander) (- (elite-ai-commander-fuel commander) (distance (aref galaxy hyper-to-planet) (aref galaxy (elite-ai-commander-current-planet commander)))))
	    (setf (elite-ai-commander-current-planet commander) hyper-to-planet)
	    )
	)
      hyper-to-planet	    
      ;;(setq commander-list (cdr commander-list))
     ;; )
    )
  )

(defun elite-ai-commander-find-next-planet (commander)
  "Returns next planet where to jump"
  (let (
	(current-planet)
	(current-planet-info)
	(current-planet-economy)
	(hyper-to-planet-info)
	(hyper-to-planet)
	(hyper-to-planet-economy)
	(commander-fuel)
	(local-systems)
	(i)
	(len)
	(distance)
	(dist)
	)
    (setq current-planet (elite-ai-commander-current-planet commander))
    (setq current-planet-info (aref galaxy current-planet))
    (setq current-planet-economy (plansys-economy current-planet-info))
    (setq commander-fuel (elite-ai-commander-fuel commander))

    (setq local-systems (elite-systems-within-radius commander-fuel current-planet))
    (setq len (length local-systems))

    ;;occasianally random destination, should not be random...make to do
    ;;passenger delivery, passenger delivery leads to unknow destination
    (if (and (> len 0) (> (elite-rand1 t) 230))
	(progn	 
	  (setq hyper-to-planet (aref local-systems (random len)))
	  )
    (progn
      (setq hyper-to-planet-economy current-planet-economy)
      (setq i 0)
      (while (< i len)
	;;(setq i (1+ i))
	(setq hyper-to-planet-info (aref galaxy (aref local-systems i)))
	(if (>= current-planet-economy 4)
	    (progn 
	      ;;find planet where economy is less that current planet economy
	      (if (< (plansys-economy hyper-to-planet-info) current-planet-economy)
		  (progn
		    (if (and (not (elite-ai-commander-planet-visited commander (aref local-systems i))) (< (plansys-economy hyper-to-planet-info) hyper-to-planet-economy))
			(progn
			  (setq hyper-to-planet-economy (plansys-economy hyper-to-planet-info))
			  (setq hyper-to-planet (aref local-systems i))		  
			  )
		      )
		    )
		)
	      )
	  (progn
	    ;;find planet where economy is greater that current planet economy
	    (if (> (plansys-economy hyper-to-planet-info) current-planet-economy)
		(progn
		  (if (and (not (elite-ai-commander-planet-visited commander (aref local-systems i))) (> (plansys-economy hyper-to-planet-info) hyper-to-planet-economy))
		      (progn
			(setq hyper-to-planet-economy (plansys-economy hyper-to-planet-info))
			(setq hyper-to-planet (aref local-systems i))		  
			)
		    )
		  )
	      )
	    )
	  )
	(setq i (1+ i))
	)
      ;;if none found get planet which is farthest away
      ;;todo, find planet within 14 ly where profit could be made
      (if (= hyper-to-planet-economy current-planet-economy)
	  (progn
	    ;;find planet farthest away
	    (setq distance 0)
	    (setq i 0)
	    (while (< i len)
	      (setq dist (distance (aref galaxy current-planet) (aref galaxy (aref local-systems i))))
	      ;;(if (and (not (elite-ai-commander-planet-visited commander (aref local-systems i))) (> dist distance))
	      (if  (> dist distance)
		  (progn
		    (setq hyper-to-planet (aref local-systems i))
		    (setq distance dist)
		    )
		)
	      (setq i (1+ i))
	      )
	    )
	)
      )
    )
    hyper-to-planet
    )
  )

(defun elite-ai-commander-planet-visited (commander hyper-to-planet)
  "Checks if commander has visited planet recently.Return true if visited"
  (let (
	)
    ;;check money first, commander do initial profit in single traderoute
    ;;after enough money they start exploring galaxy
    (if (> (elite-ai-commander-credits commander) elite-ai-commander-credits-when-start-exploring)
	(member hyper-to-planet (elite-ai-commander-visited-systems commander))
      nil
      )
    )
  )

(defun elite-ai-commander-buy-equipment (commander)
  "Buy equipment"
  (let (
	(equipment)
	(new-equipment)
	(tech-level)
	)
    ;;commander buy equipment if commander has 20% more credits than cost of equipment
    (setq equipment (elite-ai-commander-equipment commander))
    (setq tech-level (1+ (plansys-techlevel (aref galaxy (elite-ai-commander-current-planet commander)))))

    ;;(setq equipment (list))
    ;buy fuel scoops
    (if (not (member EQUIPMENT_FUEL_SCOOPS equipment))
	(progn
	  (if (>= tech-level 5)
	      (progn
		(if (< (* 1.5 (equipitem-price (aref equipment-stock EQUIPMENT_FUEL_SCOOPS ))) (elite-ai-commander-credits commander))
		    (progn
		      (setf (elite-ai-commander-equipment commander) (append equipment (list EQUIPMENT_FUEL_SCOOPS)))
		      (setf (elite-ai-commander-credits commander) (- (elite-ai-commander-credits commander) (equipitem-price (aref equipment-stock EQUIPMENT_FUEL_SCOOPS ))))


		      )
		    )
		)
	    )
	  )
      )

    ;buy cargo hold
    (if (and (= (elite-ai-commander-holdspace commander) 20) (not (member EQUIPMENT_CARGO_BAY equipment)))
	(progn
		(if (< (* 1.5 (equipitem-price (aref equipment-stock EQUIPMENT_CARGO_BAY ))) (elite-ai-commander-credits commander))
		    (progn
		      (setf (elite-ai-commander-equipment commander) (append equipment (list EQUIPMENT_CARGO_BAY)))
		      (setf (elite-ai-commander-holdspace commander) 35)
		      (setf (elite-ai-commander-credits commander) (- (elite-ai-commander-credits commander) (equipitem-price (aref equipment-stock EQUIPMENT_CARGO_BAY ))))


		      )
		    )
	  )
	)

    )
  )

(defun elite-ai-commander-buy-fuel (commander)
  "Attempt to buy fuel-to-buy tonnes of fuel."
  (let
      (
       (f)
       (fuel)
       (equipment)
       )
    (setq f 70)
    (setq equipment (elite-ai-commander-equipment commander))
    (if  (member EQUIPMENT_FUEL_SCOOPS equipment)
	(progn
	  ;;if commander has fuel scoops (s)he refuels from sun
	  (setf (elite-ai-commander-fuel commander)  f)
	  )
      (progn
	(setq fuel (elite-ai-commander-fuel commander))
	(if (> (+ f fuel) maxfuel)
	    (setq f (- maxfuel fuel)))
	(if (> elite-fuelcost 0)
	    (if (> (* f elite-fuelcost) (elite-ai-commander-credits commander))
		(setq f (/ (elite-ai-commander-credits commander) elite-fuelcost))
	      )
	  )
	(setf (elite-ai-commander-fuel commander) (+ (elite-ai-commander-fuel commander) f))
	(setf (elite-ai-commander-credits commander) (- (elite-ai-commander-credits commander) (* elite-fuelcost f)))
	)
      f
      )    
    )
  )

(defvar elite-ai-commander-agriculture-tradegoods
  (list TRADEGOOD_FOOD
	TRADEGOOD_TEXTILES
	TRADEGOOD_LIQUOR
	TRADEGOOD_FURS
	)
  "Tradegoods that are bought in agricultural planets")

(defvar elite-ai-commander-agriculture-profit-tradegoods
  (list TRADEGOOD_FURS
	)
  "Tradegoods that are bought in agricultural planets for big profit")

(defvar elite-ai-commander-industrial-tradegoods
  (list TRADEGOOD_COMPUTERS
	TRADEGOOD_LUXURIES
	TRADEGOOD_MACHINERY
	TRADEGOOD_ALLOYS
	)
  "Tradegoods that are bought in industrial planets")

(defvar elite-ai-commander-industrial-profit-tradegoods
  (list TRADEGOOD_COMPUTERS
	TRADEGOOD_LUXURIES
	)
  "Tradegoods that are bought in industrial  planets for profit")

(defun elite-ai-commander-buy (ai-commander local-system local-market)
  "AI commander buys cargo"
  (let (
	(trade-good)
	(economy)
	(market-quantity)
	(market-price)	
	(cash)
	(max-amount)
	(cargo)
	(available-cargo-space)
	)
    (setq cargo (elite-ai-commander-current-cargo ai-commander))
    
    ;;get localsystem economy type
    ;;0       anarchy           Rich Ind
    ;;1       Feudal            Average Ind
    ;;2       Multi-gov         Poor Ind
    ;;3       Dictatorship      Mainly Ind
    ;;4       Communist         Mainly Agri
    ;;5       Confederacy       Rich Agri
    ;;6       Democracy         Average Agri
    ;;7       Corporate State   Poor agri
    (setq economy (plansys-economy local-system))

    (if (>= economy 4)
	(progn
	  ;;agriculture, items:food, textiles,liquor,furs
	  ;;set trade good
	  (if (> (elite-ai-commander-credits ai-commander) 2000)
	      (setq trade-good (nth (random (length elite-ai-commander-agriculture-profit-tradegoods)) elite-ai-commander-agriculture-profit-tradegoods  ))
	  (setq trade-good (nth (random (length elite-ai-commander-agriculture-tradegoods)) elite-ai-commander-agriculture-tradegoods  ))
	  )
	  ;;(setq trade-good TRADEGOOD_FOOD)

	  )
      (progn
	;;industrial, items:computers,luxuries,machinery,alloys
	  (if (> (elite-ai-commander-credits ai-commander) 2000)
	      (setq trade-good (nth (random (length elite-ai-commander-industrial-profit-tradegoods)) elite-ai-commander-industrial-profit-tradegoods  ))
	(setq trade-good (nth (random (length elite-ai-commander-industrial-tradegoods)) elite-ai-commander-industrial-tradegoods  ))	
	)
	  )
      )
    (setq market-quantity (aref (markettype-quantity local-market) trade-good))
    (if (> market-quantity 0)
	(progn
	  (setq market-price  (aref (markettype-price local-market) trade-good))
	  (setq cash (elite-ai-commander-credits ai-commander))
	  
	  (setq max-amount (mymin market-quantity (/ cash market-price)))

	  (setq max-amount (mymin max-amount (elite-ai-commander-holdspace ai-commander)))
	  

	  (if (> max-amount 0)
	      (progn
		;;buy

		(aset (markettype-quantity local-market) trade-good (- (aref (markettype-quantity local-market) trade-good) max-amount))
		(aset cargo trade-good (make-elite-ai-commander-cargo :quantity max-amount :purchase-price market-price))
		(setf (elite-ai-commander-holdspace ai-commander) (- (elite-ai-commander-holdspace ai-commander) max-amount))
		(setf (elite-ai-commander-current-cargo ai-commander) cargo)
		(setf (elite-ai-commander-credits ai-commander) (- (elite-ai-commander-credits ai-commander) (* max-amount market-price)))
		)
	    )
	  )
      )
    local-market
    )
  )

(defun elite-ai-commander-sell (ai-commander local-market &optional force-sell)
  "AI commander sells cargo"
  (let (
	(cargo)
	(len)
	(i)
	(item)
	(tmp)
	(qty)
	(fsell)
	)
    (setq fsell force-sell)
    (setq cargo (elite-ai-commander-current-cargo ai-commander))
    (setq len (length cargo))
    (setq i 0)
    (while (< i len)
      ;;check quantity
      ;;check if local price > purchase price ==> sell
      (setq item (aref cargo i))
      (setq qty (elite-ai-commander-cargo-quantity item))
      (if (> qty 0)
	  (progn
;; 	    (if (< (elite-ai-commander-credits ai-commander) 200)
;; 		(progn
;; 		  (setq fsell t)
;; 		  (message (concat "force sell!!"))
;; 		  (sit-for 2)
;; 
;; 		  )
;; 		)
	    (if (or fsell (>= (aref (markettype-price local-market) i) (elite-ai-commander-cargo-purchase-price item)))
		(progn
		  ;;sell goods
		  (aset cargo i (make-elite-ai-commander-cargo :quantity 0 :purchase-price 0))
		  (setf (elite-ai-commander-current-cargo ai-commander) cargo)
		  (setq tmp (* qty (aref (markettype-price local-market) i)))
		  (aset (markettype-quantity local-market) i (+ (aref (markettype-quantity local-market) i) qty))

		  ;;set ai commander cash
		  (setf (elite-ai-commander-credits ai-commander) (+ tmp (elite-ai-commander-credits ai-commander)))
		  (setf (elite-ai-commander-holdspace ai-commander) (+ (elite-ai-commander-holdspace ai-commander) qty))
		  (setf (elite-ai-commander-current-cargo ai-commander) cargo)

		  )
	      )
	    )
	)
      (setq i (1+ i))
      )
    local-market
    )
  )

(defun elite-ai-commander-in-system (system)
  "Returns AI commanders in system"
  (let (
	(commanders)
	(commander)
	(commanders-in-system)
	)
    (setq commanders-in-system nil)
    (setq commanders elite-ai-commander-list)
    (while commanders
      (setq commander (car commanders))
      ;;(setq markets (append markets (list (list local-system-number local-market))))
      (if (= (elite-ai-commander-current-planet commander) system)
	  (progn
	    (setq commanders-in-system (append commanders-in-system (list commander) ))
	    )
	  )
      (setq commanders (cdr commanders))
      )
    commanders-in-system
    )
  )
;;(elite-ai-commander-has-seen-person "Laurel Takashima" "akseli SNELLMA")
(defun elite-ai-commander-has-seen-person (commander-name person-name)
  "Return system name where commander has seen person"
  (let (
	(commanders)
	(commander)
	(system-log-persons)
	(system)
	(system-log-persons-entry)
	)
    (setq system nil)
    (setq commanders elite-ai-commander-list)
    (while commanders
      (setq commander (car commanders))
      (if (equal (elite-ai-commander-name commander) commander-name)
	  (progn
	    (setq system-log-persons (reverse (elite-ai-commander-system-log-persons commander)))
	    (while system-log-persons
	      (setq system-log-persons-entry (car system-log-persons))
	      (if (equal (downcase (car system-log-persons-entry)) (downcase person-name))
		  (setq system (cdr system-log-persons-entry))
		  )	      
	      (setq system-log-persons (cdr system-log-persons))
	      )
	    )
	)      
      (setq commanders (cdr commanders))
      )
    system
    )
  )


(defun elite-ai-commander-rendezvous ()
  "AI commander rendezvous in the same station"
  (let (
	(commanders)
	(commander)
	(local-planet)
	(other-commanders)
	(other-commander)
	(systems-checked (list));list of systems that have been checked for other commanders
	)
    (setq commanders elite-ai-commander-list)
    (while commanders
      (setq commander (car commanders))
      (setq local-planet (elite-ai-commander-current-planet commander))
      (if (not (member local-planet systems-checked))
	  (progn
	    ;;find other commanders in the same station
	    (setq other-commanders (elite-ai-commander-in-system local-planet))
	    (if (= (length other-commanders) 2)
		(progn
		  ;;only one other commander in the station
		  (setq other-commander (car other-commanders))
		  (if (equal (elite-ai-commander-name commander) (elite-ai-commander-name other-commander))
		      (setq other-commander (cadr other-commanders))
		    )
		  
		  ;;if male and female
		  (if (= (+ (elite-ai-commander-gender commander) (elite-ai-commander-gender other-commander)) 1)
		      (progn
			(let (
			      (male-commander)
			      (female-commander)
			      (male-genetic-line)
			      (female-genetic-line)
			      (new-commander)
			      )
			  ;;male and female ==> possible procreation
			  (if (= (elite-ai-commander-gender commander) 1)
			      (progn
				(setq male-commander commander)
				(setq female-commander other-commander)
				)
			    (progn
			      (setq male-commander other-commander)
			      (setq female-commander commander)
			      )
			    )			  
			  ;;check if commanders are already mates
			  (if (not (member (elite-ai-commander-id male-commander) (elite-ai-commander-mates female-commander)))
			      (progn
				;;check genetic line, do not procreate with any one who is ancestor or child of commander
				(setq male-genetic-line (elite-ai-commander-genetic-line-father male-commander))
				(setq female-genetic-line (elite-ai-commander-genetic-line-mother male-commander))
				;;if female commander is part of males genetic heritage==> do not procreate
				(if (not (or (member (elite-ai-commander-id female-commander) male-genetic-line) (member (elite-ai-commander-id female-commander) female-genetic-line)))
				    (progn
				      ;;male is not part of females genetic heritage

				      (setq male-genetic-line (elite-ai-commander-genetic-line-father female-commander))
				      (setq female-genetic-line (elite-ai-commander-genetic-line-mother female-commander))
				      ;;if male commander is part of females genetic heritage==> do not procreate
				      (if (not (or (member (elite-ai-commander-id male-commander) male-genetic-line) (member (elite-ai-commander-id male-commander) female-genetic-line)))
					  (progn
					    ;;female is not part of males genetic heritage
					    ;;procreate if males and females willingness >threshold and females willingness is greate than males willingness
					    ;;(if t
					    (if (and (> (elite-ai-commander-willingness-to-procreate female-commander) elite-ai-commander-willingness-to-procreate-threshold) (> (elite-ai-commander-willingness-to-procreate male-commander) elite-ai-commander-willingness-to-procreate-threshold) (> (elite-ai-commander-willingness-to-procreate female-commander) (elite-ai-commander-willingness-to-procreate male-commander)))
						(progn
						  (let (
							(name);;if new commander gender is female, first name from mother. if gender male first name from father
							(gender)
							(heritage)
							(genetic-line-father)
							(genetic-line-mother)
							(tmp)
							)
						    (setq gender (random 2))
						    (setq name (elite-ai-commander-get-offspring-name gender (elite-ai-commander-name female-commander) (elite-ai-commander-name male-commander)))
;; 						    (if (= gender ELITE_GENDER_FEMALE)
;; 							(progn
;; 							  ;;(setq heritage "Daughter of ")
;; 								(setq tmp (split-string (elite-ai-commander-name female-commander) " "))
;; 								(setq name (car tmp))
;; 								(setq tmp (split-string (elite-ai-commander-name male-commander) " "))
;; 								(if (cdr tmp)
;; 								    (setq name (concat name " " (cadr tmp)))
;; 								  (setq name (concat name " " (car tmp)))
;; 								  )
;; 								
;; 								)
;; 						      (progn
;; 							;;(setq heritage "Son of ")
;; 							(setq tmp (split-string (elite-ai-commander-name male-commander) " "))
;; 							(setq name (car tmp))
;; 							(setq tmp (split-string (elite-ai-commander-name female-commander) " "))
;; 							(if (cdr tmp)
;; 							    (setq name (concat name " " (cadr tmp)))
;; 								(setq name (concat name " " (car tmp)))
;; 								)
;; 							
;; 							)
;; 						      )
						    (setq heritage (elite-ai-commander-get-offspring-heritage gender (elite-ai-commander-name female-commander) (elite-ai-commander-name male-commander)))
						    ;;(setq heritage (concat heritage (elite-ai-commander-name male-commander) " and " (elite-ai-commander-name female-commander)))
						    (setq genetic-line-father (append (elite-ai-commander-genetic-line-father male-commander) (elite-ai-commander-genetic-line-mother male-commander) (list (elite-ai-commander-id male-commander))))
						    (setq genetic-line-mother (append (elite-ai-commander-genetic-line-father female-commander) (elite-ai-commander-genetic-line-mother female-commander) (list (elite-ai-commander-id female-commander))))
						    ;;new ai commander
						    ;;check males/females mother-father genetic lines ==> relatives are not allowed to procreate
						    (if (not (elite-list-has-same-element genetic-line-mother genetic-line-father))
							(progn
							  (setq new-commander (elite-ai-commander-create name gender heritage genetic-line-father genetic-line-mother (elite-ai-commander-current-planet male-commander)))
							  (elite-ai-commander-set-mates (elite-ai-commander-id male-commander) (elite-ai-commander-id female-commander) (elite-ai-commander-id female-commander) (elite-ai-commander-id male-commander))
							  (setq elite-federation-commander-born (append elite-federation-commander-born (list (elite-ai-commander-id new-commander))))
							  ;;show message in message window
							  )
						      )						  
						    )
						  )
					      )
					    )
					)
				      )
				  )
				)
			    )
			  )
			)
		    )
		  )
	      )	    
	    (setq systems-checked (append systems-checked (list local-planet)))
	    )
	)
      (setq commanders (cdr commanders))
      )
    )
  )

(defun elite-ai-commander-get-offspring-name (gender mother-name father-name)
  (let (
	(tmp)
	(name)
	)
    (if (= gender ELITE_GENDER_FEMALE)
	(progn
	  (setq tmp (split-string mother-name " "))
	  (setq name (car tmp))
	  (setq tmp (split-string father-name " "))
	  (if (cdr tmp)
	      (setq name (concat name " " (cadr tmp)))
	    (setq name (concat name " " (car tmp)))
	    )
	  
	  )
      (progn
	(setq tmp (split-string father-name " "))
	(setq name (car tmp))
	(setq tmp (split-string mother-name " "))
	(if (cdr tmp)
	    (setq name (concat name " " (cadr tmp)))
	  (setq name (concat name " " (car tmp)))
	  )	
	)
      )
    name
    )
  )

(defun elite-ai-commander-get-offspring-heritage (gender mother-name father-name)
  (let (
	(heritage)
	)
    (if (= gender ELITE_GENDER_FEMALE)
	(setq heritage "Daughter of ")
      (setq heritage "Son of ")
      )
    (concat heritage father-name " and " mother-name)
    )
)

(defun elite-ai-commander-offspring-with-player (ai-commander)
  "Player has an offspring with ai commander"
  (let (
	(male-genetic-line)
	(female-genetic-line)
	(genetic-line-father)
	(genetic-line-mother)
	(gender)
	(name)
	(heritage)
	(new-commander)
	(result)
	(tmp)
	)
    (setq male-genetic-line (elite-ai-commander-genetic-line-father ai-commander))
    (setq female-genetic-line (elite-ai-commander-genetic-line-mother ai-commander))

    (if (not (member (commander-id elite-commander) (elite-ai-commander-mates ai-commander)))
	(progn
	  (if (not (or (member (commander-id elite-commander) male-genetic-line) (member (commander-id elite-commander) female-genetic-line)))
	      (progn
		(setq gender (random 2))
		;;make offspring
		(if (= (commander-gender elite-commander) ELITE_GENDER_MALE)
		    (progn
		      (setq genetic-line-father (list (commander-id elite-commander)))
		      (setq genetic-line-mother (append (elite-ai-commander-genetic-line-father ai-commander) (elite-ai-commander-genetic-line-mother ai-commander) (list (elite-ai-commander-id ai-commander))))
		      (setq name (elite-ai-commander-get-offspring-name gender (elite-ai-commander-name ai-commander) (commander-name elite-commander) ) )
		      (setq heritage (elite-ai-commander-get-offspring-heritage gender (elite-ai-commander-name ai-commander) (commander-name elite-commander) ) )
		      )
		  (progn
		      (setq genetic-line-mother (list (commander-id elite-commander)))
		      (setq genetic-line-father (append (elite-ai-commander-genetic-line-father ai-commander) (elite-ai-commander-genetic-line-mother ai-commander) (list (elite-ai-commander-id ai-commander))))
		      (setq name (elite-ai-commander-get-offspring-name gender (commander-name elite-commander) (elite-ai-commander-name ai-commander)))
		      (setq heritage (elite-ai-commander-get-offspring-heritage gender (commander-name elite-commander) (elite-ai-commander-name ai-commander)))
		    )
		  )
		(setq new-commander (elite-ai-commander-create 
				     name
				     gender 
				     heritage 
				     genetic-line-father 
				     genetic-line-mother 
				     currentplanet
				     )
		      )
		;;set commander mates
		(setf (commander-mates elite-commander) (append (commander-mates elite-commander) (list (elite-ai-commander-id ai-commander))))
		;;set ai commander mate
		(setf (elite-ai-commander-mates (nth (elite-ai-commander-id ai-commander) elite-ai-commander-list))(append (elite-ai-commander-mates ai-commander) (list (commander-id elite-commander))))
		(setq elite-federation-commander-born (append elite-federation-commander-born (list (elite-ai-commander-id new-commander))))

		(if (= gender ELITE_GENDER_FEMALE)
		    (setq tmp "daughter")
		  (setq tmp "son")
		  )
		(setq result (concat 
			    ">Message from Tleilax facilities: "
			    "Congratulations. "
			    "Your genetic line continues a healthy "
			    tmp
			    ".\nHave a nice day."
			    )
		    )
		)
	    (progn
	      (setq result (concat 
			    ">Message from Tleilax facilities: "
			    (commander-name elite-commander)
			    ", you are ancestor of "
			    (elite-ai-commander-name ai-commander)
			    ". Law forbids your offspring.\nHave a nice day."
			    )
		    )
	      )
	    )
	  )
      (progn
	(setq result (concat 
		      ">Message from Tleilax facilities: "
		      (commander-name elite-commander)
		      " and "
		      (elite-ai-commander-name ai-commander)
		      ", you have already mated."
		      ". Law forbids your offspring.\nHave a nice day."
		      )
	      )
	)
      )
    result
    )
  )


(defvar elite-ai-commander-set-willingness-counter 1)
(defvar elite-ai-commander-set-willingness-interval 10)
;;(setq elite-ai-commander-set-willingness-interval 10000)

(defun elite-ai-commander-set-willingness-to-procreate ()
  "Set commanders willingness to procreate"
  (let (
	(commanders)
	(commander)
	)
    (if (= elite-ai-commander-set-willingness-counter elite-ai-commander-set-willingness-interval)
	(progn
	  (setq commanders elite-ai-commander-list)
	  (while commanders
	    (setq commander (car commanders))
	    ;;should be rules for generating willingness...
	    (setf (elite-ai-commander-willingness-to-procreate commander) (elite-rand1 t))
	    (setq commanders (cdr commanders))
	    )
	  (setq elite-ai-commander-set-willingness-counter 1)
	  )
      (progn
	(setq elite-ai-commander-set-willingness-counter (1+ elite-ai-commander-set-willingness-counter))
	)
      )
    )
  )


(defun elite-ai-commander-info ()
  "Display ai commander info"
  (interactive)
  (let (
	(commanders)
	(commander)
	(txt ">Elite Federation commanders\n")
	)
    (if (not (elite-is-docked))
	(progn
	  (elite-insert-text (concat txt "Not docked.\n"))
	  )
      (progn
	;;info: name,credits,current planet,current galaxy
	(setq commanders elite-ai-commander-list)
	(while commanders
	  (setq commander (car commanders))
	  (setq txt (concat txt (elite-ai-commander-name commander) "\n" ) )
	  (if (not (equal (elite-ai-commander-heritage commander) ""))
	      (setq txt (concat txt (elite-ai-commander-heritage commander) " " "\n" ) )	  
	    )
	  (setq txt (concat txt "  " (elite-get-system-name (elite-ai-commander-current-planet commander)) (format " %.1f CR" (/ (elite-ai-commander-credits commander) 10.0)) "\n" ) )
	  (setq commanders (cdr commanders))
	  )
	(elite-insert-text txt)
	)
      )
    )
  )

(defun elite-ai-commander-richest ()
  "Displays commanders 'highscore' list, richest first"
  (let (

	)

    )
)

(defvar elite-ai-commander-credits-when-start-exploring 20000
;;(setq elite-ai-commander-credits-when-start-exploring 200)
  "Commanders do single traderoute until they have this amount of credits")

(defun elite-ai-commander-generate (commander-id)
  "Generate ai commander, used during initialisation"
  (let (
	(local-planet)
	)
    ;;(setq local-planet numforLave)
    (setq local-planet (random galsize))
    (make-elite-ai-commander
     :id commander-id
     :name (aref elite-ai-commander-names commander-id);;(concat "AI Commander " (number-to-string commander-id));;todo commander name
     :gender (aref elite-ai-commander-genders commander-id)
     :heritage ""
     :genetic-line-father (list);ids of fathers
     :genetic-line-mother (list);ids of mothers
     :willingness-to-procreate (elite-rand1 t)
     :mates (list)
     :birthday 0
     :birthplace local-planet
     :credits 1000
     :fuel maxfuel
     :equipment (list)
     :status AI_COMMANDER_STATUS_ACTIVE
     :holdspace 20
     :current-cargo (make-vector (+ lasttrade 1) (make-elite-ai-commander-cargo :quantity 0 :purchase-price 0))
     :current-galaxy galaxynum;;todo random galaxy
     :current-planet local-planet
     :system-log-persons (list)
     :visited-systems (make-list 10 -1)
     )
    )
  )

(defun elite-ai-commander-create (name gender heritage genetic-line-father genetic-line-mother local-planet)
  "Creates AI commander, used when male and female commander creates new commander"
  (let (
	(new-commander)
	)
    (setq new-commander
	  (make-elite-ai-commander
	   :id (length elite-ai-commander-list)
	   :name name
	   :gender gender
	   :heritage heritage
	   :genetic-line-father genetic-line-father 
	   :genetic-line-mother genetic-line-mother 
	   :willingness-to-procreate (elite-rand1 t)
	   :mates (list)
	   :birthday elite-day
	   :birthplace local-planet
	   :credits 1000
	   :fuel maxfuel
	   :equipment (list)
	   :status AI_COMMANDER_STATUS_ACTIVE
	   :holdspace 20
	   :current-cargo (make-vector (+ lasttrade 1) (make-elite-ai-commander-cargo :quantity 0 :purchase-price 0))
	   :current-galaxy galaxynum;;todo random galaxy
	   :current-planet local-planet
	   :system-log-persons (list)
	   :visited-systems (make-list 10 -1)
	   )
	  )
    (setq elite-ai-commander-list (append elite-ai-commander-list (list new-commander)))
    new-commander
    )     
  )

(defun elite-ai-commander-set-mates (id1 mate1 id2 mate2)
  "Set mates list for commander"
  (let (

	)
    (setf (elite-ai-commander-mates (nth id1 elite-ai-commander-list)) (append (elite-ai-commander-mates (nth id1 elite-ai-commander-list)) (list mate1)))
    (setf (elite-ai-commander-mates (nth id2 elite-ai-commander-list)) (append (elite-ai-commander-mates (nth id2 elite-ai-commander-list)) (list mate2)))
    )
  )

;;(length (make-vector (+ lasttrade 1) (make-elite-ai-commander-cargo :quantity 0 :purchase-price 0)))

(defstruct elite-ai-commander-cargo
  quantity
  purchase-price
  )

(defstruct elite-ai-commander
  id
  name
  gender;0=female,1=male
  heritage;heritage like son of dart vader and mara jade
  genetic-line-father;ids of fathers
  genetic-line-mother;ids of mothers
  willingness-to-procreate;commander's willingness to procreate, if females willingness greater than males==>procreate
  mates;list of mates with whom commander has children
  birthday
  birthplace
  credits
  fuel
  equipment;;list of equipment in commander's CobraMKIII
  status;;commanders status can be any string
  holdspace
  current-galaxy
  current-planet
  visited-systems;list of system numbers where commander has been,hold up to 5-10 systems.
                 ;;commander does not go to system which number is in list
  system-log-persons;;for each system show people in system and other stuff
  current-cargo
  )

(defconst AI_COMMANDER_STATUS_ACTIVE 0)
(defconst AI_COMMANDER_STATUS_STUCK 1)

;;(elite-ai-commander-display-info)
(defun elite-ai-commander-display-info ()
  "Displays ai commander info. Mainly for debugging/testing."
  (let (
	(commander-list)
	(commander)
	(info)
	(i)
	(tmp)
	)
    (setq info "")
    (setq commander-list elite-ai-commander-list)
    (while commander-list
      (setq commander (car commander-list))
      (setq info (concat info 
			 "ID: " (number-to-string (elite-ai-commander-id commander)) "\n"
			 "Name: " (elite-ai-commander-name commander) "\n"
			 "Heritage: " (elite-ai-commander-heritage commander) "\n"
			 "Birthday: " (number-to-string (elite-ai-commander-birthday commander)) "\n"
			 "Birthplace: " (elite-get-system-name (elite-ai-commander-birthplace commander)) "(" (number-to-string (elite-ai-commander-current-planet commander)) ")" "\n"
			 "Willingness to procreate: " (number-to-string (elite-ai-commander-willingness-to-procreate commander)) "\n"
			 "Credits: " (format "%.1f" (/ (elite-ai-commander-credits commander) 10)) "\n"
			 "Status: " (number-to-string (elite-ai-commander-status commander)) "\n"
			 "Fuel: " (number-to-string (elite-ai-commander-fuel commander)) "\n"
			 "Holdspace: " (number-to-string (elite-ai-commander-holdspace commander)) "\n"
			 "Galaxy: " (number-to-string (elite-ai-commander-current-galaxy commander)) "\n"
			 "Planet: " (elite-get-system-name (elite-ai-commander-current-planet commander)) "(" (number-to-string (elite-ai-commander-current-planet commander))")" "\n"
			 )
	    )
      ;;mates
      (setq info (concat info "Mates: "))
      (setq tmp (elite-ai-commander-mates commander))
      (while tmp
	(setq info (concat info (number-to-string (car tmp))  " "))
	(setq tmp (cdr tmp))
	)
      (setq info (concat info "\n"))

      ;;genetic line male
      (setq info (concat info "Genetic line father: "))
      (setq tmp (elite-ai-commander-genetic-line-father commander))
      (while tmp
	(setq info (concat info (number-to-string (car tmp))  " "))
	(setq tmp (cdr tmp))
	)
      (setq info (concat info "\n"))

      ;;genetic line male
      (setq info (concat info "Genetic line mother: "))
      (setq tmp (elite-ai-commander-genetic-line-mother commander))
      (while tmp
	(setq info (concat info (number-to-string (car tmp))  " "))
	(setq tmp (cdr tmp))
	)
      (setq info (concat info "\n"))

      ;;equipment
      (setq info (concat info "Equipment: "))
      (setq tmp (elite-ai-commander-equipment commander))
      (while tmp
	(setq info (concat info (equipitem-name (aref equipment-stock (car tmp) )) " "))
	(setq tmp (cdr tmp))
	)
      (setq info (concat info "\n"))

      ;;visited planets
      (setq info (concat info "Visited planets: "))
      (setq tmp (elite-ai-commander-visited-systems commander))
      (while tmp
	(if (> (car tmp) -1)
	    (setq info (concat info
			       (elite-get-system-name (car tmp))
			       " "
			       ))
	  )
	(setq tmp (cdr tmp))
	)
      (setq info (concat info "\n"))
      ;;commander cargo
      (setq i 0)
      (while (< i (length (elite-ai-commander-current-cargo commander)))
	(if (> (elite-ai-commander-cargo-quantity (aref (elite-ai-commander-current-cargo commander) i)) 0)
	    (setq info (concat info
			       (tradegood-name (aref commodities i))
			       ": "
			       (number-to-string (elite-ai-commander-cargo-quantity (aref (elite-ai-commander-current-cargo commander) i)))
			       " Purchase price: "
			       (number-to-string (elite-ai-commander-cargo-purchase-price (aref (elite-ai-commander-current-cargo commander) i)))
			       "\n"
			       )
		  )
	  )
	(setq i (1+ i))
	)
      (setq info (concat info "\n"))      
      (setq commander-list (cdr commander-list))
      )
    (with-electric-help
     '(lambda () (insert info) (goto-char 0) "*Elite AI Commander info*")
     )    
    )
  )

(defun elite-ai-set-commander-names ()

  (setq elite-ai-commander-genders
;; 	(vconcat (make-vector 1 ELITE_GENDER_MALE)
;; 		(make-vector 1 ELITE_GENDER_FEMALE)
	(vconcat (make-vector 39 ELITE_GENDER_MALE)
		(make-vector 25 ELITE_GENDER_FEMALE)
	 )
	)
  (setq elite-ai-commander-names
	(vector
	 ;;names should have first name and last name, or just singular name
	 "Darth Vader";;first in list are males
	 "Darth Maul"
	 "Darth Sidious"
	 "Luke Skywalker"
	 "Han Solo"
	 "Korben Dallas"
	 "Jean-Luc Picard"
	 "Protector"
	 "Silenius"
	 "Paul Atreides"
	 "Clint Eastwood"
	 "Edmund Blackadder"
	 "John Sheridan"
	 "Baldrick"
	 "Bruce Wayne"
	 "Zack Allan"
	 "Harley Davidson"
	 "Alec Holland"
	 "John Constantine"
	 "Etrigan"
	 "Michael Garibaldi"
	 "Clark Kent"
	 "Conan"
	 "William Riker"
	 "Ben Kenobi"
	 "Jeffrey Sinclair"
	 "Morden"
	 "Londo Mollari"
	 "G'Kar"
	 "Worf"
	 "Garrett"
	 "Vladimir Harkonnen"
	 "Duncan Idaho"
	 "John Connor"
	 "Kyle Reese"
	 "Thor"
	 "Odin"
	 "Jack Slater"
	 "Kyle Katarn"
	 "Shodan";;here start females
	 "Chani"
	 "Abigail Arcane"
	 "Susan Ivanova"
	 "Elektra"
	 "Beverly Crusher"
	 "Ellen Ripley"
	 "Lyta Alexander"
	 "Marion Sherwood"
	 "Valeria"
	 "Martha Washington"
	 "Kathryn Janeway"
	 "Leia Organa"
	 "Meredith Argent";FFE
	 "Lorelei"
	 "Delenn"
	 "Sarah Connor"
	 "Sif"
	 "Amidala"
	 "Jessica Atreides"
	 "Mara Jade"
	 "Talia Winters"
	 "Elizabeth Lochley"
	 "Catherine Sakai"
	 "Laurel Takashima"
	 )
	)
  )
;; garrett mara jade 
;;boy garrett jade
:;daughter: mara garrett
;; boy: Garrett, son of Mara Jade
;; girl: Mara, daughther of Garret
  
(defvar elite-ai-commander-names nil
  "AI Commander names")

(defvar elite-ai-commander-genders nil
  "AI Commander genders")

(defvar elite-ai-commander-willingness-to-procreate-threshold 140
  "Threshold value for willingness, if commanders willingness > this value (s)he is willing to procreate")
;;(setq elite-ai-commander-willingness-to-procreate-threshold 0)

;;(elite-ai-commander-write-path-file)
(defun elite-ai-commander-write-path-file ()
  "Write path file to for external program that show how commanders a roaming in the galaxy"
  (let (
	(commanders-path)
	(commander-path)
	(name)
	(path)
	(system)
	)
    (find-file "~/commanders.csv")
    (erase-buffer)

    (setq commanders-path elite-ai-commander-path)
    (while commanders-path
      (setq commander-path (car commanders-path))
      (setq name (car commander-path))
      (setq path (car (cdr commander-path)))
      ;;write csv
      (while path
	(insert name ",")
	(setq system (car path))
	(insert (number-to-string system) ",")
	(insert (elite-get-system-name system) ",")
	(insert	(number-to-string (plansys-x (aref galaxy system ))) ",")
	(insert	(number-to-string (plansys-y (aref galaxy system ))) ",")
	(insert (aref econnames (plansys-economy (aref galaxy system ))) "\n")
	(setq path (cdr path))
	)
      (setq commanders-path (cdr commanders-path))
      )
    (save-buffer)
    (kill-buffer (current-buffer))
    )
  )

(provide 'elite-ai-commanders)