;;; elite-for-emacs-commands.el - Elite for EMACS commands

;; Elite for EMACS is based on Elite series by Ian Bell and David Braben.
;; Original Elite, (C) 1984 Ian Bell and David Braben.
;; Elite for EMACS uses code from original Elite and it is (C) 1984 Ian Bell and David Braben.
;; Additional code
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

;; Commentary:

(defvar elite-for-emacs-game-is-on nil
  "")

;;(defvar elite-for-emacs-current-state nil
;;  "what state are we")

(defvar elite-for-emacs-pilot-welcome-message
  ;;;(setq elite-pilot-welcome-message
  "Welcome to the Elite Federation of Pilots. You are now one of The Few in The Eight Galaxies. All pilots strive to Elite, few succeed and most die.\nPilot your Cobra MkIII with honor.\nQ'Apla!"
  )

(defun elite-for-emacs-prompt ()
  (let (
	(prompt)
	(cmdr)
	)
    (if elite-for-emacs-game-is-on
      (progn
	(setq cmdr (car elite-for-emacs-commander-list))
	
	(if (eq (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list))  STATE_BAZAAR)
	    (progn
	      (setq prompt (elite-for-emacs-bazaar-prompt))
	      )
	  (progn
	    
	    (setq prompt
		  (concat
		   (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr))
		   " "
		   (format "%.1f" (/ (elite-for-emacs-commander-credits cmdr) 10.0))
		   ">"
		   )
		  )
	    )
	  )
	)
      (progn
	(setq prompt "Elite>")
	)
      )
    prompt
    )
  )

(defun elite-for-emacs-mode-line ()
  (let (
	(modeline)
	)
    ;;custome modeline format
    (if elite-for-emacs-game-is-on
      (progn
	(setq cmdr (car elite-for-emacs-commander-list))
	(setq modeline (list "---"
	  ;;'elite-for-emacs-command
	      ;;"Elite for EMACS "
	      ;;'elite-for-emacs-version
	      "Condition: "
	      (elite-for-emacs-commander-condition (car elite-for-emacs-commander-list))
	      " "
	      "Day: "
	      (number-to-string (elite-for-emacs-commander-current-day cmdr))
	      " "
	      (elite-for-emacs-short-local-system-info (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) t)
	      )
	      )
	)
      (progn
	(setq modeline (list "---"
	  ;;'elite-for-emacs-command
	      "Elite for EMACS "
	      'elite-for-emacs-version
	      ))

	)
      )

    (if elite-for-emacs-online
	(progn
	  (append modeline (list
			    "-*ONLINE*"
			    "-%-"))
	  )
      (progn
	(append modeline (list "-%-"))
	)
      )

    )
  )

(defun elite-for-emacs-post-command ()
  "What to do after command is executed"
  (let (

	)
    (if elite-for-emacs-game-is-on
	(progn
	  (if (eq this-command 'newline)
	      (progn
		(elite-for-emacs-set-command-list)
		)
	    )

	  (if (eq this-command 'yank)
	      (progn
		;;check yank command, if it was script act accordingly
		(if (= (string-match "script" (car kill-ring)) 0)
		    (setq elite-for-emacs-command (car kill-ring))
		  ;;(elite-for-emacs-script-execute elite-for-emacs-command-output-string)
		  )
		)
	    )
	  )
      )
    )
  )

(defun elite-for-emacs-set-command-list ()
  (let (

	)
    (if (eq (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list))  STATE_IN_SPACE)
	(progn
	  ;;set commands while in space
	  (setq elite-for-emacs-command-list
		(append
			(list
			 (list "commander-info" 'elite-for-emacs-commander-info)
			 (list "local-systems" 'elite-for-emacs-local-systems)
			 (list "dock" 'elite-for-emacs-dock)
			 (list "inventory" 'elite-for-emacs-inventory)
			 (list "hyperspace" 'elite-for-emacs-hyperspace)
			 (list "galaxy-systems" 'elite-for-emacs-list-galaxy-reverse)
			 (list "galaxy-systems-reverse" 'elite-for-emacs-list-galaxy)
			 (list "galactic-hyperspace" 'elite-for-emacs-galactic-hyperspace)
			 (list "system-info" 'elite-for-emacs-system-info)
			 (list "path" 'elite-for-emacs-path-to-system)
			 )
			elite-for-emacs-base-command-list
			)
		)
	  )
      )

    (if (eq (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_DOCKED)
	(progn
	  ;;set commands while in docked
	  (setq elite-for-emacs-command-list
		(append
			(list
			 (list "market" 'elite-for-emacs-market-info)
			 (list "commander-info" 'elite-for-emacs-commander-info)
			 (list "local-systems" 'elite-for-emacs-local-systems)
			 (list "list-equipment" 'elite-for-emacs-list-equipment)
			 (list "buy-goods" 'elite-for-emacs-buy-goods)
			 (list "buy-equipment" 'elite-for-emacs-buy-equipment)
			 (list "sell-goods" 'elite-for-emacs-sell-goods)
			 (list "sell-all" 'elite-for-emacs-sell-all)
			 (list "refuel" 'elite-for-emacs-refuel)
			 (list "galaxy-systems" 'elite-for-emacs-list-galaxy-reverse)
			 (list "galaxy-systems-reverse" 'elite-for-emacs-list-galaxy)
			 (list "inventory" 'elite-for-emacs-inventory)
			 (list "undock" 'elite-for-emacs-undock)
			 (list "system-info" 'elite-for-emacs-system-info)
			 (list "path" 'elite-for-emacs-path-to-system)
			 (list "save" 'elite-for-emacs-save-commander)
			 (list "bazaar" 'elite-for-emacs-bazaar)
			 )
			elite-for-emacs-base-command-list
			)
		)

	  (if elite-for-emacs-online
	      (progn
		(setq elite-for-emacs-command-list
		      (append
		       elite-for-emacs-command-list
		       (list
			(list "message-board" 'elite-for-emacs-message-board)
			(list "send-message" 'elite-for-emacs-send-message)			
			(list "commanders" 'elite-for-emacs-other-commanders)			
			)
		       )
		      )
	    
		)
	    )
	  

	  )
      )

    (if (eq (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_BAZAAR)
	(progn
	  ;;set commands while in docked
	  (setq elite-for-emacs-command-list
		(append
			(list
			 (list "leave" 'elite-for-emacs-leave-bazaar)
			 )
			elite-for-emacs-base-command-list
			)
		)


	  )
      )


    )
  )

(defun elite-for-emacs-pre-command ()
  "What to do before command is executed"
  (let (

	)

    )
  )

(defun elite-for-emacs-version-info ()
  "Elite for EMACS version."
  (let (

	)
    (insert "Elite for EMACS " elite-for-emacs-version "\n")
    (insert "Based on Elite (c) 1984 Ian Bell and David Braben.\n")
    (insert "Elite for EMACS by Sami Salkosuo.")
    ;;(insert elite-for-emacs-logo)
    )
  )

(defun elite-for-emacs-logo ()
  (insert elite-for-emacs-logo)

)

(defun elite-for-emacs-logo-no-text ()
  (insert elite-for-emacs-logo-no-text)
)

(defun elite-for-emacs-changes ()
  "Changes"
  (let (

	)
    (insert "
Elite for EMACS changes

version 0.1
")
    )
  )

(defun elite-for-emacs-load-commander ()
  "Load Elite commander. Usage: load <commander name>."
  (let (
	(params)
	(buffer)
	(file-name)
	(temp)
	(temp2)
	(cmdr-vector)
	(i)
	(exists)
	)
    (setq params (cdr (split-string elite-for-emacs-command)))
    (if (/= (length params) 1)
	(insert "Usage: load <commander name>.")
      (progn
	(if elite-for-emacs-online
	    (progn
	      ;;load commander
	      ;;temp is commander data
	      ;;(setq temp (elite-for-emacs-online-load-commander "Sami"))
	      (setq temp (elite-for-emacs-online-load-commander (nth 0 params)))
	      (if (not temp)
		  (setq exists nil)
		(progn
		  ;;(insert (prin1-to-string temp))
		  (setq elite-for-emacs-online t)
		  (setq elite-for-emacs-saved-commander-list (list (car temp)))
		  (setq exists t)
		  )
		)
	      )
	  (progn
	    (setq file-name (concat "~/.elite-for-emacs-commander-" (nth 0 params)))
	    ;;(setq file-name (concat "~/.elite-for-emacs-commander-" "Sami"))
	    (if (file-exists-p file-name)
		(progn
		  (setq buffer (find-file file-name))
		  (eval-buffer buffer)
		  (kill-buffer buffer)
		  (setq exists t)
		  )
	      (setq exists nil)	      
	      )
	    )
	  )

	(if exists
	    (progn
	      (setq elite-for-emacs-game-is-on t)
	      (setq elite-for-emacs-commander-list  nil)
	      ;;todo upgrade commander struct
	      (setq temp  (make-elite-for-emacs-commander))
	      ;;(length (car elite-for-emacs-saved-commander-list))
	      ;;(length (car elite-for-emacs-commander-list))
	      ;;(length (make-elite-for-emacs-commander))

	      (while elite-for-emacs-saved-commander-list
		(setq temp2 (car elite-for-emacs-saved-commander-list))
		(if (/= (length temp) (length temp2))
		    (progn
		      ;;commander struct changed. update commander
		      (setq cmdr-vector (make-elite-for-emacs-commander))
		      (setq i 0)
		      (while (< i (length temp2))
			(aset cmdr-vector i (aref temp2 i))
			(setq i (1+ i))
			)

		      (setq elite-for-emacs-commander-list
			    (append
			     elite-for-emacs-commander-list
			     (list cmdr-vector)
			     )
			    )
		      )
		  (progn
		    (setq elite-for-emacs-commander-list
			  (append
			   elite-for-emacs-commander-list
			   (list temp2)
			   )
			  )
		    )
		  )

		(setq elite-for-emacs-saved-commander-list (cdr elite-for-emacs-saved-commander-list))
		)

	      ;;(setq elite-for-emacs-commander-list elite-for-emacs-saved-commander-list)

	      (if cmdr-vector
		  (progn
		    (insert "Commander " (nth 0 params)" updated and loaded.")
		    )
		(progn
		  (insert "Commander " (nth 0 params) " loaded.")
		  )

		)
	      )
	  (progn
	  (insert "Commander " (nth 0 params) " does not exist.")
	    
	  )
	
	  )
	
	)
      
      )
    )
  )


(defun elite-for-emacs-new-commander ()
  "Create new Elite commander. Usage: new <commander name> <male|female> [no-random]. If no-random parameter is present commander's home system is Lave."
  (let (
	(params)
	(temp)
	(current-planet)
	)
    (setq params (cdr (split-string elite-for-emacs-command)))
    (if (and (/= (length params) 2) (/= (length params) 3))
	(insert "Usage: new <commander name> <male|female> [no-random].")
      (progn
	(elite-for-emacs-reset)

	(elite-for-emacs-create-new-commander (nth 0 params) (nth 1 params) (nth 2 params))

	;;(insert (concat "Incoming message: Greetings Commander " (nth 0 params) ".\n"))
	;;(insert elite-for-emacs-pilot-welcome-message )
	(setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_DOCKED)
	(setq current-planet (elite-for-emacs-commander-current-planet (car elite-for-emacs-commander-list)))
	(if elite-for-emacs-online
	    (progn
	      ;;check commander name. if it is taken return error

	      ;;(prin1-to-string (car elite-for-emacs-commander-list))
	      (setq temp (elite-for-emacs-online-check-name (car elite-for-emacs-commander-list)))
	      (if (not (string= temp "OK"))
		  (error (concat "Commander name " (nth 0 params) " already taken. Please choose another.") )
		)
	      ;;get fluct from server
	      ;;no need because fluct is for market and all market info is fetched from server
	      ;;(setq temp (elite-for-emacs-online-fluct))
	      ;;(setf (elite-for-emacs-commander-fluct (car elite-for-emacs-commander-list)) temp)

	      ;;local market fetched from server when executing market,buy or sell
	      ;;(setf (elite-for-emacs-commander-local-market (car elite-for-emacs-commander-list))
		;;(genmarket temp (aref (elite-for-emacs-get-galaxy 0) current-planet)) )

	      ;;(prin1 (car elite-for-emacs-commander-list))
	      )
	  (progn
	    ;;
	    (setf (elite-for-emacs-commander-local-market (car elite-for-emacs-commander-list))
		(genmarket 0 (aref (elite-for-emacs-get-galaxy 0) current-planet)) )
	    (setf (elite-for-emacs-commander-fluct (car elite-for-emacs-commander-list)) 0)

	    )
	  )
	(insert "Welcome to Elite for EMACS Commander " (nth 0 params) ".")

	(setq elite-for-emacs-game-is-on t)

	)
      )

    )
  )

(defun elite-for-emacs-reset ()
  (let (

	)
    ;;set environment for game, commands etc
    (setq elite-for-emacs-commander-list nil)
    (mysrand 12345)

    ;;command list
    )
  )

(defun elite-for-emacs-undock ()
  (let (
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (if (string= (elite-for-emacs-commander-condition cmdr) CONDITION_DOCKED)
	(progn
	  (insert "Leaving station..")
	  (if elite-for-emacs-online
	      (progn
		(if elite-for-emacs-online-autosave-undock
		    (progn
		      (setq temp (elite-for-emacs-online-save-commander (car elite-for-emacs-commander-list)))
		      (if (not (string= temp "OK"))
			  (error (concat "Automatic save failed.") )
			)
		      )
		  )
		;;(insert "Commander saved.")
		
		)
	    )

	  (setf (elite-for-emacs-commander-condition cmdr) CONDITION_GREEN)
	  (setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_IN_SPACE)


	  )
      (progn
	(insert "Not docked.")
	)
	)

    )
  )

(defun elite-for-emacs-dock ()
  "Dock station."
  (let (
	(cmdr)
	(rnd-byte)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (insert "Docking station..")
    (setf (elite-for-emacs-commander-condition cmdr) CONDITION_DOCKED)
    (setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_DOCKED)
        

    (if (elite-for-emacs-commander-auto-refuel cmdr)
	(elite-for-emacs-refuel)
      )

    (if elite-for-emacs-online
	(progn
	  (if elite-for-emacs-online-autosave-dock
	      (progn
		(setq temp (elite-for-emacs-online-save-commander (car elite-for-emacs-commander-list)))
		(if (not (string= temp "OK"))
		    (error (concat "Automatic save failed.") )
		  )
		)
	    )
	  ;;(insert "Commander saved.")

	  )
      )


    )
  )

(defun elite-for-emacs-buy-goods ()
  "buy goods. Usage: buy-goods <trade item name> <amount>."
  (let (
	(params)
	(bought)
	(item)
	(item-index)
	)
    (setq params (cdr (split-string elite-for-emacs-command)))
    ;;(setq params (list "fu" 44))
    (if (/= (length params) 2)
	(insert "Usage: buy-goods <trade item name> <amount>.")
      (progn
	;;
	(condition-case error
	    (progn
	      (setq item (nth 0 params))
	      (setq item-index  (elite-for-emacs-trade-good-index item))

	      (setq bought (elite-for-emacs-gamebuy  (car elite-for-emacs-commander-list) item (string-to-number (nth 1 params))))
	      (insert "Bought " (number-to-string bought) (aref unitnames ( tradegood-units (aref commodities item-index))) " of " (aref tradnames item-index) ".")
	      )
	  (error
	   (insert
	    (error-message-string error)
	    ;;"\n"
	    "Usage: buy-goods <trade item name> <amount>.")
	   )
	  )
	;;amount to integer if not integer -> error/set to zero
	)
      )
    )
)

(defun elite-for-emacs-sell-goods ()
  "Sell goods. Usage: sell-goods <trade item name> <amount>."
  (let (
	(params)
	(amount)
	(item)
	(item-index)
	(current-cargo)
	(cmdr)
	(amount-in-hold)
	(cargo-to-sell)
	(localmarket)
	(revenue)
	(tmp)
	)
    (setq cmdr  (car elite-for-emacs-commander-list))
    (setq params (cdr (split-string elite-for-emacs-command)))
    ;;(setq params (list "fu" 44))
    (if (/= (length params) 2)
	(insert "Usage: sell-goods <trade item name> <amount>.")
      (progn
	;;
	(condition-case error
	    (progn
	      (setq item (nth 0 params))
	      (setq item-index  (elite-for-emacs-trade-good-index item))
	      (setq amount (string-to-number (nth 1 params)))
	      (setq current-cargo (elite-for-emacs-commander-current-cargo cmdr))
	      (setq amount-in-hold (aref current-cargo item-index))
	      (if (= amount-in-hold 0)
		  (insert "Nothing to sell.")
		(progn
		  ;;sell goods

		  ;;move from ships cargo
		  (setq cargo-to-sell (min amount amount-in-hold))
		  (aset current-cargo item-index (- amount-in-hold cargo-to-sell))
		  (setf (elite-for-emacs-commander-current-cargo cmdr) current-cargo)
		  ;;move to local market
		  (if elite-for-emacs-online
		      (progn
			(elite-for-emacs-online-update-local-market-sell (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) item-index cargo-to-sell)
			(setq localmarket (elite-for-emacs-online-local-market (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) ))
			)
		    (progn
		      (setq localmarket (elite-for-emacs-commander-local-market cmdr))
		      (aset (markettype-quantity localmarket) item-index (+ (aref (markettype-quantity localmarket) item-index) cargo-to-sell))		      
		      )
		    )



		  ;;todo: each ton sold decreases price 0.1CR
		  ;;todo: make gross productivity count: more productivuty more system can
		  ;;have before surplus, bigger productivity means that inhabitants can
		  ;;consume more, less productivity means that inhabitants can't affort to buy
		  ;;-->each ton sold to system decreases price and after productivity threshold
		  ;;price decreases substantially
		  ;;
		  ;;(productivity)/(population*1000)= daily salary per person (more the better)
		  ;;for example daily salary 1.998->year:719.28CR
		  ;;this much money to spend
		  ;;todo, weight for each product-> food 1.0 (everyone needs food)
		  ;; textiles 0.999 (almost everyone needs textiles)
		  ;;radioactive 0.001 (almost no one needs radioactives, (if industrial world * techlevel)only for industrial use
		  ;;liquor/wines (0.5 + economy*0.01)
		  ;;luxuries (0.1+ (econ+governm)*0.01
		  ;;computers (0.1+econ+government+tech*0.01)
		  ;;etc
		  ;;
		  ;;market price/availablility:
		  ;; base normal elite market price, todo: calculate once per month, price check based on question/demand(?)
		  ;;
		  ;;
		  ;;calculate year,day,month (year 360 days, month 30 days)
		  ;;(setq current-day 30)
		  ;;(setq e-year  (/ current-day 360))
		  ;;(setq day-in-year (- current-day (* e-year 360)))
		  ;;(setq e-month (/ day-in-year 30))
		  ;;(setq day-in-month (- day-in-year (* e-month 30)))

		  ;;set cash
		  (setq revenue (* cargo-to-sell (aref (markettype-price localmarket) item-index)))
		  (setf (elite-for-emacs-commander-credits cmdr) (+ (elite-for-emacs-commander-credits cmdr) revenue ))

		  (setf (elite-for-emacs-commander-cargo-capacity cmdr) (+ (elite-for-emacs-commander-cargo-capacity cmdr) cargo-to-sell))

		  (insert "Sold " (number-to-string cargo-to-sell) (aref unitnames ( tradegood-units (aref commodities item-index))) " of " (aref tradnames item-index) " at " (format "%.1f CR" (/ revenue 10.0)) ".")

		  (setq tmp (elite-for-emacs-commander-trade-history-current cmdr))
		  (aset tmp item-index nil)
		  (setf (elite-for-emacs-commander-trade-history-current cmdr) tmp)

		  )
		  )
	      )
	  (error
	   (insert
	    (error-message-string error)
	    ;;"\n"
	    "Usage: sell-goods <trade item name> <amount>.")
	   )
	  )
	;;amount to integer if not integer -> error/set to zero
	)
      )
    )
)

(defun elite-for-emacs-sell-all ()
  "Sell all except Gold, Platinum and Gem-Stones."
  (let (
	(amount)
	(item)
	(current-cargo)
	(cmdr)
	(amount-in-hold)
	(cargo-to-sell)
	(localmarket)
	(revenue)
	(i)
	(insert-done nil)
	(tmp)
	)
    (setq cmdr  (car elite-for-emacs-commander-list))
    (condition-case error
	(progn
	  (setq i 0)
	  ;;(insert "Sell all.")
	  (while (< i lasttrade)
	    
	    (setq current-cargo (elite-for-emacs-commander-current-cargo cmdr))
	    (setq amount-in-hold (aref current-cargo i))
	    (if (not (or (= i 13) (= i 14) (= i 15)));; do not sell gold, platinum, gem-stones
		(if (> amount-in-hold 0)
		    (progn
		      ;;sell goods
		      (if insert-done
			  (insert "\n")
			)
		      ;;move from ships cargo
		      (setq cargo-to-sell  amount-in-hold)
		      (aset current-cargo i (- amount-in-hold cargo-to-sell))
		      (setf (elite-for-emacs-commander-current-cargo cmdr) current-cargo)
		      ;;move to local market
		      
		      (if elite-for-emacs-online
			  (progn
			    (elite-for-emacs-online-update-local-market-sell (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) i cargo-to-sell)
			    (setq localmarket (elite-for-emacs-online-local-market (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) ))
			    )
			(progn
			  (setq localmarket (elite-for-emacs-commander-local-market cmdr))
			  (aset (markettype-quantity localmarket) i (+ (aref (markettype-quantity localmarket) i) cargo-to-sell))
			  )
			)
		      
		      ;;set cash
		      (setq revenue (* cargo-to-sell (aref (markettype-price localmarket) i)))
		      (setf (elite-for-emacs-commander-credits cmdr) (+ (elite-for-emacs-commander-credits cmdr) revenue ))
		      (setf (elite-for-emacs-commander-cargo-capacity cmdr) (+ (elite-for-emacs-commander-cargo-capacity cmdr) cargo-to-sell))
		      
		      (insert "Sold " (number-to-string cargo-to-sell) (aref unitnames ( tradegood-units (aref commodities i))) " of " (aref tradnames i) " at " (format "%.1f CR" (/ revenue 10.0)) ".")

		      (setq tmp (elite-for-emacs-commander-trade-history-current cmdr))
		      (aset tmp i nil)
		      (setf (elite-for-emacs-commander-trade-history-current cmdr) tmp)

		      (if (not insert-done)
			  (progn
			    (setq insert-done t)
			    ;;(insert "\n")
			    )
			)
		      )
		  )
	      )
	    (setq i (1+ i))
	    )
	  )
      (error
       (insert
	(error-message-string error)
	"\n"
	)
       )
      )
    )
  )

(defun elite-for-emacs-list-equipment ()
  "List available equipment in system."
  (let (
	(equipment-list)
	(equipment)
	(techlevel)
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq equipment-list elite-for-emacs-equipment-list)
    (insert "Equipment available in " (elite-for-emacs-get-current-system-name) ":")
    (while equipment-list
      (setq equipment (car equipment-list))
      (setq techlevel (equipment-techlevel equipment))
      (if (<= techlevel (plansys-techlevel (elite-for-emacs-get-plansys (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr))))
	  (progn
	    (insert "\n" (equipment-name equipment) " " (format "%.1fCR" (/ (equipment-price equipment) 10.0)))
	    )
	)

      (setq equipment-list (cdr equipment-list))
      )
    )
  )

(defun elite-for-emacs-buy-equipment ()
  "Buy available equipment."
  (let (
	(equipment-list)
	(equipment)
	(techlevel)
	(params)
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq params (cdr (split-string elite-for-emacs-command)))
    ;;(setq params (list "l"))
    (if (/= (length params) 1)
	(insert "Usage: buy-equipment <equipment name>.")
      (progn
	(setq equipment (elite-for-emacs-get-first-equipment-match (nth 0 params)))
	(if (not (member (equipment-id equipment) (elite-for-emacs-commander-equipment-list cmdr)))
	    (progn
	      ;;add equip to list
	      (setf (elite-for-emacs-commander-equipment-list cmdr) (append (elite-for-emacs-commander-equipment-list cmdr) (list (equipment-id equipment))))
	      ;;reduce cash
	      (setf (elite-for-emacs-commander-credits cmdr) (- (elite-for-emacs-commander-credits cmdr) (equipment-price equipment)))
	      (insert "Bought " (equipment-name equipment) " at " (format "%.1fCR" (/ (equipment-price equipment) 10.0)))
	      (if (= (equipment-id equipment) EQUIPMENT_LARGE_CARGO_BAY)
		  (progn
		    (setf (elite-for-emacs-commander-max-cargo-capacity cmdr) 35)
		    (setf (elite-for-emacs-commander-cargo-capacity cmdr) (+ (elite-for-emacs-commander-cargo-capacity cmdr) 15))

		    )
		  )

	      )
	  (progn
	    (insert "You already have " (equipment-name equipment) ".")
	    )
	  )

	)
      )

    )
  )


(defun elite-for-emacs-refuel (&optional no-msg)
  "Refuel."
  (let (
	(cmdr)
	(fuel)
	(fuel-to-buy)
	(cost)
	)
    (setq cmdr  (car elite-for-emacs-commander-list))
    (setq fuel (elite-for-emacs-commander-fuel cmdr))
    (setq fuel-to-buy (- maxfuel fuel))
    (setq cost (* fuelcost fuel-to-buy))
    (setf (elite-for-emacs-commander-fuel cmdr) maxfuel)
    (setf (elite-for-emacs-commander-credits cmdr) (- (elite-for-emacs-commander-credits cmdr) cost))
    (if (not no-msg)
	(insert (format "Bought %.1fLY fuel %.1f CR" (/ fuel-to-buy 10.0) (/ cost 10.0)))
      )

    )
  )



(defun elite-for-emacs-inventory ()
  "Show inventory."
  (let (
	(cmdr)
	(msg)
	(i)
	(commodity)
	(localmarket)
	)
  (setq i 0)
  (setq cmdr  (car elite-for-emacs-commander-list))
  (insert (format "Cash: %.1f CR" (/ (elite-for-emacs-commander-credits cmdr) 10.0)) "\n")
  (insert (format "Fuel: %.1f CR" (/ (elite-for-emacs-commander-fuel cmdr) 10.0)))

  (setq localmarket  (elite-for-emacs-commander-local-market cmdr))
  (while (<= i lasttrade)
    (setq commodity (aref commodities i))
    (if (> (aref (elite-for-emacs-commander-current-cargo cmdr) i) 0)
	(progn
	  (insert
	   "\n"
	   (tradegood-name commodity)
	   " "
	   ;;in cargo hold
	   (format "%d%s" (aref (elite-for-emacs-commander-current-cargo cmdr) i) (aref unitnames (tradegood-units commodity)))
	   )

	  ;;view trade history
	  (if (member EQUIPMENT_TRADE_HISTORY_V1 (elite-for-emacs-commander-equipment-list cmdr))
	      (progn
		(setq trade-history-current (elite-for-emacs-commander-trade-history-current cmdr))
		(if (and trade-history-current (aref trade-history-current i))
		    (progn
		      (setq trade-history-current (aref trade-history-current i))
		      (insert " Bought from "
			      (elite-for-emacs-get-system-name (nth 1 trade-history-current) (nth 0 trade-history-current))
			      " at price "
			      (format "%.1f CR" (/ (nth 5 trade-history-current) 10.0))
			      " ("
			      (format "%.1f CR" (/ (nth 4 trade-history-current) 10.0))
			      "/"
			      (aref unitnames (tradegood-units commodity))
			      ")"
			      )
		      )
		  )
		)
	    )
	  

	  )
      )
    (setq i (1+ i))
    )
    (insert  "\nCargo space: " (number-to-string (elite-for-emacs-commander-cargo-capacity cmdr)) "/" (number-to-string (elite-for-emacs-commander-max-cargo-capacity cmdr)) "t" )

  )
)
  


(defun elite-for-emacs-market-info ()
  "Show local system market info."
  (let (
	(cmdr)
	(msg)
	(i)
	(commodity)
	(localmarket)
	(in-cargo-hold)
	(market-price)
	(market-quantity)
	(total-profit 0)
	)
  (setq i 0)
  (setq cmdr  (car elite-for-emacs-commander-list))

  (if elite-for-emacs-online
      (progn
	(setq localmarket (elite-for-emacs-online-local-market (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr)))
	)
    (progn
      (setq localmarket  (elite-for-emacs-commander-local-market cmdr))
      )
    )


  (while (<= i lasttrade)
    (setq commodity (aref commodities i))

    (setq in-cargo-hold (aref (elite-for-emacs-commander-current-cargo cmdr) i))
    (setq market-price (aref (markettype-price localmarket) i))
    (setq market-quantity (aref (markettype-quantity localmarket) i))
    (insert (tradegood-name commodity)
	    " "
	    (format "%.1f" (/ market-price 10.0))
	    " "
	    (format "%d%s" market-quantity (aref unitnames (tradegood-units commodity)))
	    ;;in cargo hold
	    (format "/%d%s" in-cargo-hold (aref unitnames (tradegood-units commodity)))
	     )
    (if (member EQUIPMENT_TRADE_HISTORY_V1 (elite-for-emacs-commander-equipment-list cmdr))
	(progn
	  (if (> in-cargo-hold 0)
	      (progn		
		;;view trade history
		(setq trade-history-current (elite-for-emacs-commander-trade-history-current cmdr))
		(if (and trade-history-current (aref trade-history-current i))
		    (progn
		      (setq trade-history-current (aref trade-history-current i))
		      (insert " Value: "
			      (format "%.1f CR" (/ (* in-cargo-hold market-price) 10.0))
			      )

		      (insert " Bought from "
			      (elite-for-emacs-get-system-name (nth 1 trade-history-current) (nth 0 trade-history-current))
			      " at price "
			      (format "%.1f CR" (/ (nth 5 trade-history-current) 10.0))
			      " ("
			      (format "%.1f CR" (/ (nth 4 trade-history-current) 10.0))
			      "/"
			      (aref unitnames (tradegood-units commodity))
			      ")"
			      )
		      (insert " Profit "
			      (format "%.1f CR" (/ (- (* in-cargo-hold market-price) (nth 5 trade-history-current)) 10.0))
			      )
		      
		      (setq total-profit (+ total-profit (- (* in-cargo-hold market-price) (nth 5 trade-history-current))))

		      )
		  )
		)
	  )
	  )
      )

    (setq i (1+ i))
    (if (<= i lasttrade)
	(insert "\n")
      )
    )

    (if (member EQUIPMENT_TRADE_HISTORY_V1 (elite-for-emacs-commander-equipment-list cmdr))
	(progn
	  (insert "\n------\nTotal profit: "
		  (format "%.1f CR" (/ total-profit 10.0))
		  
		  )
	  )
      )
    )
  )

(defun elite-for-emacs-local-systems ()
  "List systems within 7 light years."
  (let (
	(syscount 0)
	(d)
	(galaxy)
	(cmdr)
	(currentplanet)
	(fuel)
	)
    ;;(setq syscount 0)
  (setq cmdr  (car elite-for-emacs-commander-list))
  (setq galaxy (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)))
  (setq currentplanet  (elite-for-emacs-commander-current-planet cmdr))
  (setq fuel (elite-for-emacs-commander-fuel cmdr))
  (insert "Galaxy number ")
  (insert (number-to-string (1+ (elite-for-emacs-commander-current-galaxy cmdr))) "\n")
  (while (< syscount galsize)
    (setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
    (if (<= d maxfuel)
	(progn
	  (if (<= d fuel)
	      (insert "* ")
	    (insert "- ")
	    )
	  (insert (elite-for-emacs-short-local-system-info (elite-for-emacs-commander-current-galaxy cmdr) syscount))
	  ;;(prisys (aref galaxy syscount ) t)
	  (insert (format " (%.1f LY)" (/ d 10.0) ) "\n")
	    )
      )
    (setq syscount (1+ syscount))
    )
    (insert (format "Fuel: %.1f Light Years" (/ fuel 10.0) ))
    )
  )



(defun elite-for-emacs-commander-info ()
  "Show player info."
  (let (
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))

    (insert "Commander " (elite-for-emacs-commander-name cmdr) "\n")
    (insert "Home system: " (elite-for-emacs-get-system-name (elite-for-emacs-commander-home-galaxy cmdr) (elite-for-emacs-commander-home-system cmdr)) "\n")
    (insert "Present system: " (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr)) "\n")
    (insert "Present galaxy: " (number-to-string (1+ (elite-for-emacs-commander-current-galaxy cmdr))) "\n")
    ;;(insert "Hyperspace system: " (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-hyperspace-system cmdr)) "\n" )
    (insert "Condition: " (elite-for-emacs-commander-condition cmdr) "\n")
;;
;; 	       "\n"
;; 	       (format "Fuel: %.1f Light Years" (/ (commander-fuel elite-commander) 10.0) )
    (insert (format "Cash: %.1f CR\n" (/ (elite-for-emacs-commander-credits cmdr) 10.0)))
    (insert (format "Fuel: %.1f Light Years" (/ (elite-for-emacs-commander-fuel cmdr) 10.0) ))

;;     (setq commander-info
;; 	  (concat
;; 	   commander-info
;; 	   "Present System: "
;; 	   (elite-get-system-name currentplanet)
;; 	   "\n"
;; 	   "Hyperspace System: "
;; 	   (elite-get-system-name (commander-hyperspace-system elite-commander))
;; 	   "\n"
;; 	   "Condition: "
;; 	   (commander-condition elite-commander)
;; 	   "\n"
;; 	   ))
;; 	(setq commander-info
;; 	      (concat
;; 	       commander-info
;; 	       (format "Cash: %.1f CR" (/ (commander-credits elite-commander) 10.0))
;; 	       "\n"
;; 	       (format "Fuel: %.1f Light Years" (/ (commander-fuel elite-commander) 10.0) )
;; 	       "\n"
;; 	       )
;; 	      )
;;     (setq commander-info (concat commander-info "Legal Status: "))
;;     (setq tmp (commander-legal-status elite-commander))
;;     (if (= tmp 0)
;; 	(setq commander-info (concat commander-info "Clean\n"))
;;       (progn
;; 	(if (> tmp elite-offender-fugitive-threshold)
;; 	    (setq commander-info (concat commander-info "Fugitive\n"))
;; 	  (setq commander-info (concat commander-info "Offender\n"))
;; 	  )
;; 	))
;;     (setq commander-info (concat commander-info "Rating: "))
;;     (setq tmp (commander-elite-score elite-commander))
;;     (setq i 0)
;;     (while (< i elite-ranks)
;;       (if (>= tmp (elite-rank-score (aref elite-rank-rating i)))
;; 	  (progn
;; 	    (setq ranks (list ranks (elite-rank-title (aref elite-rank-rating i))))
;; 	    ;(setq commander-info (concat commander-info (elite-rank-title (aref elite-rank-rating i)) "\n"))
;; 	    )
;; 	  )
;;       (setq i (1+ i))
;;       )
;;     (setq commander-info (concat commander-info (car (cdr ranks)) "\n"))
;;
;;     ;;gender,marital status,etc.
;;     (setq commander-info (concat commander-info "Gender: "))
;;     (if (= (commander-gender elite-commander) ELITE_GENDER_MALE)
;; 	(setq commander-info (concat commander-info "Male\n"))
;;       (setq commander-info (concat commander-info "Female\n"))
;;     )


    )
  )


(defun elite-for-emacs-hyperspace (&optional galactic-jump)
  "Hyperspace to selected system."
  (let (
	(params)
	(planet-name)
	(cmdr)
	(target-index)
	(dist)
	(fuel)
	(galaxy)
	(dist-to-planet)
	)
    (setq cmdr (car elite-for-emacs-commander-list))

    (setq params (cdr (split-string elite-for-emacs-command)))
    ;;(setq params (list "zao"))
    (if (/= (length params) 1)
	(insert "Usage: hyperspace <planet name>.")
      (progn
	(setq planet-name (car params))
	(setq galaxy (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)))
	(setq fuel (elite-for-emacs-commander-fuel cmdr))
	(setq target-index (elite-for-emacs-get-system-index (elite-for-emacs-commander-current-galaxy cmdr) planet-name t))
	;;(setq target-index (elite-for-emacs-get-system-index (elite-for-emacs-commander-current-galaxy cmdr) "ZAON" t))
	(if galactic-jump
	    (insert "Galactic hyperspace to galaxy " (number-to-string (1+ (elite-for-emacs-commander-current-galaxy cmdr))) " system " (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) target-index))
	  (insert "Hyperspace to " (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) target-index))
	  )
	(setq dist (distance (aref galaxy target-index) (aref galaxy (elite-for-emacs-commander-current-planet cmdr))))
	(if (> dist fuel)
	    (insert "...Not enough fuel.")
	  (progn
	    ;;hyperspace to next system
	    (setf (elite-for-emacs-commander-fuel cmdr) (- fuel dist))
	    (setf (elite-for-emacs-commander-current-planet cmdr) target-index)
	    (setf (elite-for-emacs-commander-hyperspace-system cmdr) target-index)
	    (setf (elite-for-emacs-commander-current-day cmdr) (ceiling (+ (elite-for-emacs-commander-current-day cmdr) (/ dist 10.0))))
	    
	    (if (not elite-for-emacs-online)
		(progn
		  (setq rnd-byte (randbyte))
		  (setf (elite-for-emacs-commander-fluct cmdr) rnd-byte)
		  (setf (elite-for-emacs-commander-local-market cmdr) (genmarket rnd-byte (aref (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)) (elite-for-emacs-commander-current-planet cmdr))))
		  )
	      )

 	      ;;(insert (elite-for-emacs-short-local-system-info (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr)))
	      (insert "\n")
	      (elite-for-emacs-system-info)

	      (if (< (elite-for-emacs-commander-credits cmdr) 0)
		  (progn
		    (insert "You have negative account in your balance. Negative interest 50%.")
		    (setf (elite-for-emacs-commander-credits cmdr) (+ (elite-for-emacs-commander-credits cmdr) (/ (elite-for-emacs-commander-credits cmdr) 2)))
		    )
		  )

	      ;;todo: distance to system
	      (setq dist-to-planet (random 20))
	      

	      )
	  )
	)
      )
    )
  )

(defun elite-for-emacs-galactic-hyperspace ()
  "Hyperspace to next galaxy."
  (let (
	(cmdr)
	(next-galaxy)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (if (member EQUIPMENT_GALACTIC_HYPERDRIVE (elite-for-emacs-commander-equipment-list cmdr))
	(progn
	  (setq next-galaxy (1+ (elite-for-emacs-commander-current-galaxy cmdr)))
	  (if (= next-galaxy 8)
	      (setq next-galaxy 0)
	    )
	  (setf (elite-for-emacs-commander-current-galaxy cmdr) next-galaxy)
	  (setf (elite-for-emacs-commander-equipment-list cmdr) (remove EQUIPMENT_GALACTIC_HYPERDRIVE (elite-for-emacs-commander-equipment-list cmdr)))
	  (setq  elite-for-emacs-command (concat "galhyp " (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr))))
	  (elite-for-emacs-hyperspace t)

	  )
      (progn
	(insert "No galactic hyperdrive.")
	)
      )
  )
)

(defun elite-for-emacs-system-info (&optional local-system)
  "System info."
  (let (
	(cmdr)
	(params)
	(target-index)
	(planet-name)
	(galaxy)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq params (cdr (split-string elite-for-emacs-command)))
    ;;(setq params (list "za"))
    (setq galaxy (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)))
    (if (not params)
	(insert (prisys (aref galaxy (elite-for-emacs-commander-current-planet cmdr))))
      (progn
	(condition-case error
	    (progn
	      (if local-system
		  (setq target-index  (elite-for-emacs-commander-current-planet cmdr))		
		(setq target-index (elite-for-emacs-get-system-index (elite-for-emacs-commander-current-galaxy cmdr) (nth 0 params)))
		)
	      (insert (prisys (aref galaxy target-index) ) )
	;;(setq planet-name (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) target-index))

	      )
	  (error
	   (insert (error-message-string error) "\n")
	   ;;"\n"
	   (insert "Usage: system-info [<system name>]")

	   )
	  )
	)
      )
    )
  )


(defun elite-for-emacs-message-board ()
  "System message board. Displays 20 recent messages."
  (let (
	(cmdr)
	(messages);;list of messages
	(msg)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq messages (elite-for-emacs-online-message-board (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr)))

    (if (not messages)
	(insert "No messages.")
      (progn
	(while messages
	  ;;(setq messages (car (read-from-string messages)))
	  (setq msg (car messages))
	  (insert (nth 0 msg) " " (nth 1 msg) ": " (nth 2 msg))
	  
	  (setq messages (cdr messages))
	  (if messages
	      (insert "\n")
	    )
	  )
	)
      )
    )
  )

(defun elite-for-emacs-send-message ()
  "Send message to system message board. Usage: send-message <your message here can include space but limited to 160 characters>."
  (let (
	(params)
	(cmdr)
	(msg)
	(tmp)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq params (cdr (split-string elite-for-emacs-command)))
    (if (= (length params) 0)
	(insert "Usage: send-message <your message here can include space but limited to 160 characters>.")
      (progn
	(setq msg "")
	(while params
	  (setq msg (concat msg (car params) " "))
	  (setq params (cdr params))
	)
	(setq tmp (elite-for-emacs-online-send-message (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) (elite-for-emacs-commander-name cmdr) msg))
	(if (string= tmp "OK")
	    (progn
	      (setf (elite-for-emacs-commander-last-message-sent-galaxy cmdr) (elite-for-emacs-commander-current-galaxy cmdr))
	      (setf (elite-for-emacs-commander-last-message-sent-planet cmdr) (elite-for-emacs-commander-current-planet cmdr))
	      (insert "Message sent.")
	      )
	  )	

	)
      )

    )
  )

(defun elite-for-emacs-other-commanders ()
  "Display info of other commanders in universe."
  (let (
	(commanders)
	(commander)
	)
    (setq commanders (elite-for-emacs-online-commanders))
    ;;(insert (format "%Name" "Credits"))
    (insert "Total commanders: " (number-to-string (length commanders)) "\n")
    (insert "Name,Credits,Last known system(galaxy)\n")
    (while commanders
      (setq commander (car commanders))
      (insert (elite-for-emacs-commander-name commander) ", "
	      (format "%.1f CR" (/ (elite-for-emacs-commander-credits commander) 10.0))
	      )
      (insert ", ")
      (if (elite-for-emacs-commander-last-message-sent-planet commander)
	  (progn
	    (insert (elite-for-emacs-get-system-name (elite-for-emacs-commander-last-message-sent-galaxy commander) (elite-for-emacs-commander-last-message-sent-planet commander))
		    " ("
		    (number-to-string (elite-for-emacs-commander-last-message-sent-galaxy commander))
		    ")"
		    )	    
	    )
	(progn
	  (insert "N/A")
	  )
	)


      (if (cdr commanders)
	  (insert "\n")
	)

      (setq commanders (cdr commanders))
      )

    )
  )

(defun elite-for-emacs-script-execute ()
  "Execute script. Usage: script ( \"cmd params\" \"cmd2 param1 param 2\" ...)."
  (let (
	(command-list)
	(command)
	(cmd)
	)
    (condition-case error
	(progn
	  (setq elite-for-emacs-command-history (append elite-for-emacs-command-history (list elite-for-emacs-command)))
	  (setq command-list (car (read-from-string (substring elite-for-emacs-command 7))))
	  (while command-list
	    (setq command (car command-list))
	    (setq elite-for-emacs-command command)
	    (setq cmd (elite-for-emacs-get-first-command-match (car (split-string command))))
	    (setq cmd (cadr (assoc cmd elite-for-emacs-command-list)))
	    ;;act on command
	    (if  (or (commandp cmd) (functionp cmd))
		(progn
		  (if (commandp cmd)
		      (command-execute cmd)
		    (funcall cmd)
		    )
		  (elite-for-emacs-set-command-list);;set commands because normal operation require newline
		  (if (> (length command-list) 1)
		      (insert "\n")
		    )
		  )
	      )
	    (setq command-list (cdr command-list))
	    )
	  ;;(prin1 command-list)
	  )
      (error
       (insert (error-message-string error))
       )
      )
    )
  )

(defun elite-for-emacs-list-galaxy-reverse ()
  (elite-for-emacs-list-galaxy t)
)
(defun elite-for-emacs-list-galaxy (&optional reverse)
  "List all systems in galaxy sorted by distance, nearest first."
  (let (
	(msg)
	(d)
	(lyd)
	(syscount)
	(systems)
	(system-destinations (list))
	(galaxy)
	(current-galaxy)
	(currentplanet)
	(cmdr)
	(params)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq current-galaxy (elite-for-emacs-commander-current-galaxy cmdr))
    (setq currentplanet (elite-for-emacs-commander-current-planet cmdr))
    (setq galaxy (aref elite-for-emacs-galaxies-in-universe current-galaxy))
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
    (if reverse
	(setq systems (sort systems '>))
      (setq systems (sort systems '<))
      )
    (setq syscount 0)

    (insert "Galaxy number " (number-to-string (1+ current-galaxy)) "\n")
    (while systems
      ;(setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
      (setq d (car systems))
      (setq syscount (plist-get system-destinations d))
      (insert
       (elite-for-emacs-short-local-system-info current-galaxy syscount)
       (format " (%.1f LY)" d )
       (format " (%i,%i)" (plansys-x (aref galaxy syscount )) (plansys-y (aref galaxy syscount )))
       )
      (setq systems (cdr systems))
      (if systems
	(insert "\n")
	)

      )

    )
  )

(defun elite-for-emacs-galaxy-map ()
  "Partial galaxy map. Centered on local or specified system."
  (let (
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    ;;todo: map
    ;; -get current cooord.
    ;; -based on screen size show all in 20/30/etc radius.
    ;; -one character is on number
    )
  )


(defun elite-for-emacs-path-to-system ()
  "Path to system. Usage: path <system where you want to go>."
  (let (
	(cmdr)
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
       (galaxy)
       (d)
       (number-of-local-systems)
       (path)
       (completed)
       (exclude-systems);exclude systems that are visited
       (travel-time)
       (currentplanet)
       (current-galaxy)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq params (cdr (split-string elite-for-emacs-command)))
    (if (/= (length params) 1)
	(progn
	  (insert "Usage: path <system where you want to go>.")
	  )
      (progn


    (setq completed nil)
    (setq path nil)
    (setq local-systems nil)
    (setq destination-name (car params))
    (setq galaxy (elite-for-emacs-commander-current-galaxy cmdr))
    (setq destination (elite-for-emacs-get-system-index galaxy destination-name))
    (setq destination-name (elite-for-emacs-get-system-name galaxy destination))
    (setq temp-currentplanet (elite-for-emacs-commander-current-planet cmdr))
    (setq currentplanet (elite-for-emacs-commander-current-planet cmdr))
    (setq path (append path (list currentplanet)))
    (setq exclude-systems (list))
    (setq current-galaxy (aref elite-for-emacs-galaxies-in-universe galaxy))

    (while (not completed)

    (setq number-of-local-systems 0)
    ;local systems
    (setq syscount 0)
    (while (< syscount galsize)
      (setq d (distance (aref current-galaxy syscount ) (aref current-galaxy temp-currentplanet)))
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
      (setq d (distance (aref current-galaxy (plist-get local-systems i)) (aref current-galaxy destination)))
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

    (insert (concat
	       "Path from "
	       (elite-for-emacs-get-system-name galaxy currentplanet)
	       " to "
	       destination-name
	       "\n"
	       )
	  )
    (while path
      (setq syscount (car path))

      (setq travel-time (+ travel-time (ceiling (/ (distance (aref current-galaxy syscount) (aref current-galaxy temp-currentplanet)) 10.0))))
      (setq temp-currentplanet syscount)

      (setq d (distance (aref current-galaxy syscount) (aref current-galaxy currentplanet)))
      (insert (concat
		 (elite-for-emacs-short-local-system-info galaxy syscount)
		 (format " (%.1f LY)" (/ d 10.0))
		 (format " (%i,%i)" (plansys-x (aref current-galaxy syscount )) (plansys-y (aref current-galaxy syscount )))
		 "\n"))
      (setq path (cdr path))
      )

    (insert (concat
		      "Total travel time: "
		      (number-to-string travel-time)
		      " days."))


    )
)

    )
  )

(defun elite-for-emacs-save-script ()
  "Save script for later. Usage: save-script <script name> <script (\"cmd params\" \"cmd2 param1 param 2\" ...)>."
  (let (
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    ;;(setq params (cdr (split-string elite-for-emacs-command)))

    )
  )

(defun elite-for-emacs-start-script ()
  "For debugging."
  (let (
	(script)
	)
    (setq elite-for-emacs-command "script (\"new sami male\" \"bu fu 1\" \"bu f 55\" \"un\" \"hyp zao\" \"do\" \"sell-all\" \"buy comp 35\" \"un\" \"hyp isin\" \"do\" \"sell-all\" \"bu fu 35\" \"undo\" \"hyp enso\" \"doc\" \"sell-all\" \"buy comp 35\" \"undo\" \"hyp isin\" \"do\" \"sell-all\"  \"bu fu 35\" \"undo\" \"hyp enso\" \"doc\" \"sell-all\" \"buy comp 35\" \"undo\" \"hyp isin\" \"do\" \"sell-all\" \"buy-equi la\" \"bu fu 35\" \"undo\" \"hyp enso\" \"doc\" \"sell-all\" \"buy comp 35\" \"undo\" \"hyp isin\" \"do\" \"sell-all\" \"bu fu 35\" \"undo\" \"hyp enso\" \"doc\" \"sell-all\" \"buy comp 35\" \"undo\" \"hyp isin\" \"do\" \"sell-all\" \"bu fu 35\" \"undo\" \"hyp enso\" \"doc\" \"sell-all\" \"buy comp 35\" \"undo\" \"hyp isi\" \"doc\" \"sell-a\")");
    (elite-for-emacs-script-execute)

    )
  )


(defun elite-for-emacs-save-commander ()
  "Save commander."
  (let (
	(temp)
	)

    (if elite-for-emacs-online
	(progn
	  (setq temp (elite-for-emacs-online-save-commander (car elite-for-emacs-commander-list)))
	  (if (not (string= temp "OK"))
	      (error (concat "Save commander failed.") )
	    )
	  (insert "Commander saved.")

	  )
      (progn
	(find-file (concat "~/.elite-for-emacs-commander-" (elite-for-emacs-commander-name (car elite-for-emacs-commander-list))))
	(erase-buffer)

	(insert "(setq elite-for-emacs-saved-commander-list '")
	(prin1 elite-for-emacs-commander-list (current-buffer))
	(insert ")\n")
	(save-buffer)
	(kill-buffer (current-buffer))
	)
      )

    )
)

(defun elite-for-emacs-kill-buffer ()
  "Clean up when killing buffer "
  (let (

	)

 ;;save
    (if (and 
	 elite-for-emacs-game-is-on 
	 elite-for-emacs-save-confirmation-when-exit 
	 (eq (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_DOCKED) 
	 (y-or-n-p-with-timeout (concat "Save commander " (elite-for-emacs-commander-name (car elite-for-emacs-commander-list)) " ") 4 t)
	 )
	(elite-for-emacs-save-commander)
      )

    (setq elite-for-emacs-game-is-on nil)
    (setq elite-for-emacs-online nil )
    (setq elite-for-emacs-buffer-name elite-for-emacs-buffer-name-offline)

    ;;(setq elite-for-emacs-commander-list nil)
    )
  )
