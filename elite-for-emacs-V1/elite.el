;;; elite.el -  Elite for EMACS 

(defconst emacs-elite-version "0.10.2"
  "Version number of Elite for EMACS")

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


;;Installation

;;add emacs elite to load path
;;(add-to-list 'load-path (expand-file-name "~/emacs/elite-for-emacs"))
;;(require 'elite)


(require 'cl)
(require 'widget)
(require 'elite-variables)
(require 'elite-commands)
(require 'elite-functions)
(require 'elite-screen-functions)
(require 'elite-space-screen-functions)
(require 'elite-commanders)
(require 'elite-combat)
(require 'elite-ships)
(require 'elite-missions)
(require 'elite-missions-passenger-delivery)
(require 'elite-sensors)
(require 'elite-people)
(require 'elite-special-mission-1)
(require 'elite-commander-log)
(require 'elite-windows)
(require 'elite-encounter)
(require 'elite-ai-commanders)
(require 'elite-federation)

;;;main function
(defun elite ()
  "Starts Elite for EMACS"
  (interactive)
  ;;create new buffer
  (let (
	(new-commander)
	(name)
	(tmp)	
	(e-buffer)
	)

    (if (not window-system)
	(progn
	  (message "Only windowed system is supported.")
	  )
      (progn


	(setq e-buffer (get-buffer elite-buffer-name))
	(if (bufferp e-buffer)
	    ;;buffer exists
	    (switch-to-buffer elite-buffer-name)
	  (progn
	    (random t)


	    (setq cursor-in-echo-area t)
	    (setq elite-dead-commander nil)
	    (get-buffer-create elite-buffer-name)
	    (switch-to-buffer elite-buffer-name)
	    (erase-buffer)

	    ;;(setq elite-original-cursor-color (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
	    (setq elite-original-cursor-color (cdr (assq 'cursor-color
							 (frame-parameters (selected-frame))))
		  elite-original-foreground-color (cdr (assq 'foreground-color
							     (frame-parameters (selected-frame))))
		  elite-original-background-color (cdr (assq 'background-color
							     (frame-parameters (selected-frame)))))
      
	    ;;(set-foreground-color elite-foreground-color)
	    ;;(set-background-color elite-background-color)

	    (setq elite-original-screen-width (frame-width))
	    (setq elite-original-screen-height (frame-height))
	    ;;(setq elite-original-screen-height (screen-height))
	    ;;(frame-parameters (selected-frame))
	    (set-frame-height (selected-frame) elite-screen-height)
	    (set-frame-width (selected-frame) elite-screen-width)
	    ;;(set-frame-name elite-frame-title)
  
	    (setq elite-screen-middle-column (/ (- elite-screen-width 3) 2))
	    (setq elite-screen-middle-line (/ (- elite-screen-height 3) 2))

	    (setq elite-original-scroll-bar-mode scroll-bar-mode)
	    (setq elite-original-cursor-in-non-selected-windows cursor-in-non-selected-windows)
	    (scroll-bar-mode nil)
	    (setq cursor-in-non-selected-windows nil)

	    ;;set up major-mode
	    (elite--mode-setup)
	    (elite-mode-set-startup-mode-line)

	    ;;Show start-up screen
	    (message (concat "Welcome to Elite for EMACS v." emacs-elite-version ". Load Commander? (y or n)"))
	    (setq elite-animation-interval 130)
	    (elite-startup)
	    ;;set up major-mode
	    ;;      (elite--mode-setup)
	    ;;      (elite-mode-set-startup-mode-line)
   
	    ;;populate shipdata
	    (elite-initialize-ships)

	    ;;Populate trade names array from commodities
	    (populate-tradnames)
	    ;;Populate equipment array
	    (populate-equipment-stock)
	    ;;(insert (concat tradnames))
      
	    ;;clear mission list before loading commander
	    (setq elite-current-missions nil)
	    (setq elite-accepted-missions nil)
	    (setq elite-message-shell-inserted-day -1)
	    (setq elite-space-message-shell-inserted-day -1)
	    ;;load commander list
	    (elite-set-commander-list)

	    ;;reset special mission variables
	    (setq elite-special-mission-1-refused nil)
	    (setq elite-special-mission-1-accepted nil)
	    (setq elite-special-mission-1-completed nil)
	    (setq elite-special-mission-1-traitors nil)
	    (setq elite-special-mission-1-traitor-status nil)
	    (setq elite-special-mission-1-log-entries nil)
	    (setq elite-special-mission-1-naval-galactic-hyperdrive nil)

	    ;;misc reseting
	    (setq elite-people-names nil)
	    (setq elite-people-in-universe nil)
	    (setq elite-people-in-universe (make-vector 8 (list)))
	    (setq elite-additional-commands nil)
	    (setq elite-people-out-of-loop nil)
	    (setq elite-autosave 'nil)
	    (setq elite-autofuel 'nil)
	    (setq elite-ai-commander-list nil)
	    (setq elite-ai-commander-activity-in-progress nil)
	    (setq elite-federation-commander-born nil)
	    (setq elite-station-ui-widget-buffer-created nil)
	    (setq elite-space-ui-widget-buffer-created nil)
	    (setq elite-ai-commander-path nil)
	    (setq commander-visited-systems nil)

	    ;;(setq new-commander (read-string "Load Commander (y/n): "))
	    ;;(if (string= new-commander "y")
	    (if (y-or-n-p "Load Commander? ")
		(progn
		  ;;ask commander name
		  (setq name (read-string "Commander name? "))
		  (setq new-commander (elite-load-commander name))
		  ;;(sleep-for 0 300)
		  )
	      (progn
		(setq new-commander t)
		)
	      )
	    
	    (if (not new-commander)
		(progn
		  (setq elite-commander-name (commander-name elite-commander))
		  (setq elite-commander-gender (commander-gender elite-commander))
		  (mysrand 12345)
		  (message "Initializing universe...")
		  (setq galaxynum (commander-current-galaxy elite-commander))
		  (setq galaxy (aref elite-galaxies-in-universe (1- galaxynum)))

		  (elite-set-planet-completion-list)
		  ;; 	    (if (/= elite-galaxy-in-saved-file galaxynum)
		  ;; 		(buildgalaxy galaxynum)
		  ;; 	      )

		  (setq currentplanet (commander-current-planet elite-commander))
		  (mysrand (commander-lastrand elite-commander))
		  ;;(setq localmarket (genmarket (commander-fluct elite-commander) (aref galaxy currentplanet)))
		  (setq localmarket (commander-local-market elite-commander))
		  (setq fuel (commander-fuel elite-commander))
		  (setq elite-day (commander-current-day elite-commander))
		  (setq elite-player-cash (commander-credits elite-commander))
		  (setq shipshold (commander-current-cargo elite-commander))
		  (if (commander-large-cargo-bay elite-commander)
		      (setq holdspace (- 35 (elite-cargo)));;(commander-cargo-capacity elite-commander))
		    (setq holdspace (- 20 (elite-cargo)));;(commander-cargo-capacity elite-commander))
		    )
		  (message "Initializing universe...done")
		  ;;load commander from file
		  ;;set variables
		  ;;(message "to be implemented")
		  )
	      (progn
		(setq elite-commander-name (read-string "Commander name (default Jameson): " nil nil "Jameson"))
	  
		(setq elite-commander-gender (completing-read  "Commander gender (female/male): "  '(("female" 1) ("male" 2) )  nil t))
		(if (equal elite-commander-gender "female")
		    (setq elite-commander-gender ELITE_GENDER_FEMALE)
		  (setq elite-commander-gender ELITE_GENDER_MALE)
		  )
		;;initialize random generator
		;;(message "Initializing universe...")
		(mysrand 12345)
		;;build galaxy	
		(setq galaxynum 1)

		;;init all galaxies
		;; 	  (setq tmp 1)
		;; 	  (while (<= tmp 8)
		;; 	    (setq galaxy (make-vector galsize []))
		;; 	    (aset elite-galaxies-in-universe (1- tmp) (buildgalaxy tmp (concat " (" tmp "/8)")))
		;; 	    (setq tmp (1+ tmp))
		;; 	    )
		(setq galaxy (aref elite-galaxies-in-universe 0))
	  
		(elite-set-planet-completion-list)


		;;(buildgalaxy galaxynum);;builds galaxy
		;;system 7 should be LAVE
		(setq currentplanet numforLave);;don't use jump
		(setq localmarket (genmarket 0 (aref galaxy numforLave)))
		(setq fuel maxfuel)
		(setq holdspace 20)
	  
		(setq elite-day 0)

		;;;(setq commander-visited-systems nil)
		(setq commander-visited-systems (append commander-visited-systems (list currentplanet)))

		(setq elite-player-cash 1000)
		;;(setq elite-player-cash 1000000)
		(setq shipshold (make-vector (+ lasttrade 1) 0))
		;;use repalce all to start using commander info
	  
		(setq elite-commander (elite-create-commander elite-commander-name elite-commander-gender))

		;;initialize people
		(elite-initialize-people)

		;;initialize ai commanders
		(elite-ai-initialize)

		;;(elite-update-commander)
		;;(message "Initializing universe...done")	  

		))

	    ;;;(set-cursor-color elite-cursor-color)

	    ;;(setq elite-screen-middle-line (+ 2 (/ (- elite-screen-height 3) 2)))

	    (setq elite-rand1-seed (random))
	    ;;(elite-rand1)

	    (elite-ui-station-initialize)
	    (elite-ui-refresh)
	    (elite-initialize-missions)
 
	    (elite-show-missions)
	    (elite-display-ai-commanders)

	    (if new-commander
		(progn
		  ;;show welcome message to new commander
		  (if elite-use-windows
		      (progn
			(elite-message-shell (concat ">" elite-pilot-welcome-message ))
			)
		    (progn
		      (elite-insert-text (concat ">" elite-pilot-welcome-message ))
		      )
		    )
		  )
	      (progn
		(elite-display-commander-info)
		)
	      )
	    ;;new commander
       
	    ;;initialzation done.. wait commands...
	    ;;(elite-insert-text "Waiting commands:\nHelp here....\nemacs commands as interface to elite universe\ncommands in separate file \n")
 
	    ;;set mode-line
	    ;;(elite-mode-set-station-mode-line)


	    (message (concat elite-welcome-message " (Version: " emacs-elite-version ")" ))
	    ;;      (plansys-name (aref galaxy 7))

	    (setq cursor-in-echo-area nil)

	    )
	  )
	)
      )
    )
  )


(defvar elite-pilot-welcome-message
  ;;;(setq elite-pilot-welcome-message
  ">Incoming message:\nGreetings Commander.Welcome to the Elite Federation of Pilots. Piloting Cobra MkIII, you are now one of The Few in The Eight Galaxies. All pilots strive to Elite, few succeed and most die.\nPilot your Cobra MkIII with honor.\nQ'Apla!"
  )

(defun elite-mode ()
  "Major mode for Emacs-Elite. 
 Special commands: \\{elite-mode-map}
Turning on elite-mode runs the hook `elite-mode-hook'."
  ;;  (interactive)
  (elite--mode-setup)
  )      

;;; Create mode-specific tables.
(defvar elite-mode-syntax-table nil 
  "Syntax table used while in Elite mode.")

(if elite-mode-syntax-table
    ();; Do not change the table if it is already set up.
  (setq elite-mode-syntax-table (make-syntax-table)))
;;  (modify-syntax-entry ?' "w   " elite-mode-syntax-table))

(defvar elite-mode-abbrev-table nil
  "Abbrev table used while in text mode.")
(define-abbrev-table 'elite-mode-abbrev-table ())

(defvar elite-mode-map nil);; Create a mode-specific keymap.

(if elite-mode-map
    ();; Do not change the keymap if it is already set up.
  (setq elite-mode-map (make-sparse-keymap))

  (define-key elite-mode-map "\M-i" 'elite-system-info)
  (define-key elite-mode-map "\C-i" 'elite-current-system-info)
  (define-key elite-mode-map "\M-h" 'elite-hyperspace-to)
  (define-key elite-mode-map "\C-f" 'elite-buy-fuel)
  (define-key elite-mode-map "\C-b" 'elite-buy-goods)
  (define-key elite-mode-map "\C-s" 'elite-sell-goods)
  (define-key elite-mode-map "\M-s" 'elite-select-hyperspace-system)
  (define-key elite-mode-map "\C-e" 'elite-buy-equipment)
  (define-key elite-mode-map "\C-u" 'elite-undock)
  (define-key elite-mode-map "\C-d" 'elite-dock)
  (define-key elite-mode-map "\C-l" 'elite-local-systems)
  (define-key elite-mode-map "\M-l" 'elite-list-galaxy)
  (define-key elite-mode-map "\C-o" 'elite-display-inventory)
  (define-key elite-mode-map "\C-c\C-c" 'elite-display-commander-info)
  (define-key elite-mode-map "\C-j" 'elite-jump-toward-planet)
  (define-key elite-mode-map "\M-j" 'elite-jump-toward-sun)
  (define-key elite-mode-map "\M-e" 'elite-display-equipment)
  (define-key elite-mode-map "\C-c\C-h" 'elite-show-help)
  (define-key elite-mode-map "\C-p" 'elite-path)
  (define-key elite-mode-map "\C-c\C-m" 'elite-available-missions)
  (define-key elite-mode-map "\M-m" 'elite-select-mission)
  (define-key elite-mode-map "\M-g" 'elite-galactic-jump)
  (define-key elite-mode-map "\C-c\C-a" 'elite-additional-command)
  (define-key elite-mode-map "\C-c\C-q" 'elite-quit)
  (define-key elite-mode-map "\C-c\C-d" 'elite-display-ai-commanders)
  (define-key elite-mode-map "\C-t" 'elite-talk-to-commander)
  )


(defun elite--mode-setup ()
  "Sets up elite-mode."
  (kill-all-local-variables)
  (use-local-map elite-mode-map)
  (setq local-abbrev-table elite-mode-abbrev-table)
  (set-syntax-table elite-mode-syntax-table)
  
  ;;no local variables
  ;;(elite-set-local-variables)


  ;;   (setq paragraph-start (concat "[ \t]*$\\|" page-delimiter))
  ;;   (make-local-variable 'paragraph-separate)
  ;;   (setq paragraph-separate paragraph-start)
  ;;   (make-local-hook 'elite-exit-hook)
  ;; ;;  (remove-hook 'kill-buffer-hook 'elite-exit-hook)
  ;;   (add-hook 'elite-exit-hook 'elite-kill-buffer)
  ;;(add-hook 'kill-buffer-hook 'elite-kill-buffer)
  ;;(add-hook 'pre-command-hook 'elite-self-insert-command)
  (setq mode-name "Elite")
  (setq major-mode 'elite-mode)
  (run-hooks 'elite-mode-hook);; Finally, this permits the user to
  ;;   customize the mode with a hook.

  )

(defun elite-mode-set-startup-mode-line ()
  "Set mode line for start up. Before user starts game."
  (setq mode-line-format
	(list  mode-line-buffer-identification
	       "%-"))
  (force-mode-line-update)

  )

(defun elite-mode-set-station-mode-line ()
  "Set mode-line for elite-mode."
  (setq mode-line-format
	(list  mode-line-buffer-identification
	       " "
	       (format "Cash: %.1f CR" (/ elite-player-cash 10.0))
	       " "
	       (format "Fuel: %.1f LY" (/ fuel 10.0))
	       " "
	       (format"Cargo:%it" holdspace)
	       " "
	       "System: "
	       (elite-short-local-system-info)
	       " "
	       "Day: "
	       (number-to-string elite-day)
	       ;; 	       (plansys-name (aref galaxy currentplanet))
	       ;; 	       " "
	       ;; 	       "TL: "
	       ;; 	       (number-to-string (1+ (plansys-techlevel (aref galaxy currentplanet))))
	       ;; 	       " "
	       ;; 	       (aref econnames (plansys-economy (aref galaxy currentplanet)))
	       ;; 	       " "
	       ;; 	       (aref govnames (plansys-govtype (aref galaxy currentplanet)))
	       "%-"))
  (force-mode-line-update)
  )

(defun elite-mode-set-space-mode-line ()
  "Set mode-line for elite-mode when space."
  (setq mode-line-format
	(list  mode-line-buffer-identification
	       " "
	       "in space..."
	       " System: "
	       (elite-short-local-system-info)
	       " "
	       "Day: "
	       (number-to-string elite-day)
	       ;; 	       " "
	       ;; 	       "Condition: "
	       ;; 	       (commander-condition elite-commander)
	       "%-"))
  (force-mode-line-update)
  )

(defun elite-mode-set-empty-mode-line ()
  "Set mode-line for elite-mode when space."
  (setq mode-line-format
	(list  mode-line-buffer-identification
	       "%-"))
  (force-mode-line-update)
  )



(defun elite-kill-buffer ()
  "Called when buffer is killed. Saves commander and other game data."
  ;;   (setq cursor-in-echo-area nil)
  ;;   (if (string= (buffer-name) elite-buffer-name)
  ;;       ;;(if (or (eq (current-buffer) elite-space-buffer) (eq (current-buffer) elite-space-message-buffer) (eq (current-buffer) elite-space-command-buffer) (eq (current-buffer) elite-space-sensor-buffer) (string= (buffer-name) elite-buffer-name))
  ;;       (progn
  ;; 	(if (not elite-dead-commander)
  ;; 	    (if (elite-is-docked)
  ;; 		(if (y-or-n-p "Exiting.. Save Commander? ")
  ;; 		    (elite-save-commander)
  ;; 		  )
  ;; 	      (progn
  ;; 		(read-string "Not docked. Can't save Commander. Press C-g to go back to game. Enter to exit.")
  ;; 		))
  ;; 	  )
  ;; 
  ;; 	(if (bufferp (get-buffer elite-special-mission-1-status-buffer-name))
  ;; 	    (kill-buffer elite-special-mission-1-status-buffer-name)
  ;; 	)
  ;; 
  ;; 	(if (and elite-space-buffer (bufferp (get-buffer elite-space-buffer)))
  ;; 	    (kill-buffer elite-space-buffer)
  ;; 	  )
  ;; 	(if (and elite-space-command-buffer (bufferp (get-buffer elite-space-command-buffer)))
  ;; 	    (kill-buffer elite-space-command-buffer)
  ;; 	  )
  ;; 	(if (and elite-space-message-buffer (bufferp (get-buffer elite-space-message-buffer)))
  ;; 	    (kill-buffer elite-space-message-buffer)
  ;; 	  )
  ;; 	(if (and elite-space-sensor-buffer (bufferp (get-buffer elite-space-sensor-buffer)))
  ;; 	    (kill-buffer elite-space-sensor-buffer)
  ;; 	  )
  ;; 
  ;; 	(if (and elite-command-buffer (bufferp (get-buffer elite-command-buffer)))
  ;; 	    (kill-buffer elite-command-buffer)
  ;; 	  )
  ;; 	(if (and elite-market-buffer (bufferp (get-buffer elite-market-buffer)))
  ;; 	    (kill-buffer elite-market-buffer)
  ;; 	  )
  ;; 	(if (and elite-message-buffer (bufferp (get-buffer elite-message-buffer)))
  ;; 	    (kill-buffer elite-message-buffer)
  ;; 	  )
  ;; 
  ;; 
  ;; 	(set-frame-height (selected-frame) elite-original-screen-height)
  ;; 	(set-frame-width (selected-frame) elite-original-screen-width)
  ;; 	(set-cursor-color elite-original-cursor-color)
  ;; 	(set-foreground-color elite-original-foreground-color)
  ;; 	(set-background-color elite-original-background-color)
  ;; 	;;(set-frame-name elite-original-frame-title)
  ;; 	;;(set-cursor-color "cyan")
  ;; 
  ;; 	(if (not (one-window-p))
  ;; 	    (delete-other-windows)
  ;; 	  )
  ;; 
  ;; 	(message "")
  ;; 	)
  ;;     )
  )

(provide 'elite)