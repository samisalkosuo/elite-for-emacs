;;; elite-windows.el -  Functions for Emacs-Elite windowed user interface

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

(defvar elite-station-message-buffer nil
  "Buffer for messages")
(defvar elite-station-message-window nil
  "Window for messages")

(defvar elite-market-buffer nil
  "Buffer for market info")
(defvar elite-market-window nil
  "Window for market info")

(defvar elite-widget-buffer nil
  "Buffer for widget ui")
(defvar elite-widget-window nil
  "Window for window ui")

(defvar elite-use-windows t
  "t if using separate windows and buffers")

(defun elite-initialize-windows ()
  "initialise windows"
  (let (
	(tmp)
	)
    (if (not (one-window-p))
	(progn
	  (delete-other-windows)
	  (switch-to-buffer elite-buffer-name)
	  )
      )

    ;;create buffers
    (if elite-widget-buffer
	(kill-buffer elite-widget-buffer)
	)
    (setq elite-station-message-buffer (get-buffer-create "Elite Message Buffer"))
    (setq elite-market-buffer  (get-buffer-create "Elite Market Buffer"))
    (setq elite-widget-buffer (get-buffer-create "Elite Widget Buffer"))

    ;;create windows
    (setq elite-station-message-window (selected-window))
    (setq elite-market-window (split-window elite-station-message-window 57 t))
    (setq elite-widget-window (split-window elite-market-window 10))
    
    (set-window-buffer elite-station-message-window elite-station-message-buffer)
    (set-window-buffer elite-market-window elite-market-buffer)
    (set-window-buffer elite-widget-window elite-widget-buffer)
;   (set-window-dedicated-p elite-station-message-window t)
;   (set-window-dedicated-p elite-widget-window t)
;   (set-window-dedicated-p elite-market-window t)  
  ;set modes
  (select-window elite-station-message-window)
  (use-local-map elite-mode-map)
  (setq local-abbrev-table elite-mode-abbrev-table)
  (set-syntax-table elite-mode-syntax-table)
  (setq major-mode 'elite-mode)
  (setq mode-line-format (list   
			  (elite-short-local-system-info)
			  "  "
			  "Day: "
			  (number-to-string elite-day)
			  "%-"))
  (force-mode-line-update)

  (select-window elite-market-window)
  (use-local-map elite-mode-map)
  (setq local-abbrev-table elite-mode-abbrev-table)
  (set-syntax-table elite-mode-syntax-table)
  (setq major-mode 'elite-mode)
  (setq mode-line-format (list   
			  "Market"
			  "%-"))
  (force-mode-line-update)


  (elite-windows-create-station-widget-buffer)
;;   (use-local-map elite-mode-map)
;;   (setq local-abbrev-table elite-mode-abbrev-table)
;;   (set-syntax-table elite-mode-syntax-table)
;;   (setq major-mode 'elite-mode)
  (setq mode-line-format (list   
			  "Commands"
			  "%-"))
  (force-mode-line-update)


  (elite-station-ui-switch-to-widget-buffer)


  ;(message "")
  )
)


(defvar elite-station-ui-show-widget-help t
  "If t show widget help in minibuffer")
;;(setq elite-station-ui-show-widget-help t)

(defvar elite-station-ui-widget-buffer-created nil
  "If t show widget help in minibuffer")
;;(setq elite-windows-station-widget-buffer-created nil)

(defun elite-windows-create-station-widget-buffer ()
  (let (
	(my-widget-keymap)
	)
    (elite-display-market)
    (if (not elite-station-ui-widget-buffer-created)
	(progn
	  (select-window elite-widget-window)
	  (switch-to-buffer elite-widget-buffer)
	  (erase-buffer)

	  (widget-insert " ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-station-ui-scroll-message-buffer-up)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Scroll message window up (shift-up arrow)"
					)
				      )
			 "u")
	  (widget-insert " ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-station-ui-scroll-message-buffer-down)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Scroll message window down (shift-down arrow)"
					)
				      )
			 "d")
	  (widget-insert "\n ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-undock)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Undock from station (u)"
					)
				      )
			 "Undock")
	  (widget-insert "\n ")
	  (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-buy-goods)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Buy goods (b)"
				  )
				)
		   "Buy goods")

	  (widget-insert "\n ")
	  (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-sell-goods)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Sell goods (S)"
				  )
				)
		   "Sell goods")
	  (widget-insert "\n ")
	  (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-buy-fuel)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Buy fuel (f)"
				  )
				)
		   "Buy fuel")
	  (widget-insert "\n ")
	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-ai-commander-info)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Elite Federation pilots (F)"
					)
				      )
			 "Elite Federation pilots")
	  (widget-insert "\n ")

	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-display-ai-commanders)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Commanders in current system (C)"
					)
				      )
			 "Commanders")
	  (widget-insert "\n ")
	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-talk-to-commander)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Talk to commander (t)"
					)
				      )
			 "Talk to commander")
	  (widget-insert "\n ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-local-systems)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Show systems within 7 light years (l)"
					)
				      )
			 "Local systems")    
	  (widget-insert "\n ")
	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-display-inventory)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Display inventory (i)"
					)
				      )
			 "Inventory")
	  (widget-insert "\n ")
	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-display-commander-info)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Display commander info (c)"
					)
				      )
			 "Commander info")
	  (widget-insert "\n ")


	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-available-missions)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Display available missions (m)"
					)
				      )
			 "Available missions")
	  (widget-insert "\n ")
	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-select-mission)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Select mission (M)"
					)
				      )
			 "Select mission")
	  (widget-insert "\n ")

	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-current-system-info)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Display system info (I)"
					)
				      )
			 "Current system info")
	  (widget-insert "\n ")
	  (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-system-info)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Display system info on specified system (N)"
				  )
				)
		   "System info")
	  (widget-insert "\n ")
	  (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-display-equipment)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Available equipment (e)"
				  )
				)
		   "Available equipment")
	  (widget-insert "\n ")
	  (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-buy-equipment)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Buy equipment (B)"
				  )
				)
		   "Buy equipment")

	  (widget-insert "\n ")

	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-select-hyperspace-system)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Select hyperspace system (s)"
					)
				      )
			 "Select hyperspace system")
	  (widget-insert "\n ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-list-galaxy)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Show systems in galaxy (L)"
					)
				      )
			 "Galaxy systems")
	  (widget-insert "\n ")

	  (widget-create 'push-button	
			 :notify (lambda (&rest ignore)
				   (elite-path)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Calculate shortest path to system (p)"
					)
				      )
			 "Path to")
	  (widget-insert "\n ")
    (if elite-special-mission-1-accepted
	(progn
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-special-mission-1-show-status)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-space-ui-show-widget-help
					  "Special mission 1 status (M-1)"
					)
				      )
			 "Special mission 1 status")
	  (widget-insert "\n ")
	  )
	)


	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-save-commander)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Save commander (M-s)"
					)
				      )
			 "Save commander")
	  (widget-insert "\n ")

	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-quit)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-station-ui-show-widget-help
					  "Quit Elite for EMACS (Q)"
					)
				      )
			 "Quit")

	  (widget-minor-mode 1)

	  (setq my-widget-keymap widget-keymap);(make-keymap))
	  ;;(suppress-keymap my-widget-keymap)
	  ;(set-keymap-parent my-widget-keymap widget-keymap)
	  (define-key my-widget-keymap [up] 'widget-backward)
	  (define-key my-widget-keymap [down] 'widget-forward)
	  (define-key my-widget-keymap [left] 'widget-backward)
	  (define-key my-widget-keymap [right] 'widget-forward)
	  (define-key my-widget-keymap [(shift up)] 'elite-station-ui-scroll-message-buffer-up)
	  (define-key my-widget-keymap [(shift down)] 'elite-station-ui-scroll-message-buffer-down)
	  (define-key my-widget-keymap [(control prior)] 'elite-station-ui-scroll-message-buffer-up-page)
	  (define-key my-widget-keymap [(control next)] 'elite-station-ui-scroll-message-buffer-down-page)
	  (define-key my-widget-keymap [(control home)] 'elite-station-ui-scroll-message-buffer-start)
	  (define-key my-widget-keymap [(control end)] 'elite-station-ui-scroll-message-buffer-end)

	  (define-key my-widget-keymap "Q" 'elite-quit)
	  (define-key my-widget-keymap "p" 'elite-path)
	  (define-key my-widget-keymap "h" 'elite-hyperspace-to)
	  (define-key my-widget-keymap "u" 'elite-undock)
	  (define-key my-widget-keymap "l" 'elite-local-systems)
	  (define-key my-widget-keymap "i" 'elite-display-inventory)
	  (define-key my-widget-keymap "c" 'elite-display-commander-info)
	  (define-key my-widget-keymap "I" 'elite-current-system-info)
	  (define-key my-widget-keymap "s" 'elite-select-hyperspace-system)
	  (define-key my-widget-keymap "L" 'elite-list-galaxy)
	  (define-key my-widget-keymap "C" 'elite-display-ai-commanders)
	  (define-key my-widget-keymap "t" 'elite-talk-to-commander)
	  (define-key my-widget-keymap "N" 'elite-system-info)
	  (define-key my-widget-keymap "e" 'elite-display-equipment)
	  (define-key my-widget-keymap "B" 'elite-buy-equipment)
	  (define-key my-widget-keymap "b" 'elite-buy-goods)
	  (define-key my-widget-keymap "S" 'elite-sell-goods)
	  (define-key my-widget-keymap "f" 'elite-buy-fuel)
	  (define-key my-widget-keymap "F" 'elite-ai-commander-info)
	  (define-key my-widget-keymap "m" 'elite-available-missions)
	  (define-key my-widget-keymap "M" 'elite-select-mission)

	  (define-key my-widget-keymap "\M-s" 'elite-save-commander)
	  (if elite-special-mission-1-accepted
	      (progn
		(define-key my-widget-keymap "\M-1" 'elite-special-mission-1-show-status)
		)
	    )

	  (use-local-map my-widget-keymap)
	  (widget-setup)
	  (goto-char 2)
	  ;;(setq elite-station-ui-widget-buffer-created t)
	  )
      )

    )
  )

(defun elite-refresh-windows ()
  "refresh windows"

  ;ai commander activity
  (elite-display-market)
  ;(elite-update-command-shell-mode-line)

)

(defun elite-update-command-shell-mode-line ()

  (select-window elite-station-message-window)
  (switch-to-buffer elite-station-message-buffer)
  (setq mode-line-format (list   
			  (elite-short-local-system-info)
			  " "
			  (format "F:%.1fLY" (/ (commander-fuel elite-commander) 10.0))
			  " "
			  "Day: "
			  (number-to-string elite-day)
			  "%-"))
  (force-mode-line-update)

  (elite-station-ui-switch-to-widget-buffer)

)

;(elite-initialize-windows)

(defun elite-display-market-window (txt)
  "displays market info in market window"
  (let (
	(market-list)
	(list-item)
	(len)
	)
  (select-window elite-market-window)
  (switch-to-buffer elite-market-buffer)
  (erase-buffer)
  (setq market-list (split-string txt "\n"))
  (while market-list
    (setq list-item (car market-list))
    (setq len (length list-item))
    (insert list-item)	  
    (insert (make-string (- 28 len) ? ))
    (setq market-list (cdr market-list))
    (if market-list
	(progn
	  (setq list-item (car market-list))
	  (insert list-item)	  
	  (insert "\n")
	  ;(insert "\t     " list-item "\n")
	  (setq market-list (cdr market-list))
	  )
      )
     )

  ;insert total cargo, free space, cash to modeline
  (setq mode-line-format (list   
			  "Total Cargo: "
			  (number-to-string (elite-cargo))
			  " "
			  "Free Space: "
			  (number-to-string holdspace)
			  " "
			  (format "F: %.1fLY" (/ (commander-fuel elite-commander) 10.0))
			  " "
			  (format "Cash: %.1f CR" (/ (commander-credits elite-commander) 10.0))
			  "%-"))
  (force-mode-line-update)

  ;insert cargo also in market list
;     (setq i 0)
;     (while (< i AlienItems)
;       (setq cargo (aref shipshold i))
;       (if (> cargo 0)
; 	  (progn 
; 	    (setq msg (concat msg "\n" (aref tradnames i) " "  cargo (elite-get-unit i)))
; 	  ))
;       (setq i (1+ i))
;       )

  (elite-station-ui-switch-to-widget-buffer)

  )
)

(defun elite-command-shell (txt)
  "Command shell "
  (let (
	(txt-list)
	)
    (select-window elite-station-message-window)
    (switch-to-buffer elite-station-message-buffer)
    (goto-char (point-max))
    ;(setq txt "tret\netetttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt")
    (setq txt-list (elite-split-string-for-shell txt))
    (while txt-list
      (insert (car txt-list) "\n")
      (setq txt-list (cdr txt-list))
      )
    ;(insert ">")
    ;;continue ai commander activity
    (elite-ai-commander-continue-activity)

    (elite-station-ui-switch-to-widget-buffer)
    )
  )

(defvar elite-message-shell-inserted-day -1
  "Inserted day in message shell")

(defun elite-message-shell (txt &optional long)
  "message shell"
  (let (
	(txt-list)
	(max-height)
	(width)
	(txt-line)
	(current-line)
	)
    (if t
	(progn
	  (elite-command-shell txt)
	  )
      (progn
	(select-window elite-widget-window)
	(switch-to-buffer elite-widget-buffer)
	(setq max-height (- (window-height) 4))
	(setq width (window-width));todo: use width==>arbitrary window sizes
	(goto-char (point-max))
	(if (> elite-day elite-message-shell-inserted-day)
	    (progn
	      (insert "Day: " (number-to-string elite-day) "\n")
	      (setq elite-message-shell-inserted-day elite-day)
	      )
	  )
	(setq current-line 1)
	;;(setq txt "tret\netetttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt")
	(setq txt-list (elite-split-string-for-shell txt))
	(while txt-list
	  (setq txt-line (concat (car txt-list) "\n"))
	  (if (and long (= current-line max-height))
	      (progn
		(read-string "More..")
		(recenter 0)
		)
	    )
      (insert txt-line)
      (setq current-line (1+ current-line))
;       (if (pos-visible-in-window-p (+ (point) (length txt-line)))
; 	  (insert txt-line)
; 	(progn
; 	  (read-string "More...")
; 	  (insert txt-line)
; 	  )
; 	)
      
      (setq txt-list (cdr txt-list))
      )
	(select-window elite-station-message-window)
	(switch-to-buffer elite-station-message-buffer)
	)
      )
    
    )
  )

(defun elite-split-string-for-mini-buffer (txt)
  "Splits string for mini buffer"
  (let (
	(len)
	(text "")
	(msg)
	(tmp)
	(tmp-line)
	(minibuffer-list "")
	)
    (setq len (- elite-screen-width 25))
    (setq msg (split-string txt "\n"))
    (while msg
      (setq tmp (car msg))
      (setq text (concat text tmp " "))
      (setq msg (cdr msg))
      )
    
    ;split word by word
    (setq msg (split-string text " "))
    (while msg
      (setq tmp (car msg))
      (setq tmp-line (concat tmp-line tmp " "))
      (if (> (length tmp-line) len)
	  (progn
	    (setq minibuffer-list (concat minibuffer-list tmp-line "\n"))
	    (setq tmp-line "")
	    )
	  )
      (setq msg (cdr msg))
      )
    (setq minibuffer-list (concat minibuffer-list tmp-line))
    
    (split-string minibuffer-list "\n")
    )
  )


(defun elite-station-ui-scroll-message-buffer-up-page ()
  (interactive)
  (elite-station-ui-scroll-message-buffer t 30)
)

(defun elite-station-ui-scroll-message-buffer-down-page ()
  (interactive)
  (elite-station-ui-scroll-message-buffer nil 30)
)

(defun elite-station-ui-scroll-message-buffer-up ()
  (interactive)
  (elite-station-ui-scroll-message-buffer t)
)

(defun elite-station-ui-scroll-message-buffer-down ()
  (interactive)
  (elite-station-ui-scroll-message-buffer)

)

(defun elite-station-ui-scroll-message-buffer-start ()
  (interactive)
  (select-window elite-station-message-window)
  (switch-to-buffer elite-station-message-buffer)
  (goto-char 1)
  (elite-station-ui-switch-to-widget-buffer)
)

(defun elite-station-ui-scroll-message-buffer-end ()
  (interactive)
  (select-window elite-station-message-window)
  (switch-to-buffer elite-station-message-buffer)
  (goto-char (point-max))
  (elite-station-ui-switch-to-widget-buffer)

)

(defun elite-station-ui-scroll-message-buffer (&optional up lines)
  
  (let (
	(original-other-buffer)
	)
    (if (not lines)
	(setq lines 1)
      )
    (setq original-other-buffer other-window-scroll-buffer)
    (setq other-window-scroll-buffer elite-station-message-buffer)
    (if up
	(scroll-other-window (- lines))
      (scroll-other-window lines)
      )
    (setq other-window-scroll-buffer original-other-buffer)
    )
  )

(defun elite-station-ui-switch-to-widget-buffer ()
  "Switch active window to space command shell"

  (select-window elite-widget-window)
  (switch-to-buffer elite-widget-buffer)
;;   (select-window elite-space-message-window)
;;   (switch-to-buffer elite-space-message-buffer)
  (set-cursor-color elite-cursor-color)
  )

(provide 'elite-windows)