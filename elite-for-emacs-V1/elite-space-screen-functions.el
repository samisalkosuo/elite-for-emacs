;;; elite-space-screen-functions.el -  Functions for Emacs-Elite 
;                                      user interface when in space

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

(defvar elite-space-shell-start-point 2417
  "Starting point of elite space shell for commands and messages.")

(defvar elite-position-in-universe 0
  "Player position.
  0= docked, station position
  1= outside station
  2= 
  1...10
  safe zone 0,1,2
  ")
  
(defvar elite-sun-position 20
  "Sun position in space.")

(defvar elite-space-screen-use-windows t
  "t if using separate windows and buffers")

(defvar elite-space-buffer nil
  "Buffer for starfield, outside window from players ship")

(defvar elite-space-message-buffer nil
  "Buffer for showing text while in space")

(defvar elite-space-widget-buffer nil
  "Buffer for message in space")

(defvar elite-space-sensor-buffer nil
  "Buffer for sensors in space")

(defvar elite-space-window nil
  "Window for starfield, outside window from players ship")

(defvar elite-space-message-window nil
  "Window for showing text while in space")

(defvar elite-space-widget-window nil
  "Window for message in space")

(defvar elite-space-sensor-window nil
  "Window for sensors in space")

(defun elite-space-ui-initialize-windows ()
  "Creates buffers and windows for spaceflight"
  (let (
	
	)
    	(if (not (one-window-p))
	    (progn
	      (delete-other-windows)
	      (switch-to-buffer elite-buffer-name)
	      )
	  )

	;;create buffers
	;;     (if elite-space-buffer
	;; 	(kill-buffer elite-space-buffer))
	(setq elite-space-buffer (get-buffer-create "Elite Space Screen"))

	;;     (if elite-space-message-buffer
	;; 	(kill-buffer elite-space-message-buffer))
     (setq elite-space-message-buffer (get-buffer-create "Elite Space Message Screen"))

     (if elite-space-widget-buffer
	 (kill-buffer elite-space-widget-buffer))
     (setq elite-space-widget-buffer (get-buffer-create "Elite Space Widget Screen"))

     (setq elite-space-sensor-buffer (get-buffer-create "Elite Space Sensor Screen"))

     ;create windows
    (setq elite-space-window (selected-window))
    (setq elite-space-message-window (split-window elite-space-window 20))
    (setq elite-space-widget-window (split-window elite-space-message-window 57 t))
    (setq elite-space-sensor-window (split-window elite-space-widget-window 7))

    (set-window-buffer elite-space-window elite-space-buffer)
    (set-window-buffer elite-space-message-window elite-space-message-buffer)
    (set-window-buffer elite-space-widget-window elite-space-widget-buffer)
    (set-window-buffer elite-space-sensor-window elite-space-sensor-buffer)

    ;set modelines
    (select-window elite-space-window)
    (use-local-map elite-mode-map)
    (setq local-abbrev-table elite-mode-abbrev-table)
    (set-syntax-table elite-mode-syntax-table)
    (setq major-mode 'elite-mode)
    (setq mode-line-format (list
			    "In space..."  
; 			    "Condition: "
; 			    (commander-condition elite-commander)
			    "%-"))
    (force-mode-line-update)

    
    (select-window elite-space-message-window)
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

    (elite-space-ui-create-widget-buffer)
;;     (use-local-map elite-mode-map)
;;     (setq local-abbrev-table elite-mode-abbrev-table)
;;     (set-syntax-table elite-mode-syntax-table)
;;     (setq major-mode 'elite-mode)    
    (setq mode-line-format (list   
			    "Commands"
			    "%-"))
    (force-mode-line-update)

    (select-window elite-space-sensor-window)
    (use-local-map elite-mode-map)
    (setq local-abbrev-table elite-mode-abbrev-table)
    (set-syntax-table elite-mode-syntax-table)
    (setq major-mode 'elite-mode)
    (setq mode-line-format (list   
			    "Sensor readings (malfunction: HUD readings inaccurate)"
			    "%-"))
    (force-mode-line-update)

    (elite-switch-to-widget-buffer)
    (goto-char 0)
    )
  )

(defvar elite-space-ui-show-widget-help t
  "If t show widget help in minibuffer")
;;(setq elite-space-ui-show-widget-help t)

(defvar elite-space-ui-widget-buffer-created nil
  "If t show widget help in minibuffer")

(defun elite-space-ui-create-widget-buffer ()
  (let (
	(my-widget-keymap)
	)
    (if (not elite-space-ui-widget-buffer-created)
	(progn
	  (select-window elite-space-widget-window)
	  (switch-to-buffer elite-space-widget-buffer)
	  (erase-buffer)
	  ;;(setq elite-space-ui-widget-buffer-created t)
	  ;;(widget-insert "Here is some documentation.\n\n")

	  (widget-insert " ")
	  (widget-create 'push-button
			 :notify (lambda (&rest ignore)
				   (elite-space-ui-scroll-message-buffer-up)
				   )
			 :help-echo (lambda (&rest ignore)
				      (if elite-space-ui-show-widget-help
					  "Scroll message window up (shift-up arrow)"
					)
				      )
			 "u")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-space-ui-scroll-message-buffer-down)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Scroll message window down (shift-down arrow)"
				  )
				)
		   "d")
    (widget-insert "\n ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-jump-toward-planet)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Jump toward planet (j)"
				  )
				)
		   "Jump toward planet")
    (widget-insert "\n ")
    (widget-create 'push-button	
		   :notify (lambda (&rest ignore)
			     (elite-jump-toward-sun)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Jump toward sun (m)"
				  )
				)
		   "Jump toward sun")
    (widget-insert "\n ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-hyperspace-to)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Hyperspace to selected system (h)"
				  )
				)
		   "Hyperspace")    
    (widget-insert "\n ")
    (widget-create 'push-button	
		   :notify (lambda (&rest ignore)
			     (elite-dock)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Dock to station (d)"
				  )
				)
		   "Dock")    
    (widget-insert "\n ")
    (widget-create 'push-button	
		   :notify (lambda (&rest ignore)
			     (elite-local-systems)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
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
				(if elite-space-ui-show-widget-help
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
				(if elite-space-ui-show-widget-help
				    "Display commander info (c)"
				  )
				)
		   "Commander info")
    (widget-insert "\n ")
    (widget-create 'push-button	
		   :notify (lambda (&rest ignore)
			     (elite-current-system-info)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Display current system info (I)"
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
			     (elite-select-hyperspace-system)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Select hyperspace system (s)"
				  )
				)
		   "Select hyperspace system")
    (widget-insert "\n ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (elite-galactic-jump)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Select hyperspace system (G)"
				  )
				)
		   "Galactic hyperspace")
    (widget-insert "\n ")
    (widget-create 'push-button	
		   :notify (lambda (&rest ignore)
			     (elite-list-galaxy)
			     )
 		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
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
				(if elite-space-ui-show-widget-help
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
			     (elite-quit)
			     )
		   :help-echo (lambda (&rest ignore)
				(if elite-space-ui-show-widget-help
				    "Quit Elite for EMACS (Q)"
				  )
				)
		   "Quit")

    (widget-minor-mode 1)

    (setq my-widget-keymap widget-keymap);(make-keymap))
    (define-key my-widget-keymap [up] 'widget-backward)
    (define-key my-widget-keymap [down] 'widget-forward)
    (define-key my-widget-keymap [left] 'widget-backward)
    (define-key my-widget-keymap [right] 'widget-forward)
    (define-key my-widget-keymap [(shift up)] 'elite-space-ui-scroll-message-buffer-up)
    (define-key my-widget-keymap [(shift down)] 'elite-space-ui-scroll-message-buffer-down)
    (define-key my-widget-keymap [(control prior)] 'elite-space-ui-scroll-message-buffer-up-page)
    (define-key my-widget-keymap [(control next)] 'elite-space-ui-scroll-message-buffer-down-page)
    (define-key my-widget-keymap [(control home)] 'elite-space-ui-scroll-message-buffer-start)
    (define-key my-widget-keymap [(control end)] 'elite-space-ui-scroll-message-buffer-end)

    (define-key my-widget-keymap "Q" 'elite-quit)
    (define-key my-widget-keymap "p" 'elite-path)
    (define-key my-widget-keymap "j" 'elite-jump-toward-planet)
    (define-key my-widget-keymap "m" 'elite-jump-toward-sun)
    (define-key my-widget-keymap "h" 'elite-hyperspace-to)
    (define-key my-widget-keymap "d" 'elite-dock)
    (define-key my-widget-keymap "l" 'elite-local-systems)
    (define-key my-widget-keymap "i" 'elite-display-inventory)
    (define-key my-widget-keymap "c" 'elite-display-commander-info)
    (define-key my-widget-keymap "I" 'elite-current-system-info)
    (define-key my-widget-keymap "s" 'elite-select-hyperspace-system)
    (define-key my-widget-keymap "G" 'elite-galactic-jump)
    (define-key my-widget-keymap "L" 'elite-list-galaxy)
    (define-key my-widget-keymap "N" 'elite-system-info)
    (if elite-special-mission-1-accepted
	(progn
	  (define-key my-widget-keymap "\M-1" 'elite-special-mission-1-show-status)
	  )
	)

    (use-local-map my-widget-keymap)
    (widget-setup)

    ;;      (use-local-map elite-mode-map)
    ;;      (setq local-abbrev-table elite-mode-abbrev-table)
    ;;      (set-syntax-table elite-mode-syntax-table)
    ;;      (setq major-mode 'elite-mode)

;;     (goto-char 0)
    ;;     (widget-forward 1)

    )
      )
    )
  )

(defun elite-space-ui-scroll-message-buffer-up-page ()
  (interactive)
  (elite-space-ui-scroll-message-buffer t 12)
)

(defun elite-space-ui-scroll-message-buffer-down-page ()
  (interactive)
  (elite-space-ui-scroll-message-buffer nil 12)

)

(defun elite-space-ui-scroll-message-buffer-up ()
  (interactive)
  (elite-space-ui-scroll-message-buffer t)
)

(defun elite-space-ui-scroll-message-buffer-down ()
  (interactive)
  (elite-space-ui-scroll-message-buffer)

)

(defun elite-space-ui-scroll-message-buffer-start ()
  (interactive)
  (select-window elite-space-message-window)
  (switch-to-buffer elite-space-message-buffer)
  (goto-char 1)
  (elite-switch-to-widget-buffer)


)

(defun elite-space-ui-scroll-message-buffer-end ()
  (interactive)
  (select-window elite-space-message-window)
  (switch-to-buffer elite-space-message-buffer)
  (goto-char (point-max))
  (elite-switch-to-widget-buffer)

)

(defun elite-space-ui-scroll-message-buffer (&optional up lines)
  
  (let (
	(original-other-buffer)
	)
    (if (not lines)
	(setq lines 1)
      )
    (setq original-other-buffer other-window-scroll-buffer)
    (setq other-window-scroll-buffer elite-space-message-buffer)
    (if up
	(scroll-other-window (- lines))
      (scroll-other-window lines)
      )
    (setq other-window-scroll-buffer original-other-buffer)
    )
  )


(defun elite-ui-space-initialize ()
  "Creates ui outline for space."
  (let
      (
       (current-row)
       (chars)
       (tmp)
       )
    (if elite-space-screen-use-windows
	(elite-space-ui-initialize-windows)
      (progn
	
	;(setq buffer-read-only nil)
    ;draw lines
    (erase-buffer)
    (goto-char 0)
    (elite-ui-insert-line "+" "-" "-" "-" elite-screen-middle-column)
    
    (setq current-row (my-what-line))
    ;(setq current-row (string-to-number (nth 1 (split-string (my-what-line)))))
    (setq tmp (+ 5 elite-screen-middle-line))
    (while (< current-row (- elite-screen-height 2))
      (if (= current-row tmp)
	  (elite-ui-insert-line "|" "+" "-" "-" elite-screen-middle-column)
	(progn
	  (if (> current-row tmp)
	      (elite-ui-insert-line "|" "|" " " " " elite-screen-middle-column)
	    (elite-ui-insert-line "|" " " " " " " elite-screen-middle-column)
	    )
	  )
	)
      (setq current-row (my-what-line))
      ;(setq current-row (string-to-number (nth 1 (split-string (my-what-line)))))
      )
    (elite-ui-insert-line "+" "+" "-" "-" elite-screen-middle-column t)
    
    (elite-ui-refresh)
    
    ;(setq buffer-read-only t)
    )
      )
    )
  )

(defun elite-space-ui-refresh-windows ()
  "Refreshes space windows"
  (elite-space-ui-starfield)

  ;ai commander activity
  ;set modelines
  (select-window elite-space-message-window)
  (switch-to-buffer elite-space-message-buffer)
  (setq mode-line-format (list   
			  (elite-short-local-system-info)
			  "  "
			  "Day: "
			  (number-to-string elite-day)
			  "%-"))
  (force-mode-line-update)
  ;;continue ai commander activity
  (elite-ai-commander-continue-activity)
  (elite-switch-to-widget-buffer)
)

(defun elite-space-ui-refresh ()
  "Refresher space screen."
  ;(elite-space-ui-set-title "commander")
  ;(elite-display-commander-info)
    (if elite-space-screen-use-windows
	(elite-space-ui-refresh-windows)
      (progn
	(elite-space-ui-starfield)
	(elite-mode-set-space-mode-line)
  ;(elite-insert-position-indicator)
	(goto-char (point-max))
	)
      )
)

(defun elite-space-ui-enemy-ship (ship-class)
  "Draws crude silhuette for approaching ship"
  (let( 
      (ship-silhouette)
      (start-point)
      (i)
      (len)
      (tmp)
    ) 
    (elite-space-ui-starfield)
    (if elite-space-screen-use-windows
	(progn
	  (select-window elite-space-window)
	  (switch-to-buffer elite-space-buffer)
	  (set-cursor-color elite-background-color)
	  (setq ship-silhouette (elite-get-ship-silhouette ship-class))
	   ;(goto-char (- (+ (* 10 elite-screen-width) (/ elite-screen-width 2) (length ship-silhouette))))
	  (setq len (length ship-silhouette))
	  ;(setq i 6)
	  (setq i 0)
	  (while ship-silhouette
	    (setq tmp (car ship-silhouette))
	    (setq len (length tmp))
	    (goto-char (- (+ (* i elite-screen-width) (/ elite-screen-width 2)) (/ len 2)))
	    (insert tmp)
	    (delete-char len)
	    (setq i (1+ i))
	    (setq ship-silhouette (cdr ship-silhouette))
	    )
	  
	  (elite-switch-to-widget-buffer)
	  )
      (progn
	

  ;(setq buffer-read-only nil)
  (setq ship-silhouette (elite-get-ship-silhouette ship-class))
  ;(goto-char (- (+ (* 10 elite-screen-width) (/ elite-screen-width 2) (length ship-silhouette))))
  (setq len (length ship-silhouette))
  (setq i 7)
  (while ship-silhouette
    (setq tmp (car ship-silhouette))
    (setq len (length tmp))
    (goto-char (- (+ (* i elite-screen-width) (/ elite-screen-width 2)) (/ len 2)))
    (insert tmp)
    (delete-char len)
    (setq i (1+ i))
    (setq ship-silhouette (cdr ship-silhouette))
    )
  
  
  ;(setq buffer-read-only t)
))
  )
)

(defun elite-enemy-destroyed-animation (ship-class)
  "Enemy destruction animation"
  (let (
	(i)
	(ship-silhouette)
	(start-point)
	(middle-point)
	(tmp)
	(len)
	(delay 35)
	(size 9);was 7
	)
    ;(message "KABOOM!")
    (if elite-space-screen-use-windows
	(progn
	  (select-window elite-space-window)
	  (switch-to-buffer elite-space-buffer)
	  (set-cursor-color elite-background-color)
	  (setq ship-silhouette (elite-get-ship-silhouette ship-class))
	  (setq tmp (car ship-silhouette))
	  (setq len (length tmp))
	  (setq start-point  (/ elite-screen-width 2))
	  ;(setq start-point  (+ (* 5 elite-screen-width) (/ elite-screen-width 2)))
	  (setq middle-point (+ start-point (* elite-screen-width (/ (length ship-silhouette) 2))))
	  (setq start-point (- middle-point elite-screen-width 1))
	  
	  (setq i 0)
	  (let (
		(point2 (1+ start-point))
	   (point3 (+ 2 start-point))
	   (point4 (1+ middle-point))
	   (point5 (+ middle-point elite-screen-width 1))
	   (point6 (+ middle-point elite-screen-width))
	   (point7 (1- (+ middle-point elite-screen-width)))
	   (point8 (1- middle-point))
	   )
       
       (while (< i size)
	 (elite-enemy-exploding (list (- start-point (* i elite-screen-width) (* i 1)) "\\" (- point2 (* i elite-screen-width)) "|" (+ (- point3 (* i elite-screen-width)) (* i 1)) "/" (+ point4 i) "-" (+ point5 (* i elite-screen-width) (* i 1)) "\\" (+ point6 (* i elite-screen-width)) "|" (- (+ point7 (* i elite-screen-width)) (* i 1)) "/" (- point8 (* i 1)) "-"));first round
	 (sit-for 0 delay)
	 (setq i (1+ i))
	 )
       (setq i 0)
       (while (< i size)
	 (elite-enemy-exploding (list (- start-point (* i elite-screen-width) (* i 1)) " " (- point2 (* i elite-screen-width)) " " (+ (- point3 (* i elite-screen-width)) (* i 1)) " " (+ point4 i) " " (+ point5 (* i elite-screen-width) (* i 1)) " " (+ point6 (* i elite-screen-width)) " " (- (+ point7 (* i elite-screen-width)) (* i 1)) " " (- point8 (* i 1)) " "));first round
	 (sit-for 0 delay)
	 (setq i (1+ i))
	 )
        )


	  (elite-switch-to-widget-buffer)
	  )
      (progn
    ;(setq buffer-read-only nil)
    (setq ship-silhouette (elite-get-ship-silhouette ship-class))
    (setq tmp (car ship-silhouette))
    (setq len (length tmp))
    (setq start-point  (+ (* 7 elite-screen-width) (/ elite-screen-width 2)))
    (setq middle-point (+ start-point (* elite-screen-width (/ (length ship-silhouette) 2))))
    (setq start-point (- middle-point elite-screen-width 1))

     (setq i 0)
     (let (
	   (point2 (1+ start-point))
	   (point3 (+ 2 start-point))
	   (point4 (1+ middle-point))
	   (point5 (+ middle-point elite-screen-width 1))
	   (point6 (+ middle-point elite-screen-width))
	   (point7 (1- (+ middle-point elite-screen-width)))
	   (point8 (1- middle-point))
	   )
       
       (while (< i 8)
	 (elite-enemy-exploding (list (- start-point (* i elite-screen-width) (* i 1)) "\\" (- point2 (* i elite-screen-width)) "|" (+ (- point3 (* i elite-screen-width)) (* i 1)) "/" (+ point4 i) "-" (+ point5 (* i elite-screen-width) (* i 1)) "\\" (+ point6 (* i elite-screen-width)) "|" (- (+ point7 (* i elite-screen-width)) (* i 1)) "/" (- point8 (* i 1)) "-"));first round
	 (sit-for 0 delay)
	 (setq i (1+ i))
	 )
       (setq i 0)
       (while (< i 8)
	 (elite-enemy-exploding (list (- start-point (* i elite-screen-width) (* i 1)) " " (- point2 (* i elite-screen-width)) " " (+ (- point3 (* i elite-screen-width)) (* i 1)) " " (+ point4 i) " " (+ point5 (* i elite-screen-width) (* i 1)) " " (+ point6 (* i elite-screen-width)) " " (- (+ point7 (* i elite-screen-width)) (* i 1)) " " (- point8 (* i 1)) " "));first round
	 (sit-for 0 delay)
	 (setq i (1+ i))
	 )
        )
     ;(setq buffer-read-only t)
     )
      )
    )
  )

(defun elite-enemy-exploding (points)
  ""
  (while points
    (goto-char (car points))
    (setq points (cdr points))
    (insert (car points))
    (delete-char 1)
    (setq points (cdr points))
    )
)

(defun elite-space-ui-set-title (section)
  "Sets section title in one of ui section."
  (let
      (
       (title)
       (title-length)
       )
  ;(setq buffer-read-only nil)

  (if (string= section "commander")
      (progn
	(setq title (concat "Commander " elite-commander-name))
	(setq title-length (length title))
	(goto-char 2359);(+ 6 elite-screen-width))
	(insert title)
	(delete-char title-length)
	)
    )


  ;(setq buffer-read-only t)

  )
)

(defun elite-insert-position-indicator (msg)
  "Inserts position indicator to screen."
  (let 
      ( 
       (i 2474)
       (tmp)
       )
	(if elite-space-screen-use-windows
	   (progn
	     (elite-space-sensor-shell msg t)
	     )
	  (progn
	    ;(setq buffer-read-only nil)

					;delete two lines 
	    (goto-char i)
	    (insert (make-string 55 ? ))
	    (delete-char 55)
	    (goto-char i)

    ;(setq tmp (concat "Position " (number-to-string elite-position-in-universe) ". "))
;     (if (< elite-position-in-universe (- elite-sun-position 4))
; 	(concat (tmp  "Fying toward the sun..")))
;     (if (= elite-position-in-universe (- elite-sun-position 4))
; 	(concat (tmp  "It is getting hotter.")))
;     (if (= elite-position-in-universe (- elite-sun-position 3))
; 	(concat (tmp "You can feel the heat.")))
;     (if (= elite-position-in-universe (- elite-sun-position 2))
; 	(concat (tmp "The heat is on!")))
;     (if (= elite-position-in-universe (- elite-sun-position 1))
; 	(concat (tmp "The heat is unbearable. Scoop fuel or leave.. Now.")))
	    (insert msg)
	    (delete-char (length msg))
	    ;(setq buffer-read-only t)   
	    )
	  )
    )
  )

;sensor functions in space
(defvar elite-space-sensor-starting-point 2589
  "Starting point for sensors.")

(defun elite-space-sensor-insert-text (txt)
  "Insert text into sensor screen"
  (let (
	
	)	
    ;(setq buffer-read-only nil)
    (goto-char elite-space-sensor-starting-point)
    (insert txt)
    (delete-char (length txt))
    ;(setq buffer-read-only t)
    )
  )

(defun elite-space-insert-text (txt &optional type)
  "Insert text in space screen."
  (if type
      (progn
	
	(if (string= type "shell")
	    (elite-space-ui-shell txt)
	  )

	(if (string= type "commander-info")
	    (progn
	      (elite-space-ui-display-commander-info nil t)
	      (elite-space-ui-display-commander-info txt)
	      )
	  )
	

	)
    (progn
	(elite-space-ui-shell txt)
      )
    )
)


(defun elite-space-ui-display-commander-info (txt &optional clear)
  "Displays commander info."
  (let (
	(text-list)
	(tmp)
	(i)
	)
    (if clear
	(progn
	  (setq i 1)
	  (while (< i 11)
	    (goto-char (+ 2359 (* i elite-screen-width)))
	    (insert (make-string 30 ? ))
	    (delete-char 30)
	    (setq i (1+ i))
	    )	  
	  )
      (progn
	(setq i 1)
	;(setq txt "tete")
	(setq text-list (split-string txt "\n"))
	(while text-list
	  (setq tmp (car text-list))
	  (goto-char (+ 2359 (* i elite-screen-width)))
	  (insert tmp)
	  (delete-char (length tmp))
	  (setq i (1+ i))
	  (setq text-list (cdr text-list))
	  )
	))
    
    ))

;elite-space-shell functions
(defvar elite-space-message-shell-inserted-day 0
  "")

(defun elite-space-message-shell (txt)
  "Shell buffer for space message shell"
  (let (
	(txt-list)
	)
    (if t
	(progn
	  (elite-space-command-shell txt)
	  )
      (progn
	(select-window elite-space-widget-window)
	(switch-to-buffer elite-space-widget-buffer)
	(set-cursor-color elite-background-color)
	;;(erase-buffer)
	(goto-char (point-max))
	(if (> elite-day elite-space-message-shell-inserted-day)
	    (progn
	      (insert "Day: " (number-to-string elite-day) "\n")
	      (setq elite-space-message-shell-inserted-day elite-day)
	      )
	  )    
	(setq txt-list (elite-split-string-for-shell txt))
	(while txt-list
	  (insert (car txt-list) "\n")
	  (setq txt-list (cdr txt-list))
	  )
	(elite-switch-to-widget-buffer)
	)
      )
    )
  )

(defun elite-space-sensor-shell (txt &optional position-indicator)
  "Shell buffer for space message shell"
  (let (
	(txt-list)
	)
    (select-window elite-space-sensor-window)
    (switch-to-buffer elite-space-sensor-buffer)
    (set-cursor-color elite-background-color)
    (if position-indicator
	(progn
	  (select-window elite-space-window)
	  (setq mode-line-format (list
				  "In space..."
				  txt
				  "%-"))
	  (force-mode-line-update)


; 	  (goto-char 0)
; 	  (insert " ")
; 	  (goto-char 0)
; 	  (kill-line)
; 	  ;(insert txt "\n")
; 	  (insert txt )
	  )
      (progn
	(goto-char (point-max))
	;(insert "\n")
	(setq txt-list (elite-split-string-for-shell txt))
	(while txt-list
	  (insert (car txt-list) "\n")
	  (setq txt-list (cdr txt-list))
	  )
	)
      )
    (elite-switch-to-widget-buffer)
    )
  )

(defun elite-space-command-shell (txt)
  "Shell buffer for window UI"
  (let (
	(txt-list)
	)
    (select-window elite-space-message-window)
    (switch-to-buffer elite-space-message-buffer)
    (set-cursor-color elite-background-color)
    (goto-char (point-max))
    ;(setq txt "tret\netetttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt")
    (setq txt-list (elite-split-string-for-shell txt))
    (while txt-list
      (insert (car txt-list) "\n")
      (setq txt-list (cdr txt-list))
      )
    (elite-switch-to-widget-buffer)
    )
  )

(defun elite-switch-to-widget-buffer ()
  "Switch active window to widget window"

  (select-window elite-space-widget-window)
  (switch-to-buffer elite-space-widget-buffer)
;;   (select-window elite-space-message-window)
;;   (switch-to-buffer elite-space-message-buffer)
  (set-cursor-color elite-cursor-color)
  )

(defun elite-space-ui-shell (txt &optional clear)
  "Insert text in space ui commands & messages area."
  (let (
	(start-point)
	(i)
	(j)
	(tmp)
	(tmp2)
	(text-list)
	(writable-text-list (list))
	)
    (if clear
	(progn
	  ;clear shell screen or some of it
	  )
      (progn

	(if elite-space-screen-use-windows
	   (progn
	     (elite-space-command-shell txt)
	     )
	  (progn
	    (setq start-point (elite-space-ui-shell-starting-point))
	    (goto-char start-point)
	;check new lines.. make list
	
	;check list contents and cut string to max elite-shell-width
	;make new list if lines with correct width
	;loop through list check position after each write
	;if all list longer than 16 lines wait user input after writing 
	;shell screen like "more.."
	(setq text-list (split-string txt "\n"))
	(while text-list
	  (setq tmp (car text-list))
	  (if (> (length tmp) elite-shell-width)
	      (progn
		(while (> (length tmp) elite-shell-width)
		  (setq tmp2 (substring tmp 0 elite-shell-width))
		  (setq writable-text-list (append writable-text-list (list tmp2)))
		  (setq tmp (substring tmp elite-shell-width (length tmp)))
		  )
		(setq writable-text-list (append writable-text-list (list tmp)))
		)
	    (progn
	      (setq writable-text-list (append writable-text-list (list tmp)))
	      )
	    )
	  (setq text-list (cdr text-list))
	  )
	
	(while writable-text-list
	  (goto-char (elite-space-ui-shell-starting-point))
	  (setq tmp (car writable-text-list)) 
	  (insert tmp)
	  (delete-char (length tmp))
	  (setq writable-text-list (cdr writable-text-list))
	  )
	(goto-char (point-max))
	)
      )
	    )
	  )
    )
  )

(defun elite-split-string-for-shell (txt)
  "Splits string for displaying in command shell"
  (let (
	(text-list)
	(writable-text-list (list))
	(tmp)
	(tmp2)
	)
	(setq writable-text-list (list))
	;check list contents and cut string to max elite-shell-width
	;make new list if lines with correct width
	;loop through list check position after each write
	;if all list longer than 16 lines wait user input after writing 
	;shell screen like "more.."
	(setq text-list (split-string txt "\n"))
	(while text-list
	  (setq tmp (car text-list))
	  (if (> (length tmp) elite-shell-width)
	      (progn
		(while (> (length tmp) elite-shell-width)
		  (setq tmp2 (substring tmp 0 elite-shell-width))
		  (setq writable-text-list (append writable-text-list (list tmp2)))
		  (setq tmp (substring tmp elite-shell-width (length tmp)))
		  )
		(setq writable-text-list (append writable-text-list (list tmp)))
		)
	    (progn
	      (setq writable-text-list (append writable-text-list (list tmp)))
	      )
	    )
	  (setq text-list (cdr text-list))
	  )
	writable-text-list
)

)
(defun elite-space-ui-shell-starting-point ()
  "Finds start point for text in elite-shell."
  (let (
	(char-in-shell)
	(i)
	(tmp)
	)
    (goto-char elite-space-shell-start-point)
    (setq char-in-shell (buffer-substring elite-space-shell-start-point (1+ elite-space-shell-start-point)))
    ;(message (concat "char-in-shell: " char-in-shell)) 
    (while (and (not (string= char-in-shell "-")) (not (string= char-in-shell " ")))
      (goto-char (+ elite-screen-width (point)))
      (setq char-in-shell (buffer-substring (point) (1+ (point))))
      )
    (if (string= char-in-shell "-")
	;last row must clear shell screen
	(progn
	  ;clear shell screen or some of it
	  (setq tmp (read-string "More.."))
	  ;todo: save messages to file..
	  (setq i 0)
	  (while (< i 11)
	    (goto-char (+ elite-space-shell-start-point (* i elite-screen-width)))
	    (insert (make-string elite-shell-width ?  ))
	    (delete-char elite-shell-width)
	    (setq i (1+ i))
	    )
	  (max elite-space-shell-start-point)
	  )
      (point)
	)
    )
  )



;drawing functions

(defun elite-space-ui-starfield ()
  "Prints starfield."
  (let (
	(number-of-stars 200)
	(lines 19)
	(star)
	(i)
	(j)
	(starseed)
	)
    (if elite-space-screen-use-windows
	(progn
  	  (select-window elite-space-window)
  	  (switch-to-buffer elite-space-buffer)
	  (set-cursor-color elite-background-color)
 	  (erase-buffer)
	  (setq i 0)
	  (while (< i 19)
	    (if (= i 18)
		(insert (make-string (1- elite-screen-width) ? ))
	      (insert (make-string (1- elite-screen-width) ? ) "\n")
	    )
	    (setq i (1+ i))
	    )

 					;height  19 lines
 					;width 113 chars
 					;create random generator galaxy+planetnumber as seed
 	  (setq elite-starfield-seed (string-to-number (concat (number-to-string currentplanet) (number-to-string galaxynum))))
 					;    (setq elite-starfield-seed (string-to-number (concat currentplanet galaxynum elite-position-in-universe)))
 					;(random t)
 	  (setq i 0)
 	  (while (< i lines)
 	    (setq j 0)
 	    (while (< j 4)
 					;(setq starseed (1+ starseed));(string-to-number (concat starseed i j)))
 	      (setq star (+ 2 (elite-starfield-rand 110)))
 	      (goto-char (+ star (* i elite-screen-width)))
 	      (insert ".")
 	      (delete-char 1)
 	      (setq j (1+ j))
 	      )
 	    
 	    (setq i (1+ i))
 	    )
	  (elite-switch-to-widget-buffer)

	  )
      (progn
    ;(setq buffer-read-only nil)
    (elite-space-ui-clear-starfield)
    ;height  19 lines
    ;width 113 chars
    ;create random generator galaxy+planetnumber as seed
    (setq elite-starfield-seed (string-to-number (concat (number-to-string currentplanet) (number-to-string  galaxynum))))
;    (setq elite-starfield-seed (string-to-number (concat currentplanet galaxynum elite-position-in-universe)))
    ;(random t)
    (setq i 1)
    (while (<= i lines)
      (setq j 0)
      (while (< j 4)
	;(setq starseed (1+ starseed));(string-to-number (concat starseed i j)))
	(setq star (+ 2 (elite-starfield-rand 110)))
	(goto-char (+ star (* i elite-screen-width)))
	(insert ".")
	(delete-char 1)
	(setq j (1+ j))
	)
      
      (setq i (1+ i))
      )
    ;(setq buffer-read-only t)
    )
      )
    )
  )

(defun elite-space-ui-clear-starfield ()
  "Clears star field."
  (let (
	(i)
	)
    (setq i 1)
    (while (<= i 19)
      (goto-char (+ 2 (* i elite-screen-width)))
      (insert (make-string 112 ?  ))
      (delete-char 112)
      (setq i (1+ i))
      )
    )
  )


(defvar elite-starfield-seed 1
  "Seed for starfield random generator.")
  

(defun elite-starfield-rand (maxvalue)
  "Random generator for starfield. Seed is currentplanet and galaxy number + star number.
   Taken from Numerical Recipes. See http://www.ulib.org/webRoot/Books/Numerical_Recipes/bookcpdf.html.
   Return number between 0-255."
  (let (
	(next)
	(rnd 65536)
	(i)
	)
    (setq i 0)
    (while (> rnd maxvalue)
      (setq elite-starfield-seed  (+ (* elite-starfield-seed 1103515245) 12345))
      (setq rnd (logand (% (/ elite-starfield-seed 65536) 32758) 255))
      (setq i (1+ i))
      )
    (max rnd)
    )
  )

; unsigned long next=1;
; int elite-starfield-rand(void) /* NOT RECOMMENDED (see text) */
; {
; next = next*1103515245 + 12345;
; return (unsigned int)(next/65536) % 32768;
; }
; void srand(unsigned int seed)
; {
; next=seed;
; }


(provide 'elite-space-screen-functions)