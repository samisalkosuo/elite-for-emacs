;;; elite-screen-functions.el -  Functions for Emacs-Elite user interface

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

;Variables for user interface

(defvar elite-original-screen-width 115
  "Orignal screen width to be restored after exiting Elite."
)
(defvar elite-original-screen-height 35
  "Orignal screen width to be restored after exiting Elite."
)

(defvar elite-original-frame-title ""
  "Original frame title.")

(defvar elite-original-cursor-color ""
  "Original cursor color.")

(defvar elite-original-foreground-color ""
   "Original foreground color.")
 
(defvar elite-original-background-color ""
   "Original background color.")

(defvar elite-screen-width 115
  "Screen width for Elite user interface."
)
(defvar elite-screen-height 35
  "Screen height for Elite user interface."
)

(defvar elite-background-color "black"
  "Background color for Elite")

(defvar elite-foreground-color "darkgray"
  "Foreground color for Elite")

(defvar elite-cursor-color "red"
  "Cursor color for Elite")

(defvar elite-original-scroll-bar-mode nil
  "Original scroll bar mode")

(defvar elite-original-cursor-in-non-selected-windows nil
  "Original curson in non selected windows")

;(defvar elite-frame-title "Elite for EMACS")

(defvar elite-screen-middle-column 0
  "Screen middle column.")

(defvar elite-screen-middle-line 0
  "Screen middle line.")

(defvar elite-market-display-max-item-length 24
  "Max length of item row in market display.")

(defvar elite-shell-start-point 1842
  "Starting point of elite shell for commands and messages.")

(defvar  elite-shell-width 55
  "Width of elite shell.")

(defun elite-ui-station-initialize ()
  "Creates user interface outline for stations."
  (let
      (
       (current-row)
       (chars)
       )
    (if elite-use-windows
	(progn
	  (elite-initialize-windows)
	  )
      (progn
    ;(setq buffer-read-only nil)
    ;draw lines
    (erase-buffer)
    (goto-char 0)
    (elite-ui-insert-line "+" "+" "-" "-" elite-screen-middle-column)
    
    ;(setq current-row (string-to-number (nth 1 (split-string (what-line)))))
    (setq current-row (my-what-line))

  (while (< current-row (- elite-screen-height 2))
    (if (= current-row elite-screen-middle-line)
	(progn
	  (elite-ui-insert-line "|" "+" "-" "-" elite-screen-middle-column)
	  )
      (elite-ui-insert-line "|" "|" " " " " elite-screen-middle-column)
      )
    (setq current-row (my-what-line))
;    (setq current-row (string-to-number (nth 1 (split-string (my-what-line)))))
    )
  (elite-ui-insert-line "+" "+" "-" "-" elite-screen-middle-column t)

  (elite-ui-refresh)
  ;(setq buffer-read-only t)
  ))
  )
  )


(defun elite-ui-refresh ()
  "Refreshes user interface."

  (elite-update-commander)
  

  (if (elite-is-docked)
      (progn
    (if elite-use-windows
	(progn
	  (elite-refresh-windows)
	  )
      (progn
	(elite-display-market)
	;(elite-display-equipment)
	(elite-local-systems)
	(elite-display-inventory)
	(elite-display-commander-info)
	(elite-ui-title "local-system")
	;(elite-ui-title "messages")
	(elite-ui-title "commander")
	(elite-mode-set-station-mode-line)
	)
      )
)
    (progn
      (elite-space-ui-refresh)    
      )
    )
  ;;(goto-char (point-max))
  )

(defun elite-ui-insert-line (start-char middle-char fill-char fill-char2 elite-screen-middle-column &optional no-end-of-line)
  "Helper function that writes a line like +----+----+"
  (insert start-char)
  (while (< (current-column) elite-screen-middle-column)
    (insert fill-char)
    )
  (insert middle-char)
  (while (< (current-column) (- elite-screen-width 2))
    (insert fill-char2)
    )
  (insert start-char)
  (if no-end-of-line
      ()
    (insert "\n"))
)

(defun my-what-line ()
  "Returns current line number."
  (interactive)
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(beginning-of-line)
	(setq start (point))
	(goto-char opoint)
	(beginning-of-line)
	(1+ (count-lines 1 (point)))
; 	(if (/= start 1)
; 	    (message "line %d (narrowed line %d)"
; 		     (1+ (count-lines 1 (point)))
; 		     (1+ (count-lines start (point))))
; 	  (message "Line %d" (1+ (count-lines 1 (point))))
; 	  )
	)
      )
    )
  )


(defun elite-ui-title (section)
  "Sets section title in one of ui section."
  (let
      (
       (title)
       (title-length)
       
       )
  ;(setq buffer-read-only nil)
  (if (string= section "local-system")
      (progn
	(setq title (elite-short-local-system-info))
	(setq title-length (length title))
	(goto-char (+ (/ elite-screen-width 2) 5 ))
	(insert title)
	(delete-char title-length)
	)
    )
  

  (if (string= section "messages")
      (progn
	;(setq title "Commands & Messages")
	(setq title "Shell")	
	(setq title-length (length title))
	(goto-char (+ 6 (* (1- elite-screen-middle-line) elite-screen-width)))
	(insert title)
	(delete-char title-length)
	)
    )

  (if (string= section "commander")
      (progn
	(setq title (concat "Commander " elite-commander-name))
	(setq title-length (length title))
	(goto-char 3);(+ 6 elite-screen-width))
	(insert title)
	(delete-char title-length)
	)
    )

  ;(setq buffer-read-only t)

  )
)



(defun elite-insert-text(txt &optional type)
  "Writes given string to screen.
   Parameter type is type what kind of message.
   All fucntions that want to print something calls this function."
  ;(setq buffer-read-only nil);Elite buffer read-only
  (if (elite-is-docked)
      (progn

	(if type
	    (progn 
	      ;if type present format text according to type
	      (if (string= type "market")
		  (progn 
		    (elite-ui-display-market nil t)	    
		    (elite-ui-display-market txt)
		    )
		)
	      
	      (if (string= type "equipment")
		  (progn 
		    (elite-ui-display-system-equipment nil t)
		    (elite-ui-display-system-equipment txt)
		    )
		)
	      
	      
	      (if (string= type "local-systems")
		  (progn 
		    (elite-ui-display-local-systems nil t)	    
		    (elite-ui-display-local-systems txt)
		    )
		)
	      
	      (if (string= type "shell")
		  (progn 
					;(elite-ui-shell nil t)	    
		    (elite-ui-shell txt)
		    )
		)
	      
	      (if (string= type "commander-info")
		  (progn 
					;(elite-ui-shell nil t)	    
		    (if elite-use-windows
			(progn
			  (elite-command-shell txt)
			  )
		      (progn
			(elite-ui-display-commander-info nil t)
			(elite-ui-display-commander-info txt)
			)
		      )
		    )
		)
	      
	      (if (string= type "inventory")
		  (progn 
					;(elite-ui-shell nil t)	    
		    (if elite-use-windows
			(progn
			  (elite-command-shell txt)
			  )
		      (progn
			(elite-ui-display-inventory nil t)
			(elite-ui-display-inventory txt)
			)
		      )
		    )
		)
	      
	      )
	  (progn
	    (elite-ui-shell txt) 
	    )
	  )
	
	)
    (progn
;       (if type
; 	  (elite-space-insert-text txt type)
	(elite-space-insert-text txt)
	;)
      
      )
    )
  ;(setq buffer-read-only t)
  ;;(goto-char (point-max))
  )

(defun elite-ui-display-local-systems (local-systems-text &optional clear-local-system-display)
  "Prints systems within 7 light years."
  (let (
	(local-systems-list)
	(i)
	(j)
	(local-system-item)
	)
    (if clear-local-system-display
	(progn
	  ;(setq i (+ AlienItems 3))
	  (setq i AlienItems)
	  (setq j 0)
	  (while (< j 13)
	    (goto-char (+ (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	    (insert (make-string 54 ?  ))
	    (delete-char 54)
	    (setq j (1+ j))
	    (setq i (1+ i))
	    )
	  )
      (progn
	;(setq i (+ AlienItems 3))
	(setq i  AlienItems )
	(goto-char (+ (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	(setq local-systems-list (split-string local-systems-text "\n"))
	(while local-systems-list
	  (setq local-system-item (car local-systems-list))
	  (insert local-system-item)
	  (delete-char (length local-system-item))
	  (setq local-systems-list (cdr local-systems-list))
	  (setq i (1+ i))
	  (goto-char (+ (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	  )	       
	)
      )
    ))

(defun elite-ui-display-market (market-text &optional clear-market-display)
  "Displays market."
  (let (
	(market-list)
	(i)
	(list-item)
	)
    (if elite-use-windows
	(progn
	  (if clear-market-display
	      ()
	    (elite-display-market-window market-text)
	    )
	  )
      (progn
	(if clear-market-display
	    (progn
	      (setq i 0)
	  (goto-char (+ (+ elite-screen-middle-column (* elite-screen-width (1+ i))) 3))
	  (while (< i 9);(1+ AlienItems))
	    (insert (make-string 54 ? ));elite-market-display-max-item-length ? ))
	    (delete-char 54);elite-market-display-max-item-length)
	    (setq i (1+ i))
	    (goto-char (+ (+ elite-screen-middle-column (* elite-screen-width (1+ i))) 3))
	    )
	  )
      (progn
	(setq i 1)
	(goto-char (+ (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	(setq market-list (split-string market-text "\n"))
	(while market-list
	  (setq list-item (car market-list))
	  (setq market-list (cdr market-list))
	  (setq list-item (concat list-item (make-string (- 27 (length list-item)) ? ) (car market-list)))
	  (insert list-item)
	  (delete-char (length list-item))
	  (setq market-list (cdr market-list))
	  (setq i (1+ i))
	  (goto-char (+ (+ elite-screen-middle-column (* elite-screen-width i)) 3))
; 	  (setq list-item (car market-list))
; 	  (insert list-item)
; 	  (delete-char (length list-item))
; 	  (setq market-list (cdr market-list))
; 	  (setq i (1+ i))
; 	  (goto-char (+ (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	  )
	))
    ))
    ))

(defun elite-ui-display-system-equipment (txt &optional clear)
  "Displays available equipment in local system."
  (let (
	(i)
	(equip-list)
	(list-item)
	)
    (if clear
	(progn
	  (setq i 0) 
	  (while (< i 4);number-of-equip-items)
	    (goto-char (+ (* i elite-screen-width) 1324));(goto-char (+ elite-market-display-max-item-length (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	    ;(insert (make-string elite-market-display-max-item-length ? ))
	    (insert (make-string 54 ? ))
	    (delete-char 54)
	    (setq i (1+ i))	    
	    )	  
	  
	  )
      (progn
	(setq i 0)
	(setq equip-list (split-string txt "\n"))
	(while equip-list
	  (goto-char (+ (* i elite-screen-width) 1324));(+ elite-market-display-max-item-length (+ elite-screen-middle-column (* elite-screen-width i)) 3))
	  (setq list-item (car equip-list))
	  (setq equip-list (cdr equip-list))
	  (setq list-item (concat list-item " " (car equip-list) " "))
	  (if (= i 0)
	      (progn
		(setq equip-list (cdr equip-list))
		(setq list-item (concat list-item " " (car equip-list) " "))
		)
	    )
	  (insert list-item)
	  (delete-char (length list-item))
	  (setq equip-list (cdr equip-list))
	  (setq i (1+ i))

	  )
	))
    
    ))

(defun elite-ui-display-commander-info (txt &optional clear)
  "Displays commander info."
  (let (
	(text-list)
	(tmp)
	(i)
	)
    (if clear
	(progn
	  (setq i 1)
	  (while (< i 14)
	    (goto-char (+ 3 (* i elite-screen-width)))
	    (insert (make-string 30 ? ))
	    (delete-char 30)
	    (setq i (1+ i))
	    )	  
	  )
      (progn
	(setq i 1)
	(setq text-list (split-string txt "\n"))
	(while text-list
	  (setq tmp (car text-list))
	  (goto-char (+ 3 (* i elite-screen-width)))
	  (insert tmp)
	  (delete-char (length tmp))
	  (setq i (1+ i))
	  (setq text-list (cdr text-list))
	  )
	))
    
    ))

(defun elite-ui-display-inventory (txt &optional clear)
  "Displays inventory."
  (let (
	(text-list)
	(tmp)
	(i)
	)
    (if clear
	(progn
	  (setq i 1)
	  (while (< i 14)
	    (goto-char (+ 6 (* i elite-screen-width) (/ elite-screen-width 4)))
	    (insert (make-string 19 ? ))
	    (delete-char 19)
	    (setq i (1+ i))
	    )	  
	  )
      (progn
	(setq i 1)
	(setq text-list (split-string txt "\n"))
	(while text-list
	  (setq tmp (car text-list))
	  (goto-char (+ 6 (* i elite-screen-width) (/ elite-screen-width 4)))
	  (insert tmp)
	  (delete-char (length tmp))
	  (setq i (1+ i))
	  (setq text-list (cdr text-list))
	  )
	))
    ))

;elite-shell functions

(defun elite-ui-shell (txt &optional clear)
  "Insert text in commands & messages area."
  (let (
	(start-point)
	(i)
	(j)
	(tmp)
	(tmp2)
	(text-list)
	(writable-text-list (list))
	)
    (if elite-use-windows
	(progn
	  (elite-command-shell txt)
	  )
      (progn
    (if clear
	(progn
	  ;clear shell screen or some of it
	  )
      (progn
	(setq start-point (elite-ui-shell-starting-point))
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
	  (goto-char (elite-ui-shell-starting-point))
	  (setq tmp (car writable-text-list)) 
	  (insert tmp)
	  (delete-char (length tmp))
	  (setq writable-text-list (cdr writable-text-list))
	  )
	)
      )
    )
  
)))

(defun elite-ui-shell-starting-point ()
  "Finds start point for text in elite-shell."
  (let (
	(char-in-shell)
	(char2-in-shell)
	(i)
	(tmp)
	)
    (goto-char elite-shell-start-point)
    ;(setq char-in-shell (buffer-substring elite-shell-start-point (1+ elite-shell-start-point)))
    (setq char-in-shell (buffer-substring elite-shell-start-point (+ 2 elite-shell-start-point)))
    ;(setq char2-in-shell (buffer-substring (1+ elite-shell-start-point) (+ 2 elite-shell-start-point)))

    ;(message (concat "char-in-shell: " char-in-shell)) 
    (while (and (not (string= char-in-shell "--")) (not (string= char-in-shell "  "))
		;(not (string= char-in-shell " "))
		;(not (string= char2-in-shell " "))
		)
		;(not (string= " " (buffer-substring (1+ (point)) (+ 2 (point))))))
      (goto-char (+ elite-screen-width (point)))
      (setq char-in-shell (buffer-substring (point) (+ 2 (point)))) 
;        (setq char-in-shell (buffer-substring (point) (1+ (point)))) 
;       (setq char2-in-shell (buffer-substring (1+ (point)) (+ 2 (point))))
     )
    (if (string= char-in-shell "--")
	;last row must clear shell screen
	(progn
	  ;clear shell screen or some of it
	  (setq tmp (read-string "More.."))
	  ;save messages to file..
	  (setq i 0)
	  (while (< i 16)
	    (goto-char (+ elite-shell-start-point (* i elite-screen-width)))
	    (insert (make-string elite-shell-width ?  ))
	    (delete-char elite-shell-width)
	    (setq i (1+ i))
	    )
	  (max elite-shell-start-point)
	  )
      (point)
	)
    )
  )

(defun prisys (plan-s &optional compressed dist)
  "Returns system info as string on specified system."  
  (let(
       (sys-info)
       )
  (if compressed
      (progn
	(setq sys-info 
	      (concat 
	       (format "%10s" (plansys-name plan-s))
	       (format " TL: %2i" (1+ (plansys-techlevel plan-s)))
	       (format " %12s " (aref econnames (plansys-economy plan-s)))
	       (format " %15s" (aref govnames (plansys-govtype plan-s)))
	       ))
	)
    (progn
	(setq sys-info 
	      (concat 
	       "System:  "
	       (plansys-name plan-s)
	       (format "\nPosition (%i,%i)" (plansys-x plan-s) (plansys-y plan-s))
	       (if dist
		   (format "\nDistance %.1f LY" (/ (distance plan-s (aref galaxy currentplanet)) 10.0))
		 )
	       (format "\nEconomy: (%i) " (plansys-economy plan-s))
	       (aref econnames (plansys-economy plan-s))
	       (format "\nGovernment: (%i) " (plansys-govtype plan-s))
	       (aref govnames (plansys-govtype plan-s))
	       (format "\nTech level: %i " (1+ (plansys-techlevel plan-s)))
	       (format "\nTurnover: %i " (plansys-productivity plan-s))
	       (format "\nRadius: %i " (plansys-radius plan-s))
	       (format "\nPopulation: %i Billion" (lsh (plansys-population plan-s) -3))
	       "\n"
	       ;"Description coming soon"
;	       (elite-planet-description plan-s)
	       ))

      ))

  (substring sys-info 0)
  ))

(defun elite-short-local-system-info (&optional system-index)
  "Return short local system description."
  (let (
	(planet-index)
	)
    (if system-index
	(setq planet-index system-index)
      (setq planet-index currentplanet)
      )
    (concat 
     (plansys-name (aref galaxy planet-index))
     " "
     "TL: "
     (number-to-string (1+ (plansys-techlevel (aref galaxy planet-index))))
     " "
     (aref econnames (plansys-economy (aref galaxy planet-index)))
     " "
     (aref govnames (plansys-govtype (aref galaxy planet-index)))
     )
    ))

(defun elite-show-help ()
  "Shows help"
  (interactive)
  (let
      (
       (msg)
       ) 
    (setq msg (concat
	       ">Help\n"
	       "Elite for EMACS is based on Elite series by Ian Bell and David Braben.\n"
	       "Original Elite, (C) 1984 Ian Bell and David Braben.\n"
	       "Elite for EMACS uses code from original Elite and it is (C) 1984 Ian Bell and David Braben."
; 	       "Elite for EMACS " 
; 	       emacs-elite-version
; 	       " is based on Elite by Ian Bell and David Braben.\n"
; 	       "Elite for EMACS " 
; 	       emacs-elite-version
; 	       " , © 2001 Sami Salkosuo (sami@roguemail.net).\n"
; 	       "Original Elite, © 1984 Ian Bell and David Braben.\n"
; 	       "...\n"
; 	       "Commands:\n"
; 	       "M-h show help\n"
; 	       "C-i current system info\n"
; 	       "M-i other system info\n"
; 	       "C-f buy fuel\n"
; 	       "C-b buy goods\n"
; 	       "C-s sell goods\n"
; 	       "M-s select hyperspace system\n"
; 	       "C-e buy equipment\n"
; 	       "C-u undock\n"
; 	       "C-l show systems within 7.0 Light Years\n"
; 	       "M-l show systems in galaxy\n"
; 	       "C-p show path to specified system\n"
; 	       "C-cC-c display commander info\n"
; 	       "M-e display available equipment"
	       )
	  )
    (elite-insert-text msg)
    ;(describe-mode)
    )
  )

;startup

(defvar elite-animation-interval 200
  "Interval between animation")

(defun elite-startup ()
  "Shows startup screen and does animation."
  (let (
	(orig 52)
	(start 52)
	(i)
	)
    (goto-char 0)
    (insert elite-startup-screen-no-text)
    
    ;(setq start (elite-animate-e start))
      
    (setq start (elite-animate-e start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-space start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-l start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-space start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-i start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-space start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-t start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-space start))
    (sit-for 0 elite-animation-interval)
    (setq start (elite-animate-e start))
    (sit-for 0 elite-animation-interval)      
    
    (goto-char 631)
    (insert "for EMACS")
    (delete-char 9)
    
    (sit-for 3)
    (setq elite-animation-interval 0)
    (while (sit-for 0 1)
      (setq start orig)
      (setq i 0)
      (while (< i 17)
	(setq start (elite-animate-clear start))
	  (setq i (1+ i))
	  )
      (goto-char 631)
      (insert "         ")
      (delete-char 9)
      
      (sit-for 1)
      (setq start orig)
      
      (setq start (elite-animate-e start))
      (setq start (elite-animate-space start))
      (setq start (elite-animate-l start))
      (setq start (elite-animate-space start))
      (setq start (elite-animate-i start))
      (setq start (elite-animate-space start))
      (setq start (elite-animate-t start))
      (setq start (elite-animate-space start))
      (setq start (elite-animate-e start))
	
      (goto-char 631)
      (insert "for EMACS")
      (delete-char 9)
      
      (sit-for 1)
      )
    
    (setq start orig)
    (setq start (elite-animate-e start))
    (setq start (elite-animate-space start))
    (setq start (elite-animate-l start))
    (setq start (elite-animate-space start))
    (setq start (elite-animate-i start))
    (setq start (elite-animate-space start))
    (setq start (elite-animate-t start))
    (setq start (elite-animate-space start))
    (setq start (elite-animate-e start))
    (goto-char 631)
    (insert "for EMACS")
    (delete-char 9)
    )
  )

(defun elite-animate-e (start)
  "Write letter e"
  (elite-animate start      (vector "*" "*" "*" "*" "*"))
  (setq start (1+ start))
      (sit-for 0 elite-animation-interval)      
  (elite-animate start (vector "*" " " "*" " " "*"))
  (setq start (1+ start))
      (sit-for 0 elite-animation-interval)      
  (elite-animate start (vector "*" " " "*" " " "*"))
  (1+ start)
  )

(defun elite-animate-space (start)
  "Write space"
  (elite-animate start (vector " " " " " " " " " "))
  (1+ start)
  )

(defun elite-animate-l (start)
  "Write letter l"
  (elite-animate start (vector "*" "*" "*" "*" "*"))
  (setq start (1+ start))
      (sit-for 0 elite-animation-interval)      
  (elite-animate start (vector " " " " " " " " "*"))
  (setq start (1+ start))
      (sit-for 0 elite-animation-interval)      
  (elite-animate start (vector " " " " " " " " "*"))
  (1+ start)
  )

(defun elite-animate-i (start)
  "Write letter l"
  (elite-animate start (vector "*" "*" "*" "*" "*"))
  (1+ start)
  )

(defun elite-animate-t (start)
  "Write letter l"
  (elite-animate start (vector "*" " " " " " " " "))
  (setq start (1+ start))
      (sit-for 0 elite-animation-interval)      
  (elite-animate start (vector "*" "*" "*" "*" "*"))
  (setq start (1+ start))
      (sit-for 0 elite-animation-interval)      
  (elite-animate start (vector "*" " " " " " " " "))
  (1+ start)
  )

(defun elite-animate-clear (start)
  (elite-animate-space start)
)

(defun elite-animate (start-point char-array)
  ""
  (let (
	(i)
	(tmp)
	)
    (setq i 0)
    (while (< i (length char-array))
      (goto-char (+ (* i elite-screen-width) start-point))
      (setq tmp (aref char-array i))
      (insert tmp)
      (delete-char (length tmp))
      (setq i (1+ i))
      )
    )
  )

(defvar elite-startup-screen 
"

                                      /\\         *** *   * *** ***         /\\                                   
                                     /  \\        *   *   *  *  *          /  \\                                  
                                     \\   \\       *** *   *  *  ***       /   /                                  
                                      \\   \\      *   *   *  *  *        /   /                                   
                                       \\   \\     *** *** *  *  ***     /   /                                    
                                       /\\   \\        for EMACS        /   /\\                                   
                                      /  \\   \\                       /   /  \\                                  
                                      \\   \\  /\\        |\\_/|        /\\  /   /                                
                                       \\    /  \\       | _ |       /  \\    /                                   
                                       /\\   \\   \\      |   |      /   /   /\\                                  
                                       \\ \\   \\   \\   _/ \\-/ \\_   /   /   / /                                
                        ________________\\    /   /__|-       -|__\\   \\    /________________                    
                       |____________________________|\\       /|____________________________|                     
                          __________________________  \\     /____________________________                        
                         |____________________________|\\___/|____________________________|                
                             ____________________________^____________________________         
                            |________________________   / \\   ________________________|                      
                              |____________________   _/ ^ \\_   ____________________|                       
                                  _________________| /  / \\  \\ |_________________                        
                                 |__________________/  / ^ \\  \\__________________|                      
                                   |__________________/ / \\ \\__________________|                           
                                                     /  / \\  \\                                               
                                                    /__/ ^ \\__\\                                               
                                                        / \\                                                
                                                       / ^ \\                                                 
                                                      /_/ \\_\\                                                  
                                                         ^                                                   
                                                                                                              ")

(defvar elite-startup-screen-no-text 
"

                                      /\\                                   /\\                                     
                                     /  \\                                 /  \\                                    
                                     \\   \\                               /   /                                    
                                      \\   \\                             /   /                                     
                                       \\   \\                           /   /                                      
                                       /\\   \\                         /   /\\                                     
                                      /  \\   \\                       /   /  \\                                  
                                      \\   \\  /\\        |\\_/|        /\\  /   /                                
                                       \\    /  \\       | _ |       /  \\    /                                   
                                       /\\   \\   \\      |   |      /   /   /\\                                 
                                       \\ \\   \\   \\   _/ \\-/ \\_   /   /   / /                                
                        ________________\\    /   /__|-       -|__\\   \\    /________________                   
                       |____________________________|\\       /|____________________________|                     
                          ____________________________\\     /____________________________                  
                         |____________________________|\\___/|____________________________|                
                             ____________________________^____________________________         
                            |________________________   / \\   ________________________|                      
                              |____________________   _/ ^ \\_   ____________________|                       
                                  _________________| /  / \\  \\ |_________________                        
                                 |__________________/  / ^ \\  \\__________________|                      
                                   |__________________/ / \\ \\__________________|                           
                                                     /  / \\  \\                                               
                                                    /__/ ^ \\__\\                                               
                                                        / \\                                                
                                                       / ^ \\                                                 
                                                      /_/ \\_\\                                                  
                                                         ^                                                   
                                                                                                              ")

(provide 'elite-screen-functions)