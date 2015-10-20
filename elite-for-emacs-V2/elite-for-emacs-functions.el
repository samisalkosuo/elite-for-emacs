;;; elite-for-emacs-functions.el - Elite for EMACS functions

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


(defun elite-for-emacs-create-new-commander (name gender no-random-home)
  (let (

	)
    ;;(setq name "Sami")
    ;;(setq gender "male")
    (if no-random-home
	(elite-for-emacs-generate-commander name -1 gender 7 0 0)
      (elite-for-emacs-generate-commander name -1 gender (random galsize) 0 0)
      )

    )
  )

(defun elite-for-emacs-get-galaxy (num)
  "Returns galaxy array of galaxy <num>"
  (aref elite-for-emacs-galaxies-in-universe num)
)

(defun elite-for-emacs-get-system-name (galaxy-index system-index)
  "Returns system name of planet in galaxy."
;(elite-get-system-name 41)
  (plansys-name (aref (elite-for-emacs-get-galaxy galaxy-index) system-index))
)

(defun elite-for-emacs-get-current-system-name ()
  "Returns system name of planet in galaxy."
;(elite-get-system-name 41)
  (plansys-name (aref (elite-for-emacs-get-galaxy (elite-for-emacs-commander-current-galaxy (car elite-for-emacs-commander-list))) (elite-for-emacs-commander-current-planet (car elite-for-emacs-commander-list))))
)

(defun elite-for-emacs-get-plansys (galaxy-index system-index)
  "Returns system plansys struct."
;(elite-get-system-name 41)
  (aref (elite-for-emacs-get-galaxy galaxy-index) system-index)
)


(defun elite-for-emacs-get-system-index (galaxy-index system-name &optional within-jump)
"Return index of system-name in galaxy."
  (let (
	(syscount)
	(d 9999)
	(p)
	(temp)
	(cmdr)
	(galaxy)
	)
    (setq syscount 0)
    (setq cmdr (car elite-for-emacs-commander-list))
    (setq p (elite-for-emacs-commander-current-planet cmdr))
    (setq galaxy (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)))
    (while (< syscount galsize)
      (setq temp (string-match (upcase system-name) (elite-for-emacs-get-system-name galaxy-index syscount)))
      (if temp
	  (if (= temp 0)  ;(plansys-name (aref galaxy syscount)))
	      (progn
		(if (and within-jump (<= (distance (aref galaxy syscount) (aref galaxy (elite-for-emacs-commander-current-planet cmdr))) maxfuel  ))
		    (progn
		      (setq p syscount)
		      (setq syscount 99999)
		      )
		  (progn
		    (if (not within-jump)
			(progn
			  (setq p syscount)
			  (setq syscount 99999)
			  )
		      )

		    )
		  )
		)
	    )
      )
      (setq syscount (1+ syscount))
      )	
    p
    )
  )


(defun elite-for-emacs-short-local-system-info (galaxy-index system-index &optional no-format)
  "Return short local system description."
  (let (
	(planet-index)
	(galaxy)
	)
    (setq planet-index system-index)
    (setq galaxy (elite-for-emacs-get-galaxy galaxy-index))
    (if no-format
	    (concat
	     (plansys-name (aref galaxy planet-index))
	     " TL: "
	     (number-to-string (1+ (plansys-techlevel (aref galaxy planet-index))))
	     " "
	     (aref econnames (plansys-economy (aref galaxy planet-index)))
	     " "
	     (aref govnames (plansys-govtype (aref galaxy planet-index)))
	     )
      (concat
       (format "%10s" (plansys-name (aref galaxy planet-index)))
       (format " TL: %2i " (1+ (plansys-techlevel (aref galaxy planet-index))))
       (format "%12s" (aref econnames (plansys-economy (aref galaxy planet-index))))
       (format " %15s" (aref govnames (plansys-govtype (aref galaxy planet-index))))
       )
      )
;; 	  printf("%10s",plsy.name);
;;   	printf(" TL: %2i ",(plsy.techlev)+1);
;;   	printf("%12s",econnames[plsy.economy]);
;;   	printf(" %15s",govnames[plsy.govtype]);

    )
  )

(defun prisys (plan-s &optional compressed)
  "Returns system info as string on specified system."
  (let(
       (sys-info)
       (cmdr)
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
      (setq cmdr (car elite-for-emacs-commander-list ))
    (setq galaxy (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)))

	(setq sys-info
	      (concat
	       "System:  "
	       (plansys-name plan-s)
	       (format "\nPosition (%i,%i)" (plansys-x plan-s) (plansys-y plan-s))
	       (format "\nDistance %.1f LY" (/ (distance plan-s (aref (aref elite-for-emacs-galaxies-in-universe (elite-for-emacs-commander-current-galaxy cmdr)) (elite-for-emacs-commander-current-planet cmdr))) 10.0)  )
	       (format "\nEconomy: (%i) " (plansys-economy plan-s))
	       (aref econnames (plansys-economy plan-s))
	       (format "\nGovernment: (%i) " (plansys-govtype plan-s))
	       (aref govnames (plansys-govtype plan-s))
	       (format "\nTech level: %i " (1+ (plansys-techlevel plan-s)))
;":%5d M CR
	       (format "\nGross Productivity: %5d M CR" (plansys-productivity plan-s))
	       (format "\nRadius: %i " (plansys-radius plan-s))
	       (format "\nPopulation: %i Billion" (lsh (plansys-population plan-s) -3))
	       ;;(format "\nPopulation: %i Billion" (plansys-population plan-s))
	       ;;" (" ")"
	       "\n"
	       (elite-for-emacs-planet-description (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-get-system-index (elite-for-emacs-commander-current-galaxy cmdr) (plansys-name plan-s)))


	       ))

      ))
  sys-info
  ))

(defun elite-for-emacs-trade-good-index (tradegood)
  "Returns index of tradegood in tradegood array."
  (let (
	(i)
	(index)
	(len)
	)
    (setq len (length tradnames))
    (setq i 0)
    (while (< i len)
      (if (string-match tradegood (aref tradnames i))
	  (progn
	    (setq index i)
	    (setq i 9999)
	    )
	)
      (setq i (1+ i))
      )
    index

    )
)

(defun elite-for-emacs-match-trade-item (trade-item)
  "Return first match of trade-item string"
  (let (
	(len)
	(item)
	(item-name)
	(i)
	)
    (setq len (length commodities))
    (setq i 0)
    (while (< i len)
      (setq item (tradegood-name (aref commodities i)))
      (setq i (1+ i))
      )
    (catch 'found

      )

    )
  )


(defun elite-for-emacs-get-first-equipment-match (equipment-name)
  "Return first equipment struct where name matches."
  (let (
	(equipment-list)
	(temp)
	(index)
	(equip nil)
	)
    ;;(setq equipment-name "carg")
    (setq equipment-list elite-for-emacs-equipment-list)
    (while equipment-list
      (setq temp (car equipment-list))
      (setq index (string-match equipment-name (equipment-name temp)))
      (if (and index (= index 0))
	  (progn
	    (setq equipment-list nil)
	    (setq equip temp)
	    )
	      )
      (setq equipment-list (cdr equipment-list))
      )
    equip
    )
  )


(defun elite-for-emacs-insert ()
  "Inserts elite-for-emacs in buffer"
  (interactive)
  (insert "elite-for-emacs-")
)
(define-key emacs-lisp-mode-map "\C-c\C-l" 'elite-for-emacs-insert)

(defun elite-for-emacs-insert-defun-skeleton ()
  (interactive)
  (let (
	(pnt)
	)
    (insert "(defun elite-for-emacs-")
    (setq pnt (point))
    (insert " ()\n")
    (lisp-indent-line)
    (insert "\"\"\n")
    (lisp-indent-line)
    (insert "(let (\n")
    (lisp-indent-line)
    (insert "(cmdr)")
    (insert "\n")
    (lisp-indent-line)
    (insert ")\n")
    (lisp-indent-line)
    (insert "(setq cmdr (car elite-for-emacs-commander-list))\n\n")
    (lisp-indent-line)
    (insert ")\n")
    (lisp-indent-line)
    (insert ")\n")
    (goto-char pnt)

    )
)
(define-key emacs-lisp-mode-map "\C-c\C-d" 'elite-for-emacs-insert-defun-skeleton)
