;;; elite-functions.el -  Functions (like makesystem etc) for Emacs-Elite

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

(defun mysrand (s)
  "Seed random generator."
  (setq lastrand (- s 1))
)

(defun myrand ()
  "Custom random generator to ensure repeatability. Return next pseudo-random number.
   Random generator is not 1:1 as in txtelite v.1.3, probably because emacs uses 28 bit integers.
   Todo: a way to calculate 32 bit numbers with 28-bit.."
  ;check sign, if negative put one bit to memory?
  (let (
	(next-random 0)
	) 
    ;calculate next pseudo-rnd
    ;As supplied by D McDonnell	from SAS Insititute C
    (setq next-random (+ (- (lsh (- (lsh (+ (lsh (+ (lsh (- (lsh lastrand 3) lastrand) 3) lastrand) 1) lastrand) 4) lastrand) 1) lastrand) 3680))
    (setq lastrand (- (logand next-random 134217727) 1))
    next-random
    )
  )

(defun randbyte ()
  (let (
	(tmp)
	)
  (setq tmp (logand (myrand) 255))
  (setf (commander-fluct elite-commander) tmp)
  ;(elite-insert-text (concat "randbyte: " (number-to-string tmp)))
  tmp
  )
)




(defun mymin (a b)
  (if (< a b)
      (+ a 0)
    (+ b 0))
  )

(defun populate-tradnames ()
  "Populates tradnames array with tradegood names from commodities"
  (let (
	(commodities-length (length commodities))
	(counter 0)
	)
  
  (while (< counter commodities-length)
    (aset tradnames counter (concat (car (split-string (tradegood-name (aref commodities counter)) " "))))
;    (setq tradnames (vconcat tradnames (tradegood-name (aref commodities counter))))
    (setq counter (1+ counter))
    )
  ))

(defun populate-equipment-stock ()
  "Populates equipment stock array."
  (let (
	(i)
       )
    (setq i 0)
    (aset equipment-stock i (elite-make-equip-item 0 0 1 1 2 "Fuel" "EQ_FUEL"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 1 300 "Missile" "EQ_MISSILE"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 1 4000 "Large Cargo Bay" "EQ_CARGO_BAY"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 2 6000 "E.C.M. System" "EQ_ECM"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 5 5250 "Fuel Scoops" "EQ_FUEL_SCOOPS"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 6 10000 "Escape Pod" "EQ_ESCAPE_POD"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 7 9000 "Energy Bomb" "EQ_ENERGY_BOMB"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 8 15000 "Energy Unit" "EQ_ENERGY_UNIT"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 9 15000 "Docking Computers" "EQ_DOCK_COMP"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 10 50000 "Galactic Hyperdrive" "EQ_GAL_DRIVE"))
    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 3 4000 "Pulse Laser" "EQ_PULSE_LASER"))
;    (setq i (1+ i))
;    (aset equipment-stock i (elite-make-equip-item 0 0 1 3 0 "-Pulse Laser" "EQ_PULSE_LASER"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 1 3 4000 ">Front" "EQ_FRONT_PULSE"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 1 3 4000 ">Rear" "EQ_REAR_PULSE"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 1 3 4000 ">Left" "EQ_LEFT_PULSE"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 1 3 4000 ">Right" "EQ_RIGHT_PULSE"))

    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 4 10000 "Beam Laser" "EQ_BEAM_LASER"))
;    (setq i (1+ i))
;    (aset equipment-stock i (elite-make-equip-item 0 0 0 4 0 "-Beam Laser" "EQ_BEAM_LASER"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 4 10000 ">Front" "EQ_FRONT_BEAM"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 4 10000 ">Rear" "EQ_REAR_BEAM"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 4 10000 ">Left" "EQ_LEFT_BEAM"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 4 10000 ">Right" "EQ_RIGHT_BEAM"))

    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 10 8000 "Mining Laser" "EQ_MINING_LASER"))
;    (setq i (1+ i))
;    (aset equipment-stock i (elite-make-equip-item 0 0 0 10 0 "-Mining Laser" "EQ_MINING_LASER"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 8000 ">Front" "EQ_FRONT_MINING"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 8000 ">Rear" "EQ_REAR_MINING"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 8000 ">Left" "EQ_LEFT_MINING"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 8000 ">Right" "EQ_RIGHT_MINING"))

    (setq i (1+ i))
    (aset equipment-stock i (elite-make-equip-item 0 0 1 10 60000 "Military Laser" "EQ_MILITARY_LASER"))
;    (setq i (1+ i))
;    (aset equipment-stock i (elite-make-equip-item 0 0 0 10 0 "-Military Laser" "EQ_MILITARY_LASER"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 60000 ">Front" "EQ_FRONT_MILITARY"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 60000 ">Rear" "EQ_REAR_MILITARY"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 60000 ">Left" "EQ_LEFT_MILITARY"))
;     (setq i (1+ i))
;     (aset equipment-stock i (elite-make-equip-item 0 0 0 10 60000 ">Right" "EQ_RIGHT_MILITARY"))
))


(defun elite-equip-present (type)
  "Returns t if user can buy equipment in current system."
  (let (
	(canbuy)
	)
    (if (string= type "EQ_FUEL")
	(setq canbuy (>= (commander-fuel elite-commander) maxfuel)))

    (if (string= type "EQ_MISSILE")
	(setq canbuy (>= (commander-missiles elite-commander) 4)))

    (if (string= type "EQ_CARGO_BAY")
	(setq canbuy (commander-large-cargo-bay elite-commander)));(> (commander-cargo-capacity elite-commander) 20)))

    (if (string= type "EQ_ECM")
	(setq canbuy  (commander-ecm elite-commander)))

    (if (string= type "EQ_FUEL_SCOOPS")
	(setq canbuy (commander-fuel-scoops elite-commander)))

    (if (string= type "EQ_ESCAPE_POD")
	(setq canbuy (commander-escape-pod elite-commander)))

    (if (string= type "EQ_ENERGY_BOMB")
	(setq canbuy (commander-energy-bomb elite-commander)))

    (if (string= type "EQ_ENERGY_UNIT")
	(setq canbuy (commander-energy-unit elite-commander)))

    (if (string= type "EQ_DOCK_COMP")
	(setq canbuy (commander-docking-computer elite-commander)))

    (if (string= type "EQ_GAL_DRIVE")
	(setq canbuy (commander-galactic-hyperdrive elite-commander)))

    (if (string= type "EQ_PULSE_LASER")
	(setq canbuy (and 
		       (string= "EQ_PULSE_LASER" (commander-front-laser elite-commander))
		       (string= "EQ_PULSE_LASER" (commander-rear-laser elite-commander))
		       (string= "EQ_PULSE_LASER" (commander-right-laser elite-commander))
		       (string= "EQ_PULSE_LASER" (commander-left-laser elite-commander))
		       )
	      ))

    (if (string= type "EQ_BEAM_LASER")
	(setq canbuy  (and 
		       (string= "EQ_BEAM_LASER" (commander-front-laser elite-commander))
		       (string= "EQ_BEAM_LASER" (commander-rear-laser elite-commander))
		       (string= "EQ_BEAM_LASER" (commander-right-laser elite-commander))
		       (string= "EQ_BEAM_LASER" (commander-left-laser elite-commander))
		       )
	      ))

    (if (string= type "EQ_MINING_LASER")
	(setq canbuy  (and 
		       (string= "EQ_MINING_LASER" (commander-front-laser elite-commander))
		       (string= "EQ_MINING_LASER" (commander-rear-laser elite-commander))
		       (string= "EQ_MINING_LASER" (commander-right-laser elite-commander))
		       (string= "EQ_MINING_LASER" (commander-left-laser elite-commander))
		       )
	      ))
    (if (string= type "EQ_MILITARY_LASER")
	(setq canbuy  (and
		       (string= "EQ_MILITARY_LASER" (commander-front-laser elite-commander))
		       (string= "EQ_MILITARY_LASER" (commander-rear-laser elite-commander))
		       (string= "EQ_MILITARY_LASER" (commander-right-laser elite-commander))
		       (string= "EQ_MILITARY_LASER" (commander-left-laser elite-commander))
		       )
	      ))    
    (not canbuy)
    ))


(defun elite-make-equip-item (canbuy y show techlevel price name type)
  "Makes equip_item struct."

  (make-equipitem
   :canbuy canbuy
   :y y
   :techlevel techlevel
   :show show
   :price price
   :name name
   :type type
   )
)

(defun elite-equipment-price (equipment-type)
  "Returns price of equipment."
  (let (
	(i)
	(equipment)
	(price)
	)
    (setq i 0)
    ;set equipment stock correspondin current planet
    (while (< i number-of-equip-items)
      (setq equipment (aref equipment-stock i))
      (if (string= (equipitem-type equipment) equipment-type)
	  (setq price (equipitem-price equipment))
	  )
      (setq i (1+ i))
      )
    (max price)
    )
  )

(defun elite-equipment-type (equipment-name)
  "Returns name of equipment."
  (let (
	(i)
	(equipment)
	(type)
	)
    (setq i 0)
    ;set equipment stock correspondin current planet
    (while (< i number-of-equip-items)
      (setq equipment (aref equipment-stock i))
      (if (string= (equipitem-name equipment) equipment-name)
	  (setq type (equipitem-type equipment))
	  )
      (setq i (1+ i))
      )
    type
    )
  )

(defun elite-equipment-name (equipment-type)
  "Returns name of equipment based on type."
  (let (
	(i)
	(equipment)
	(name)
	)
    (setq i 0)
    ;set equipment stock correspondin current planet
    (while (< i number-of-equip-items)
      (setq equipment (aref equipment-stock i))
      (if (string= (equipitem-type equipment) equipment-type)
	  (setq name (equipitem-name equipment))
	  )
      (setq i (1+ i))
      )
    name
    )
  )

(defun elite-available-equipment (&optional system)
  "Return string of available equipment.
   String format like * EQ_FUEL\n- EQ_MISSILE etc."
  (let (
	(tech_level)
	(i)
	(equipment)
	(local-equipment "")
	)
    (if system
	(setq tech_level (1+ (plansys-techlevel (aref galaxy system))))
      (setq tech_level (1+ (plansys-techlevel (aref galaxy currentplanet))))
      )
    (setf (equipitem-price (aref equipment-stock 0)) (* elite-fuelcost (- maxfuel fuel)))
    (setq i 0)
    ;set equipment stock correspondin current planet
    (while (< i number-of-equip-items)
      (setq equipment (aref equipment-stock i))
      (setf (equipitem-canbuy equipment) (and (elite-equip-present (equipitem-type equipment)) (<= (equipitem-price equipment) elite-player-cash)))
      (setf (equipitem-show equipment) (>= tech_level (equipitem-techlevel equipment)))
      (setq i (1+ i))
      )
    ;create equipment list
    (setq i 0)
    ;set equipment stock correspondin current planet
    (while (< i number-of-equip-items)
      (setq equipment (aref equipment-stock i))
      (if (equipitem-show equipment)
	  (progn
;	    (setq local-equipment (concat local-equipment (equipitem-type equipment) " ") )
	    (if (equipitem-canbuy equipment)
		(setq local-equipment (concat local-equipment "* "))
	      (setq local-equipment (concat local-equipment "- "))
		)
	  (setq local-equipment (concat
				 local-equipment
				 (equipitem-type equipment)
				 "\n"
				 ;(format " %.1f CR\n" (/ (equipitem-price equipment) 10.0))
				 ))
	  ))
      (setq i (1+ i))
      )
    local-equipment
    ))

(defun tweakseed (s)
  (let (
	(temp)
	)
    (setq temp (+ (seedtype-w0 s) (seedtype-w1 s) (seedtype-w2 s))) ;2 byte aritmetic
    (setf (seedtype-w0 s) (seedtype-w1 s))
    (setf (seedtype-w1 s) (seedtype-w2 s))
    (setf (seedtype-w2 s) temp)
    )
  )

;-Generate system info from seed
(defun makesystem (s)
  "Generate system from seed.
   Return plansys structure."
  (let (
	(longnameflag)
	(govtype)
	(economy)
	(techlevel)
	(population)
	(radius)
	(productivity)
	(planet-name)
	(thissys)
	(pair1)
	(pair2)
	(pair3)
	(pair4)
	)
    ;(setq s seed)
    (setq longnameflag (logand (seedtype-w0 s) 64))
  
  ;(insert "x: " (number-to-string  (logand (lsh (seedtype-w1 s) -8) 255)) "\n")
  ;(insert "y: " (number-to-string (logand (lsh (seedtype-w0 s) -8) 255)) "\n")

  ;instantiate thissys
  (setq thissys	
	(make-plansys
	 :x (logand (lsh (seedtype-w1 s) -8) 255);and operation because x and y are lowest two bytes only
	 :y (logand (lsh (seedtype-w0 s) -8) 255)
	 ;other variables are nonsense values, they are changed lates
	 :economy 0
	 :govtype 0
	 :techlevel 0
	 :population 0
	 :productivity 0
	 :radius 0
	 :goatsoupseed 0
	 :name ""
	 ))

  ;set govtype and economy
  ;variable can be used lates in this functions
;   (setq s
; 	;initialize seed for galaxy 1
; 	(make-seedtype
; 	 :w0 base0
; 	 :w1 base1
; 	 :w2 base2
; 	 ))
  (setq govtype (logand (lsh (seedtype-w1 s) -3) 7))
  (setq economy (logand (lsh (seedtype-w0 s) -8) 7))
  (if (<= govtype 1)
      (setq economy (logior economy 2)))
  ;set govtype and economy in thissys' plansys struct
  (setf (plansys-economy thissys) economy)
  (setf (plansys-govtype thissys) govtype)
  ;(insert "Economy: " (number-to-string economy) "\n")
  ;(insert "Govtype: " (number-to-string govtype) "\n")
  
  ;set techlevel
  (setq techlevel (+ (logand (lsh (seedtype-w1 s) -8) 3) (logxor economy 7) ))
  (setq techlevel (+ techlevel (lsh govtype -1) ))
  (if (= (logand govtype 1) 1)
      (setq techlevel (+ techlevel 1)))
  ;set techlevel in thissys' plansys struct
  (setf (plansys-techlevel thissys) techlevel)
  ;(insert "Techlevel: " (number-to-string techlevel) "\n")
  
  ;set population
  (setq population (+ (* 4 techlevel) economy))
  (setq population (+ population govtype 1))
  (setf (plansys-population thissys) population)
  ;(insert "Population: " (number-to-string population) "\n")
  
  ;set productivity
  (setq productivity (* (+ (logxor economy 7) 3) (+ govtype 4) ))
  (setq productivity (* productivity population 8))
  (setf (plansys-productivity thissys) productivity)
  ;(insert "productivity: " (number-to-string productivity) "\n")

  ;set radius
  (setq radius (+ (* 256 (+ (logand (lsh (seedtype-w2 s) -8) 15) 11) ) (plansys-x thissys) ))
  (setf (plansys-radius thissys) radius)
  ;(insert "Radius: " (number-to-string radius) "\n")

  ;set goatsoupseed
  (setf (plansys-goatsoupseed thissys) 
	(make-fastseedtype
	 :a (logand (seedtype-w1 s) #xFF)
	 :b (logand (lsh (seedtype-w1 s) -8)  #xFF)
	 :c (logand (seedtype-w2 s)  #xFF)
	 :d (logand (lsh (seedtype-w2 s) -8)  #xFF)
	 ))

  ;set name
  ;init alphabet pairs
  (setq pair1 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  (setq pair2 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  (setq pair3 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  (setq pair4 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  ;Always four iterations of random number
  (setq planet-name
	(concat 
	 (code-to-char (aref pairs pair1))
	 (code-to-char (aref pairs (1+ pair1)))
	 (code-to-char (aref pairs pair2))
	 (code-to-char (aref pairs (1+ pair2)))
	 (code-to-char (aref pairs pair3))
	 (code-to-char (aref pairs (1+ pair3)))	 
	 ))
  (if (/= longnameflag 0)
      (progn
	(setq planet-name 
	      (concat
	       planet-name
	       (code-to-char (aref pairs pair4))
	       (code-to-char (aref pairs (1+ pair4)))
	       ))
      ))
  (setf (plansys-name thissys) (stripout planet-name "."))
  ;(insert (stripout planet-name ".") "\n")
  ;return plansys
  (copy-plansys thissys)
  ))

(defun stripout (s c)
  "Remove all c's from string s."
  (let (
	(len)
	(count)
	(tmp)
	(name)
	)
  (setq len (length s))
  (setq count 0)
  (setq tmp "")
  (setq name "")
  (while (< count len)
    (setq tmp (code-to-char (aref s count)))
    (if (not (string=  tmp c))
	(setq name (concat name tmp)))
    (setq count (1+ count))
    )
  (substring name 0)
  ))


(defun code-to-char (ascii-code)
  "Helper function. Returns string of ascii code."
  (concat (make-list 1 ascii-code)))

;;Galaxy functions
;Generate galaxy

;Functions for galactic hyperspace

(defun rotate1 (x)
  "rotate 8 bit number leftwards."
  (let(
      (temp)
      )
    (setq temp (logand x 128))
    (+ (* 2 (logand x 127)) (lsh temp -7))
))

(defun twist (x)
  (+ (* 256 (rotate1 (lsh x -8)) (rotate1 (logand x 255)) ))
  )


(defun nextgalaxy (s)
  "Apply to base seed; once for galaxy 2
   twice for galaxy 3, etc.
   Eight application gives galaxy 1 again"
 (setq seed
	(make-seedtype
	 :w0 (twist (seedtype-w0 s))
	 :w1 (twist (seedtype-w1 s))
	 :w2 (twist (seedtype-w2 s))
	 )
	)
;   (setf (seedtype-w0 s) (twist (seedtype-w0 s)))
;   (setf (seedtype-w1 s) (twist (seedtype-w1 s)))
;   (setf (seedtype-w2 s) (twist (seedtype-w2 s)))

  )


;;(buildgalaxy 1)
(defun buildgalaxy (galaxynumber &optional completion-status)
  "Builds galaxy."
  (let (
	(galcount)
	(current-system)
	(syscount)
	(galaxy);;
	)
    (setq galaxy (make-vector galsize []))

    (setq galcount galaxynumber)

    (if (= galcount 1)
	(progn
	  (setq seed
		;initialize seed for galaxy 1
		(make-seedtype
		 :w0 base0
		 :w1 base1
		 :w2 base2
		 )
		)
	  )
      )
     ;(setq galaxynumber 2)
;    (while (< galcount galaxynumber)

    ;hardcoded seeds for galaxies 2-8
    ;seed values extracted from modified Text Elite 1.3 source
    (if (= galcount 2)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 46228
		 :w1 1168
		 :w2 28582
		 )
		)
	  )
      )
    (if (= galcount 3)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 26921
		 :w1 2081
		 :w2 56909
		 )
		)	  
	  )
      )
    (if (= galcount 4)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 53842
		 :w1 4162
		 :w2 48538
		 )
		)	  
	  )
      )
    (if (= galcount 5)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 42404
		 :w1 8324
		 :w2 31541
		 )
		)	  
	  )
      )
    (if (= galcount 6)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 19273
		 :w1 16393
		 :w2 63082
		 )
		)
	  )
      )
    (if (= galcount 7)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 38546
		 :w1 32786
		 :w2 60884
		 )
		)  
	  )
      )
    (if (= galcount 8)
	(progn
	  (setq seed
		(make-seedtype
		 :w0 11557
		 :w1 292
		 :w2 56233
		 )
		)	  
	  )
      )

    ;(nextgalaxy seed)
    ;(setq seed (nextgalaxy seed))
;    (setq galcount (1+ galcount))
;    )

  ;put galaxy data in array of structures
  (setq syscount 0)
  (let (
	(progress)
	(prog-text)
	(percent)
	)
    (setq prog-text "Initializing universe")
    (setq progress 0)
    (while (< syscount galsize)
    ;vector of vectors..
      (setq current-system (makesystem seed))
      (aset galaxy syscount current-system)
      ;(aset elite-planet-completion-list syscount (list (plansys-name current-system) syscount))
      (setq syscount (1+ syscount))

      ;progress
;;       (setq progress (1+ progress))
;;       (if (= (% progress 1) 0)
;; 	  (progn 
;; 	    (setq percent (number-to-string (floor (/ progress  2.56))))
;; 	     ;(setq prog-text (concat prog-text "."))
;; 	    (message (concat prog-text " "  percent "%%" completion-status))
;; 	    )
;; 	  )
      )
    )
    galaxy
  )
  )

(defun elite-set-planet-completion-list ()
	  ;set planet completion list
  (let (
	(syscount)
	)
    (setq syscount 0)
    (while (< syscount galsize)
      (aset elite-planet-completion-list syscount (list (plansys-name (aref galaxy syscount)) syscount))
      (setq syscount (1+ syscount))
      )
    )
  )

(defun genmarket (fluct p);p is plansys struct
 " Prices and availabilities are influenced by the planet's economy type
   (0-7) and a random ""fluctuation"" byte that was kept within the saved
   commander position to keep the market prices constant over gamesaves.
   Availabilities must be saved with the game since the player alters them
   by buying (and selling(?))

   Almost all operations are one byte only and overflow ""errors"" are
   extremely frequent and exploited.

   Trade Item prices are held internally in a single byte=true value/4.
   The decimal point in prices is introduced only when printing them.
   Internally, all prices are integers.
   The player's cash is held in four bytes."
   (let (
	 (lmarket (make-markettype
		  :quantity (make-vector (1+ lasttrade) 0)
		  :price (make-vector (1+ lasttrade) 0)))
	 (i)
	 (q)
	 (product)
	 (changing)
	 (commodity)
	 )
     ;(setq i 3)
     (setq i 0)
; (aset commodities 3 (make-tradegood 
;     :baseprice 40 
;     :gradient -5
;     :basequant 226
;     :maskbyte 31
;     :units 0
;(setq p (aref galaxy currentplanet))
;     :name "Slaves      "))
     ;(setq fluct 184)
     (while (<= i lasttrade)
       ;(setq p (aref galaxy 7))
       (setq commodity (aref commodities i))
       (setq product (* (plansys-economy p) (tradegood-gradient commodity)))
       (setq changing (logand fluct (tradegood-maskbyte commodity)))
       (setq q (+ (tradegood-basequant commodity) changing (- product)))
       (setq q (logand q 255))
       (if (/= (logand q ?\x80) 0)
	   (setq q 0)) ;clip to positive 8-bit
       ;error in q...
       ;get quantity array 
       (aset (markettype-quantity lmarket) i (logand q ?\x3f)) ;mast to 6 bits
       
       (setq q (+ (tradegood-baseprice commodity) changing product))
       (setq q (logand q ?\xff))
       (aset (markettype-price lmarket) i (* q 4))

       (setq i (1+ i))
       )
       (aset (markettype-quantity lmarket) AlienItems 0) ;Override to force nonavailability, change..

       (copy-markettype lmarket)

     ))

(defun elite-systems-in-range ()
  "Returns array of planet numbers that are within range."
  (let
      (
       (systems ())
       (syscount 0)
       (d)
       )
    (while (< syscount galsize)
      (setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
      (if (<= d maxfuel)
	  (if (<= d fuel)
	      (setq systems (append systems (make-list 1 syscount)))
	    )	    
	)
      (setq syscount (1+ syscount))
      )
    (vconcat (car (make-list 1 systems)))
    ;(vconcat systems)
    ))

(defun elite-systems-within-radius (radius &optional current-planet)
  "Returns array of planet numbers that are within radius light years."
  (let
      (
       (systems ())
       (syscount 0)
       (d)
       (current-sys)
       )
    (if current-planet
	(setq current-sys current-planet)
      (setq current-sys currentplanet)
      )
    (while (< syscount galsize)
      (setq d (distance (aref galaxy syscount ) (aref galaxy current-sys)))
      (if (and (> d 0) (<= d radius))
	  (setq systems (append systems (make-list 1 syscount)))
	)
      (setq syscount (1+ syscount))
      )
    (vconcat (car (make-list 1 systems)))
    ;(vconcat systems)
    )
  )


(defun elite-local-systems-completion-list ()
  "Return completion list of systems within range."
    (let (
	(i)
	(system)
	(systems-in-range)
	(completion-vector)	
	)
    (setq systems-in-range  (elite-systems-in-range))
    (setq completion-vector (make-vector (length systems-in-range) []))
    (setq i 0)
    (while (< i (length systems-in-range))
      (setq system (aref galaxy (aref systems-in-range i)))
      (aset completion-vector i 
	    (list 
	     (concat 
	      (plansys-name system) 
	      " TL: " 
	      (number-to-string (1+ (plansys-techlevel system)))
	     " "
	     (aref econnames (plansys-economy system))
	     " "
	     (aref govnames (plansys-govtype system))
	     "              ")
	     i
	     )
	     )
      (setq i (1+ i))
      )
    
    (append completion-vector nil)

))

(defun distance (a b)
  "Seperation between two planets (4*sqrt(X*X+Y*Y/4))."
  (let (
	(ax)
	(bx)
	(ay)
	(by)
	)
    ;;(setq a (aref galaxy syscount ))
    ;;(setq b (aref galaxy current-sys))
    (setq ax (plansys-x a))
    (setq bx (plansys-x b))
    (setq ay (plansys-y a))
    (setq by (plansys-y b))

  (floor (* 4.0 (sqrt (+ (* (- ax bx) (- ax bx)) (lsh (* (- ay by) (- ay by)) -2)  ))	) )
  )
)

(defun matchsys (s)
  "Return index of the planet whose name matches passed string
   closest to currentplanet - if none return currentplanet."
  (let (
	(syscount)
	(d 9999)
	(p currentplanet)
	)
    (setq syscount 0)
    (while (< syscount galsize)
      (if (string= (upcase s) (elite-get-system-name syscount));(plansys-name (aref galaxy syscount)))
	  (if (< (distance (aref galaxy syscount ) (aref galaxy currentplanet)) d)
	    (progn
	      (setq d (distance (aref galaxy syscount ) (aref galaxy currentplanet)))
	      (setq p syscount)
	      ))
	    
	  )
      (setq syscount (1+ syscount))
	)
    ;return p
    (max p)
    ))

(defun elite-get-system-name (system-index)
  "Returns system name of specified index."
;(elite-get-system-name 41)
  (plansys-name (aref galaxy system-index))
)

;;(setq tl1 (list 2 ))
;;(setq tl2 (list 3 ))
;;(elite-list-has-same-element tl1 tl2)
(defun elite-list-has-same-element (list1 list2)
  "return true if list1 and list2 has one or more same elements"
  (let (
	(rval)
	(tmp)
	(tmp-list1)
	(tmp-list2)
	)
    (setq rval nil)
    (setq tmp-list1 (sort (copy-sequence list1) '<))
    (while (and tmp-list1 (not rval))
      (setq tmp (car tmp-list1))
      (setq tmp-list2 (sort (copy-sequence list2) '<))
      (while (and tmp-list2 (not rval))
	(if (= tmp (car tmp-list2))
	    (setq rval t)
	  )
	(setq tmp-list2 (cdr tmp-list2))
	)
      (setq tmp-list1 (cdr tmp-list1))
      )
    rval
    )
  )

(defun elite-is-docked ()
  "Return true if player is docked."
  (string= (commander-condition elite-commander) CONDITION_DOCKED)
)

(defun elite-get-unit (tradegood-index)
  (aref unitnames (tradegood-units (aref commodities tradegood-index)))
  )

(defun elite-cargo ()
  "Return current cargo in tonnes."
  (let (
	(i)
	(cargo 0)
	)
    (setq i 0)
    (while (< i (1+ lasttrade))
      (setq cargo (+ cargo (aref shipshold i)))      
      (setq i (1+ i))
      )
    cargo
    )
  )

(defun elite-illegal-cargo ()
  "Return illegal cargo tonnage"
  (let 
      (
       (illegal-cargo)
       )
    (setq illegal-cargo (aref shipshold 3));slaves
    (setq illegal-cargo (+ illegal-cargo (aref shipshold 6)));narcotics
    (+ illegal-cargo (aref shipshold 10));firearms
    )
  )

(defun elite-gamejump (i)
  "Jump to system i."
  (setq currentplanet i)
  (setq localmarket (genmarket (randbyte) (aref galaxy currentplanet)))
  )

(defun elite-gamefuel (fuel-to-buy)
  "Attempt to buy fuel-to-buy tonnes of fuel."
  (let
      (
       (f fuel-to-buy)
       )
  (if (> (+ f fuel) maxfuel)
      (setq f (- maxfuel fuel)))
  (if (> elite-fuelcost 0)
      (if (> (* f elite-fuelcost) elite-player-cash)
	  (setq f (/ elite-player-cash elite-fuelcost))
	())
    ())
  (setq fuel (+ fuel f))
  (setq elite-player-cash (- elite-player-cash (* elite-fuelcost f)))
  f
  )
)

;stockmarket functions

(defun elite-trade-good-index (tradegood)
  "Returns index of tradegood in tradegood array."
  (let (
	(i)
	(index)
	)
    (setq i 0)
    (while (< i (length tradnames))
      (if (string= tradegood (aref tradnames i))
	  (setq index i)
	()
	)
      (setq i (1+ i))
      )
    (+ index 0)
    )
) 

(defun elite-gamebuy (good amount)
  "Try to buy ammount  of good (good is array index)   Return ammount bought
   Cannot buy more than is availble, can afford, or will fit in hold"
  (let (
	(tonnes-to-buy)
	(quantity)
	(i)
	)
  (if (< elite-player-cash 0)
      (+ 0 0)
    (progn
      (setq quantity (aref (markettype-quantity localmarket) good))
      (setq tonnes-to-buy (mymin quantity amount))
      (if (= ( tradegood-units (aref commodities good)) 0)
	  (setq tonnes-to-buy (mymin holdspace tonnes-to-buy))
	())
      (setq tonnes-to-buy (mymin tonnes-to-buy (floor (/ (float elite-player-cash ) (aref (markettype-price localmarket) good)))))
      ))
  (aset shipshold good (+ (aref shipshold good) tonnes-to-buy))
;  (aset shipshold good tonnes-to-buy)
  (aset (markettype-quantity localmarket) good (- (aref (markettype-quantity localmarket) good) tonnes-to-buy))
  (setq elite-player-cash (- elite-player-cash (* tonnes-to-buy (aref (markettype-price localmarket) good))))
  (if (= ( tradegood-units (aref commodities good)) 0)
      (setq holdspace (- holdspace tonnes-to-buy))
      )
  (+ tonnes-to-buy 0)
  ))

(defun elite-gamesell (good amount)
  "Sell goods."
  (let (
	(to-sell)
	(sold)
	(sell-price)
	)
    (setq to-sell (mymin (aref shipshold good) amount))
    (aset shipshold good (- (aref shipshold good) to-sell))
    (aset (markettype-quantity localmarket) good (+ (aref (markettype-quantity localmarket) good) to-sell))
    (if (= ( tradegood-units (aref commodities good)) 0)
	(setq holdspace (+ holdspace to-sell))
      )
    (setq sell-price (* to-sell (aref (markettype-price localmarket) good)))
    (setq sold (list "discarded when returned" to-sell (aref unitnames ( tradegood-units (aref commodities good))) sell-price) )
    (setq elite-player-cash (+ elite-player-cash sell-price))
    (cdr sold)
   )
)

;general purpose random function

(defvar elite-rand1-seed 0)


(defun elite-rand1 (&optional bytevalue)
 "Portable random number generator implementing the recursion:
      IX = 16807 * IX MOD (2**(31) - 1)
  Using only 32 bits, including sign.
 
  Taken from \"A Guide to Simulation\" by Bratley, Fox and Schrage.

 Function and description taken from elite new kind sources. This uses only 28
 bit because Emacs integers are 28 bits"
 (let (
       (k1)
       (ix)
       )
   (setq ix elite-rand1-seed)
   (setq k1 (/ ix 127773))
   (setq ix (- (* 16807 (- ix (* k1 127773))) (* k1 2836) ))
   (if (< ix 0)
       (setq ix (+ ix ?\x7ffffff)))
   (setq elite-rand1-seed ix)
   (if bytevalue
       (logand ix 255)
     (max ix))
; 	int k1;
; 	int ix = rand_seed;
; 	
; 	k1 = ix / 127773;
; 	ix = 16807 * (ix - k1 * 127773) - k1 * 2836;
; 	if (ix < 0)
; 		ix += 2147483647;
; 	rand_seed = ix;
; 
; 	return ix; 

	
   )
 )

;(setq debug-on-error t)
(provide 'elite-functions)