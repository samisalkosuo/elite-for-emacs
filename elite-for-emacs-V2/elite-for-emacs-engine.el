;;; elite-for-emacs-engine.el - 

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

;; Core functions and variables, genmarket etc

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
	)
  (logand (myrand) 255)

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
	;; (p)
	 )
     ;;(setq p
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

     )
   )

;;(distance a b)
(defun distance (a b)
;;  "Seperation between two planets (4*sqrt(X*X+Y*Y/4))."
  (let (
	(ax)
	(bx)
	(ay)
	(by)
	)
    ;;(setq a (aref galaxy 129 ))
    ;;(setq b (aref galaxy 7))
    (setq ax (plansys-x a))
    (setq bx (plansys-x b))
    (setq ay (plansys-y a))
    (setq by (plansys-y b))

  (floor (* 4.0 (sqrt (+ (* (- ax bx) (- ax bx)) (lsh (* (- ay by) (- ay by)) -2)  ))	) )
  )
)

(defun elite-for-emacs-gamebuy (cmdr good amount)
  "Try to buy ammount  of good (good is array index)   Return ammount bought
   Cannot buy more than is availble, can afford, or will fit in hold"
  (let (
	(tonnes-to-buy)
	(quantity)
	(i)
	(localmarket)
	(item-index)
	(holdspace)
	(shipshold)
	(cash)
	(trade-history-current)
	(tmp)
	)
    (setq cash (elite-for-emacs-commander-credits cmdr))
    (if (< cash 0)
	0 
      (progn
      ;;get first match in trade items      
      (setq item-index  (elite-for-emacs-trade-good-index good))
      (if elite-for-emacs-online
	  (setq localmarket (elite-for-emacs-online-local-market (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) ))
	(setq localmarket (elite-for-emacs-commander-local-market cmdr))
	  )


      (setq quantity (aref (markettype-quantity localmarket) item-index))
      (setq holdspace (elite-for-emacs-commander-max-cargo-capacity cmdr))
      (setq current-holdspace (elite-for-emacs-commander-cargo-capacity cmdr))
      (setq shipshold (elite-for-emacs-commander-current-cargo cmdr))
      (setq tonnes-to-buy (min quantity amount))
      (if (= ( tradegood-units (aref commodities item-index)) 0)
	  (setq tonnes-to-buy (min current-holdspace tonnes-to-buy))
	())
      (setq tonnes-to-buy (min tonnes-to-buy (floor (/ (float cash ) (aref (markettype-price localmarket) item-index)))))

      (if (> tonnes-to-buy 0)
	  (progn
	    (aset shipshold item-index (+ (aref shipshold item-index) tonnes-to-buy))
					;  (aset shipshold item-index tonnes-to-buy)
	    (if elite-for-emacs-online
		(elite-for-emacs-online-update-local-market-buy (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr) item-index tonnes-to-buy)
	      (aset (markettype-quantity localmarket) item-index (- (aref (markettype-quantity localmarket) item-index) tonnes-to-buy))
	      )
	    
	    (setf (elite-for-emacs-commander-credits cmdr) (- cash (* tonnes-to-buy (aref (markettype-price localmarket) item-index))))
	    
	    (if (and (not (or (= item-index 13) (= item-index 14) (= item-index 15))) (member EQUIPMENT_TRADE_HISTORY_V1 (elite-for-emacs-commander-equipment-list cmdr)))
		(progn
		  (setq trade-history-current (elite-for-emacs-commander-trade-history-current cmdr))
;; 		  (if (not trade-history)
;; 		      (setq trade-history (make-list 17 0))
;; 		    )
		  ;;get item index
		  ;;system where bought,galaxy,good,amount,price per amount  and money spent		  
		  (setq tmp (list
			     (elite-for-emacs-commander-current-planet cmdr)
			     (elite-for-emacs-commander-current-galaxy cmdr)
			     item-index
			     tonnes-to-buy
			     (aref (markettype-price localmarket) item-index)
			     (* tonnes-to-buy (aref (markettype-price localmarket) item-index))
			     )
			)

		  (if (not trade-history-current)
		      (progn
			
			(setq trade-history-current (make-vector 17 nil))
		      )
		    )
		  (aset trade-history-current item-index tmp)
		  (setf (elite-for-emacs-commander-trade-history-current cmdr) trade-history-current)

		  )
	      )



	    (if (= ( tradegood-units (aref commodities item-index)) 0);;check if unit is tonnes,
		                                                      ;;if it is decrease free hold space
		(setf (elite-for-emacs-commander-cargo-capacity cmdr) (- current-holdspace tonnes-to-buy))
		;;(setq current-holdspace (- current-holdspace tonnes-to-buy))
	      )
	    )
	)
      tonnes-to-buy
      )
    )
  )
  )


;;planet description
(defvar rnd_seed nil)

(defun elite-for-emacs-planet-description (galaxy-index system-index)
  "Return planet description"
  (let (
	(planet-sys)
	(rnd_seed)
	)
    (setq planet-sys (aref (aref elite-for-emacs-galaxies-in-universe galaxy-index) system-index))

    ;;lave (setq planet-sys (aref (aref elite-for-emacs-galaxies-in-universe 0) 0))
    ;;zaonce (setq planet-sys (aref (aref elite-for-emacs-galaxies-in-universe 0) 129))
    ;;(setq planet-sys '[cl-struct-plansys 20 173 5 3 4 25 7000 4116 [cl-struct-fastseedtype 156 166164 29 34938112] "LAVE"])

    ;;(list #x8f 32 105 115 32 #x97 46) (string-to-char "\x8F is \x97.")
    ;;(split-string "\x8F is \x97." "")
    ;;(setq planet-sys (aref (aref elite-for-emacs-galaxies-in-universe 1) 7))
    (setq rnd_seed (copy-fastseedtype (plansys-goatsoupseed planet-sys)))
    (setq elite-for-emacs-planet-description "")
    (goat_soup "\x8F is \x97." planet-sys)
    elite-for-emacs-planet-description
    )
  )

(defvar elite-for-emacs-rand1-seed 0)

(defun rand1 (&optional bytevalue)
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
   (setq ix elite-for-emacs-rand1-seed)
   (setq k1 (/ ix 127773))
   (setq ix (- (* 16807 (- ix (* k1 127773))) (* k1 2836) ))
   (if (< ix 0)
       (setq ix (+ ix ?\x7ffffff)))
   (setq elite-for-emacs-rand1-seed ix)
   (if bytevalue
       (logand ix 255)
     ix)
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


(defvar elite-for-emacs-planet-description "")

(defun goat_soup (source planet-sys)
  (let (
	(c)
	(rnd)
	(source-list nil)
	(tmp)
	(i)
	(len)
	(x)	
	)
    
    ;;(while t
    ;;source to list
      (setq tmp (split-string source ""))
      ;;(setq tmp (split-string "\x8F is \x97." ""))
      (setq source-list nil)
      (while tmp
	(setq c (car tmp))
	(setq source-list (append source-list (list (string-to-char c))))
	(setq tmp (cdr tmp))
	)
      (while source-list
	(setq c (car source-list))
	;;(setq source-list (cdr source-list))
	    (if (< c #x80)
		(setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (list c)))
	      ;;(insert (concat (list c)))
	      (progn
		(if (<= c #xa4)
		    (progn
		      (setq rnd (gen_rnd_number))
		      (setq tmp 0);;true: non-zero, zer=false
		      (if (>= rnd #x33)
			  (setq tmp (1+ tmp))
			)
		      (if (>= rnd #x66)
			  (setq tmp (1+ tmp))
			)
		      (if (>= rnd #x99)
			  (setq tmp (1+ tmp))
			)
		      (if (>= rnd #xCC)
			  (setq tmp (1+ tmp))
			)
		      ;;(setq source (nth tmp (nth (- c #x81) desc_list)))
		      (goat_soup (nth tmp (nth (- c #x81) desc_list)) planet-sys); .option[()+(rnd >= 0x66)+(rnd >= 0x99)+(rnd >= 0xCC)] planet-sys)
		      ;;goat_soup(desc_list[c-0x81].option[(rnd >= 0x33)+(rnd >= 0x66)+(rnd >= 0x99)+(rnd >= 0xCC)],psy);
		      ;;(goat_soup...
		      )
		  (progn
		    ;;switch...
		    (cond
		     ((= c #xB0);;planet name
		      (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (capitalize (plansys-name planet-sys))));;(insert (capitalize (plansys-name planet-sys)))
		      )
		     ((= c #xB1);; /* <planet name>ian */
		      (setq tmp (capitalize (plansys-name planet-sys)))
		      (if (and (not (string-match "e$" tmp)) (not (string-match "i$" tmp)))
			  (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description tmp));;(insert tmp)
			(progn
			  ;;(setq tmp "helleinooio")
			  (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (substring tmp 0 (1- (length tmp))) "ian" ));;(insert (substring tmp 0 (1- (length tmp))))
			  )
			)
		      )
		     ( (= c #xB2);;/* random name */
		       (setq i 0)
		       (setq len (logand (gen_rnd_number) 3))
		       (while (<= i len)
			 (setq x (logand (gen_rnd_number) #x3e))
			 (if (/= (aref pairs x) 46);;46='.' (string-to-char ".")
			     (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (char-to-string (aref pairs x))));;(insert (char-to-string (aref pairs0 x)))
			     )
			 (if (and (> i 0) (/= (aref pairs (1+ x)) 46))
			     (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (char-to-string (aref pairs (1+ x)))));;(insert (char-to-string (aref pairs0 (1+ x))))			     
			     )
			 (setq i (1+ i))
			 )
;; 		       				case 0xB2: /* random name */
;; 				{	int i;
;; 					int len = gen_rnd_number() & 3;
;; 					for(i=0;i<=len;i++)
;; 					{	int x = gen_rnd_number() & 0x3e;
;; 						if(pairs0[x]!='.') printf("%c",pairs0[x]);
;; 						if(i && (pairs0[x+1]!='.')) printf("%c",pairs0[x+1]);
;; 					}
;; 				}	break;

		       )

		     )

		    )	
		)			
	      )	    
	    )
	    ;;break	    
	    (setq source-list (cdr source-list))
	    )
      )
  )


(defun gen_rnd_number ()
  (let (
	(a)
	(x)
	)        
    (setq x (logand (* (fastseedtype-a rnd_seed) 2) #xFF));
    (setq a (+ x (fastseedtype-c rnd_seed)))
    (if (> (fastseedtype-a rnd_seed) 127)
	(setq a (1+ a))
      )
    (setf (fastseedtype-a rnd_seed) (logand a #xFF))
    (setf (fastseedtype-c rnd_seed) x)
    (setq a (/ a 256));	/* a = any carry left from above */
    (setq x (fastseedtype-b rnd_seed))

    (setq a (logand (+ a x (fastseedtype-d rnd_seed)) #xFF))
    
    (setf (fastseedtype-b rnd_seed) a)
    (setf (fastseedtype-d rnd_seed) x)
    a
    )
  )
;; int gen_rnd_number (void)
;; {	int a,x;
;; 	x = (rnd_seed.a * 2) & 0xFF;
;; 	a = x + rnd_seed.c;
;; 	if (rnd_seed.a > 127)	a++;
;; 	rnd_seed.a = a & 0xFF;
;; 	rnd_seed.c = x;
;; 
;; 	a = a / 256;	/* a = any carry left from above */
;; 	x = rnd_seed.b;
;; 	a = (a + x + rnd_seed.d) & 0xFF;
;; 	rnd_seed.b = a;
;; 	rnd_seed.d = x;
;; 	return a;
;; }



(defun elite-for-emacs-describe-inhabitants ()
  "todo"
  (let (
	(cmdr)
	)

    )
  )
;;todo: my own algorithm for inhabitants
;;humans, romulans, vulcans, klongons, kilrathi, kzins, etc.
;; void describe_inhabitants (char *str, struct galaxy_seed planet)
;; {
;; 	int inhab;
;; 
;; 	strcpy (str, "(");
;; 
;; 	if (planet.e < 128)
;; 	{
;; 		strcat (str, "Human Colonial");
;; 	}
;; 	else
;; 	{
;; 		inhab = (planet.f / 4) & 7;
;; 		if (inhab < 3)
;; 			strcat (str, inhabitant_desc1[inhab]);
;; 
;; 		inhab = planet.f / 32;
;; 		if (inhab < 6)
;; 			strcat (str, inhabitant_desc2[inhab]);
;; 
;; 		inhab = (planet.d ^ planet.b) & 7;
;; 		if (inhab < 6)
;; 			strcat (str, inhabitant_desc3[inhab]);
;; 
;; 		inhab = (inhab + (planet.f & 3)) & 7;
;; 		strcat (str, inhabitant_desc4[inhab]);
;; 	}
;; 
;; 	strcat (str, "s)");
;; }
;; static char *inhabitant_desc1[] = {"Large ", "Fierce ", "Small "};
;; 
;; static char *inhabitant_desc2[] = {"Green ", "Red ", "Yellow ", "Blue ", "Black ", "Harmless "};
;; 
;; static char *inhabitant_desc3[] = {"Slimy ", "Bug-Eyed ", "Horned ", "Bony ", "Fat ", "Furry "};
;; 
;; static char *inhabitant_desc4[] = {"Rodent", "Frog", "Lizard", "Lobster", "Bird", "Humanoid", "Feline", "Insect"};




;;variables
(defconst base0 ?\x5a4a)
(defconst base1 ?\x0248)
(defconst base2 ?\xb753)
(defconst numforLave 7); Lave is 7th generated planet in galaxy one
(defconst numforZaonce 129)
(defconst numforDiso 147)
(defconst numforRied 46)
(defconst AlienItems 16)
(defconst lasttrade AlienItems)
(defconst galsize 256
  "Galaxy size")
(defconst CONDITION_DOCKED "Docked")
(defconst CONDITION_GREEN "Green")
(defconst CONDITION_YELLOW "Yellow")
(defconst CONDITION_RED "Red")

(defconst fuelcost 2
  "0.2 CR/Light year")

(defconst maxfuel 70
  "7.0 LY tank")

(defvar lastrand 12345
  "Default random seed.")

(defconst inhabitant1 (list "Large " "Fierce " "Small "))
(defconst inhabitant2 (list "Green " "Red " "Yellow " "Blue " "Black " "Harmless "))
(defconst inhabitant3 (list "Slimy " "Bug-Eyed " "Horned " "Bony " "Fat " "Furry "))
(defconst inhabitant4 (list "Rodent" "Frog" "Lizard" "Lobster" "Bird" "Humanoid" "Feline" "Insect"))


(defstruct seedtype
  ;six byte random number used as seed for planets
  w0
  w1
  w2
)

(defvar seed
  ;variable seed
  (make-seedtype
   :w0 1
   :w1 2
   :w2 3
   ))

(defstruct fastseedtype
  ;four byte random number used for planet description
  a
  b
  c
  d
)

(defconst pairs0 "ABOUSEITILETSTONLONUTHNO"
  "Characters used in planet names.")
;must continue into ..
(defconst pairs "..LEXEGEZACEBISOUSESARMAINDIREA.ERATENBERALAVETIEDORQUANTEISRION"
  "Characters for planet names.")


(defconst desc_list
  (list
;; 81 */
	(list "fabled" "notable" "well known" "famous" "noted")
;; 82 */
	(list "very" "mildly" "most" "reasonably" "")
;; 83 */
	(list "ancient" "\x95" "great" "vast" "pink")
;; 84 */
	(list "\x9E \x9D plantations" "mountains" "\x9C" "\x94 forests" "oceans")
;; 85 */
	(list "shyness" "silliness" "mating traditions" "loathing of \x86" "love for \x86")
;; 86 */
	(list "food blenders" "tourists" "poetry" "discos" "\x8E")
;; 87 */
	(list "talking tree" "crab" "bat" "lobst" "\xB2")
;; 88 */
	(list "beset" "plagued" "ravaged" "cursed" "scourged")
;; 89 */
	(list "\x96 civil war" "\x9B \x98 \x99s" "a \x9B disease" "\x96 earthquakes" "\x96 solar activity")
;; 8A */
	(list "its \x83 \x84" "the \xB1 \x98 \x99""its inhabitants' \x9A \x85" "\xA1" "its \x8D \x8E")
;; 8B */
	(list "juice" "brandy" "water" "brew" "gargle blasters")
;; 8C */
	(list "\xB2" "\xB1 \x99" "\xB1 \xB2" "\xB1 \x9B" "\x9B \xB2")
;; 8D */
	(list "fabulous" "exotic" "hoopy" "unusual" "exciting")
;; 8E */
	(list "cuisine" "night life" "casinos" "sit coms" " \xA1 ")
;; 8F */
	(list "\xB0" "The planet \xB0" "The world \xB0" "This planet" "This world")
;; 90 */
	(list "n unremarkable" " boring" " dull" " tedious" " revolting")
;; 91 */
	(list "planet" "world" "place" "little planet" "dump")
;; 92 */
	(list "wasp" "moth" "grub" "ant" "\xB2")
;; 93 */
	(list "poet" "arts graduate" "yak" "snail" "slug")
;; 94 */
	(list "tropical" "dense" "rain" "impenetrable" "exuberant")
;; 95 */
	(list "funny" "wierd" "unusual" "strange" "peculiar")
;; 96 */
	(list "frequent" "occasional" "unpredictable" "dreadful" "deadly")
;; 97 */
	(list "\x82 \x81 for \x8A" "\x82 \x81 for \x8A and \x8A" "\x88 by \x89" "\x82 \x81 for \x8A but \x88 by \x89""a\x90 \x91")
;; 98 */
	(list "\x9B" "mountain" "edible" "tree" "spotted")
;; 99 */
	(list "\x9F" "\xA0" "\x87oid" "\x93" "\x92")
;; 9A */
	(list "ancient" "exceptional" "eccentric" "ingrained" "\x95")
;; 9B */
	(list "killer" "deadly" "evil" "lethal" "vicious")
;; 9C */
	(list "parking meters" "dust clouds" "ice bergs" "rock formations" "volcanoes")
;; 9D */
	(list "plant" "tulip" "banana" "corn" "\xB2weed")
;; 9E */
	(list "\xB2" "\xB1 \xB2" "\xB1 \x9B" "inhabitant" "\xB1 \xB2")
;; 9F */
	(list "shrew" "beast" "bison" "snake" "wolf")
;; A0 */
	(list "leopard" "cat" "monkey" "goat" "fish")
;; A1 */
	(list "\x8C \x8B" "\xB1 \x9F \xA2""its \x8D \xA0 \xA2" "\xA3 \xA4" "\x8C \x8B")
;; A2 */
	(list "meat" "cutlet" "steak" "burgers" "soup")
;; A3 */
	(list "ice" "mud" "Zero-G" "vacuum" "\xB1 ultra")
;; A4 */
	(list "hockey" "cricket" "karate" "polo" "tennis")
   )
)
;; B0 = <planet name>
;;	 B1 = <planet name>ian
;;	 B2 = <random name>


  

(defstruct markettype
  quantity
  price
  )

(defstruct plansys
  ;planet structure
  x
  y
  economy
  govtype
  techlevel
  population
  productivity
  radius
  goatsoupseed
  name ;12 characters
)
(defconst govnames (vector "Anarchy" "Feudal" "Multi-gov" "Dictatorship" "Communist" "Confederacy" "Democracy" "Corporate State")
  "Government names.")
(defconst econnames (vector "Rich Ind" "Average Ind" "Poor Ind" "Mainly Ind"  "Mainly Agri" "Rich Agri" "Average Agri" "Poor Agri")
  "Econonmy names.")

(defconst tradnames (vector "Food" "Textiles" "Radioactives" "Slaves" "Liquor/Wines" "Luxuries" "Narcotics" "Computers" "Machinery" "Alloys" "Firearms" "Furs" "Minerals" "Gold" "Platinum" "Gem-Stones" "Alien Items"))


(defconst unitnames (vector "t" "kg" "g")
  "unit types for commodities"
  )



(defstruct equipment 
  techlevel;;tech level of system where available
  price;;price of equipment
  name;;name
  id;;internal id see defconst EQUIPMENT_XXX
)

(defconst EQUIPMENT_LARGE_CARGO_BAY 0)
(defconst EQUIPMENT_GALACTIC_HYPERDRIVE 1)
(defconst EQUIPMENT_TRADE_HISTORY_V1 2);;version 1 displays latest items only
(defconst EQUIPMENT_PRECIOUS_METALS_TRADE_HISTORY_V1 3);;history for gold, platinum
(defconst EQUIPMENT_GEM_STONES_TRADE_HISTORY_V1 4);;history for gemstones
;;trade history:
;;  food total tonnes delivered (bought and sold), total amount of money spend in buyin
;;  total amount of money earned when selling
;;  display: avg price per tonne bought and sold, total profit, average earning per tonne
;;
;;  inventory: show system where bought,good,amount,price per amount  and money spent + prediction if selling in current
;;  system, price+profit
;(defvar elite-for-emacs-equipment-list
(setq elite-for-emacs-equipment-list
  (list
   (make-equipment
    :techlevel 1
    :price 4000
    :name "Large Cargo Bay"
    :id EQUIPMENT_LARGE_CARGO_BAY
    )
   (make-equipment
    :techlevel 10
    :price 50000
    :name "Galactic Hyperdrive"
    :id EQUIPMENT_GALACTIC_HYPERDRIVE
    )
    (make-equipment
     :techlevel 0
     :price 100
     :name "Trade history software v1 (excludes Gold, Platinum and Gem-Stones)"
     :id EQUIPMENT_TRADE_HISTORY_V1
     )
;;     (make-equipment
;;      :techlevel 0
;;      :price 100
;;      :name "Trade history software for precious metals v1 (Gold and Platinum)"
;;      :id EQUIPMENT_PRECIOUS_METALS_TRADE_HISTORY_V1
;;      )
;;     (make-equipment
;;      :techlevel 0
;;      :price 100
;;      :name "Trade history software for gem stones v1"
;;      :id EQUIPMENT_GEM_STONES_TRADE_HISTORY_V1
;;      )
   )
  )

(defstruct tradegood
  ;define tradegood struct
  baseprice
  gradient
  basequant
  maskbyte
  units
  name)

(defconst TRADEGOOD_FOOD 0)
(defconst TRADEGOOD_TEXTILES 1)
(defconst TRADEGOOD_RADIOACTIVES 2)
(defconst TRADEGOOD_SLAVES 3)
(defconst TRADEGOOD_LIQUOR 4)
(defconst TRADEGOOD_LUXURIES 5)
(defconst TRADEGOOD_NARCOTICS 6)
(defconst TRADEGOOD_COMPUTERS 7)
(defconst TRADEGOOD_MACHINERY 8)
(defconst TRADEGOOD_ALLOYS 9)
(defconst TRADEGOOD_FIREARMS 10)
(defconst TRADEGOOD_FURS 11)
(defconst TRADEGOOD_MINERALS 12)
(defconst TRADEGOOD_GOLD 13)
(defconst TRADEGOOD_PLATINUM 14)
(defconst TRADEGOOD_GEMS 15)
(defconst TRADEGOOD_ALIENITEMS 16)

(defvar commodities
  ;array of tradegoods
  (vector
   (make-tradegood 
    :baseprice 19
    :gradient -2
    :basequant 6
    :maskbyte 1
    :units 0
    :name "Food        ")
   (make-tradegood 
    :baseprice 20
    :gradient -1
    :basequant 10
    :maskbyte 3
    :units 0
    :name "Textiles    ")
   (make-tradegood 
    :baseprice 65
    :gradient -3
    :basequant 2
    :maskbyte 7
    :units 0
    :name "Radioactives")
   (make-tradegood 
    :baseprice 40 
    :gradient -5
    :basequant 226
    :maskbyte 31
    :units 0
    :name "Slaves      ")
   (make-tradegood 
    :baseprice 83
    :gradient -5
    :basequant 251
    :maskbyte 15
    :units 0
    :name "Liquor/Wines")
   (make-tradegood 
    :baseprice 196
    :gradient 8
    :basequant 54
    :maskbyte 3
    :units 0
    :name "Luxuries    ")
   (make-tradegood 
    :baseprice 235
    :gradient 29
    :basequant 8
    :maskbyte 120
    :units 0
    :name "Narcotics   ")
   (make-tradegood 
    :baseprice 154 
    :gradient 14
    :basequant 56
    :maskbyte 3
    :units 0
    :name "Computers   ")
   (make-tradegood 
    :baseprice 117
    :gradient 6
    :basequant 40
    :maskbyte 7
    :units 0
    :name "Machinery   ")
   (make-tradegood 
    :baseprice 78
    :gradient 1
    :basequant 17
    :maskbyte 31
    :units 0
    :name "Alloys      ")
   (make-tradegood 
    :baseprice 124
    :gradient 13
    :basequant 29
    :maskbyte 7
    :units 0
    :name "Firearms    ")
   (make-tradegood 
    :baseprice 176
    :gradient -9
    :basequant 220
    :maskbyte 63
    :units 0
    :name "Furs        ")
   (make-tradegood 
    :baseprice 32
    :gradient -1
    :basequant 53
    :maskbyte 3
    :units 0
    :name "Minerals    ")
   (make-tradegood 
    :baseprice 97
    :gradient -1
    :basequant 66
    :maskbyte 7
    :units 1
    :name "Gold        ")
   (make-tradegood 
    :baseprice 171
    :gradient -2 
    :basequant 55 
    :maskbyte 31
    :units 1
    :name "Platinum    ")
   (make-tradegood 
    :baseprice 45
    :gradient -1
    :basequant 250
    :maskbyte 15
    :units 2
    :name "Gem-Stones ")
   (make-tradegood 
    :baseprice 53
    :gradient 15
    :basequant 192
    :maskbyte 7
    :units 0
    :name "Alien Items ")

   )
)
   
;;build galaxy functions
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

(defun elite-for-emacs-build-universe ()
  (let (
	(i)
	(universe)
	)
    (setq universe (make-vector 8 nil))
    (setq i 0)
    (while (< i 8)
      
      (aset universe i (buildgalaxy (1+ i)))
      (setq i (1+ i))
      )
    universe
    )
)

(defvar elite-for-emacs-galaxies-in-universe '[[[cl-struct-plansys 2 90 2 1 8 36 11520 4610 [cl-struct-fastseedtype 72 2 83 183] "TIBEDIED"] [cl-struct-plansys 152 205 5 7 6 37 16280 5528 [cl-struct-fastseedtype 184 152 29 122] "QUBE"] [cl-struct-plansys 77 243 3 3 7 35 13720 3149 [cl-struct-fastseedtype 156 77 27 33] "LELEER"] [cl-struct-plansys 83 208 0 2 11 47 22560 4435 [cl-struct-fastseedtype 148 83 13 134] "BIARGE"] [cl-struct-plansys 180 131 3 4 6 32 14336 3508 [cl-struct-fastseedtype 32 180 51 226] "XEQUERIN"] [cl-struct-plansys 172 176 0 4 9 41 26240 4780 [cl-struct-fastseedtype 224 172 141 119] "TIRAOR"] [cl-struct-plansys 69 249 1 2 8 36 15552 3909 [cl-struct-fastseedtype 212 69 27 20] "RABEDIRA"] [cl-struct-plansys 20 173 5 3 4 25 7000 4116 [cl-struct-fastseedtype 156 20 29 21] "LAVE"] [cl-struct-plansys 236 0 0 7 11 52 45760 4076 [cl-struct-fastseedtype 184 236 83 196] "ZAATXE"] [cl-struct-plansys 216 98 2 1 6 28 8960 6360 [cl-struct-fastseedtype 200 216 61 237] "DIUSREZA"] [cl-struct-plansys 4 238 6 1 2 16 2560 5892 [cl-struct-fastseedtype 204 4 91 28] "TEAATIS"] [cl-struct-plansys 93 49 1 4 9 42 24192 6493 [cl-struct-fastseedtype 100 93 109 158] "RIINUS"] [cl-struct-plansys 244 40 0 2 8 35 16800 5364 [cl-struct-fastseedtype 16 244 179 73] "ESBIZA"] [cl-struct-plansys 84 164 4 6 6 35 16800 6740 [cl-struct-fastseedtype 112 84 45 95] "ONTIMAXE"] [cl-struct-plansys 194 11 3 0 6 28 6272 4290 [cl-struct-fastseedtype 132 194 219 37] "CEBETELA"] [cl-struct-plansys 245 220 4 5 7 38 16416 4341 [cl-struct-fastseedtype 236 245 253 197] "CEEDRA"] [cl-struct-plansys 178 84 4 5 8 42 18144 6578 [cl-struct-fastseedtype 40 178 83 158] "RIZALA"] [cl-struct-plansys 55 245 5 3 7 37 10360 3127 [cl-struct-fastseedtype 216 55 93 209] "ATRISO"] [cl-struct-plansys 214 7 7 7 6 39 10296 6102 [cl-struct-fastseedtype 252 214 155 220] "TEANREBI"] [cl-struct-plansys 6 110 6 6 6 37 11840 6662 [cl-struct-fastseedtype 52 6 205 47] "AZAQU"] [cl-struct-plansys 175 218 2 0 8 35 8960 6575 [cl-struct-fastseedtype 0 175 51 46] "RETILA"] [cl-struct-plansys 123 149 7 0 3 20 1920 4731 [cl-struct-fastseedtype 0 123 205 199] "SOTIQU"] [cl-struct-plansys 186 26 2 6 10 49 31360 6074 [cl-struct-fastseedtype 52 186 155 172] "INLEUS"] [cl-struct-plansys 22 232 0 7 13 60 52800 6678 [cl-struct-fastseedtype 60 22 221 127] "ONRIRA"] [cl-struct-plansys 18 20 4 3 7 36 12096 4114 [cl-struct-fastseedtype 152 18 83 165] "CEINZALA"] [cl-struct-plansys 245 132 4 5 7 38 16416 4597 [cl-struct-fastseedtype 232 245 125 70] "BIISZA"] [cl-struct-plansys 4 253 5 5 5 31 11160 3076 [cl-struct-fastseedtype 44 4 219 193] "LEGEES"] [cl-struct-plansys 14 137 3 0 6 28 6272 5390 [cl-struct-fastseedtype 4 14 45 90] "QUATOR"] [cl-struct-plansys 164 89 1 6 9 44 31680 6820 [cl-struct-fastseedtype 240 164 179 239] "AREXE"] [cl-struct-plansys 96 130 2 2 6 29 11136 3168 [cl-struct-fastseedtype 144 96 109 209] "ATRABIIN"] [cl-struct-plansys 108 230 6 4 3 23 5888 4972 [cl-struct-fastseedtype 228 108 91 8] "USANAT"] [cl-struct-plansys 53 209 3 1 6 29 8120 3381 [cl-struct-fastseedtype 140 53 189 98] "XEESLE"] [cl-struct-plansys 206 1 3 1 7 33 9240 5326 [cl-struct-fastseedtype 8 206 83 57] "ORESEREN"] [cl-struct-plansys 82 16 0 7 13 60 52800 5970 [cl-struct-fastseedtype 248 82 157 108] "INERA"] [cl-struct-plansys 204 143 7 3 2 19 3192 6092 [cl-struct-fastseedtype 92 204 27 44] "INUS"] [cl-struct-plansys 52 128 0 2 8 35 16800 6196 [cl-struct-fastseedtype 212 52 141 61] "ISENCE"] [cl-struct-plansys 149 101 5 4 5 30 9600 6549 [cl-struct-fastseedtype 224 149 51 238] "REESDICE"] [cl-struct-plansys 69 109 5 4 5 30 9600 5957 [cl-struct-fastseedtype 32 69 13 156] "TEREA"] [cl-struct-plansys 26 47 7 2 3 22 3168 5146 [cl-struct-fastseedtype 148 26 27 153] "ORGETIBE"] [cl-struct-plansys 19 151 7 3 5 31 5208 6419 [cl-struct-fastseedtype 220 19 157 142] "REORTE"] [cl-struct-plansys 164 220 4 7 7 40 21120 5540 [cl-struct-fastseedtype 120 164 83 186] "QUQUOR"] [cl-struct-plansys 143 153 3 1 8 37 10360 3727 [cl-struct-fastseedtype 8 143 189 99] "GEINONA"] [cl-struct-plansys 111 127 7 1 4 25 3000 5743 [cl-struct-fastseedtype 140 111 91 123] "ANARLAQU"] [cl-struct-plansys 58 85 5 4 6 34 10880 5178 [cl-struct-fastseedtype 164 58 237 249] "ORESRI"] [cl-struct-plansys 65 190 6 2 3 21 4032 5185 [cl-struct-fastseedtype 208 65 179 137] "ESESLA"] [cl-struct-plansys 104 85 5 6 5 32 12800 4712 [cl-struct-fastseedtype 176 104 173 71] "SOCELAGE"] [cl-struct-plansys 3 181 7 0 3 20 1920 6403 [cl-struct-fastseedtype 68 3 219 190] "RIEDQUAT"] [cl-struct-plansys 113 58 2 5 9 44 25344 3697 [cl-struct-fastseedtype 44 113 125 35] "GEREGE"] [cl-struct-plansys 85 99 3 5 8 41 20664 4949 [cl-struct-fastseedtype 232 85 83 136] "USLE"] [cl-struct-plansys 234 32 0 3 11 48 26880 5866 [cl-struct-fastseedtype 24 234 221 75] "MALAMA"] [cl-struct-plansys 45 139 3 7 9 47 28952 6701 [cl-struct-fastseedtype 188 45 155 15] "AESBION"] [cl-struct-plansys 223 6 6 6 7 41 13120 6879 [cl-struct-fastseedtype 116 223 77 175] "ALAZA"] [cl-struct-plansys 104 36 6 0 1 11 1408 3432 [cl-struct-fastseedtype 192 104 51 34] "XEAQU"] [cl-struct-plansys 11 58 2 0 8 35 8960 3851 [cl-struct-fastseedtype 64 11 77 244] "RAOROR"] [cl-struct-plansys 102 57 1 6 11 52 37440 5222 [cl-struct-fastseedtype 244 102 155 217] "ORORQU"] [cl-struct-plansys 13 186 2 7 10 50 35200 3085 [cl-struct-fastseedtype 124 13 93 65] "LEESTI"] [cl-struct-plansys 162 87 7 3 4 27 4536 3746 [cl-struct-fastseedtype 88 162 83 3] "GEISGEZA"] [cl-struct-plansys 164 163 3 5 7 37 18648 4004 [cl-struct-fastseedtype 40 164 253 68] "ZAINLABI"] [cl-struct-plansys 70 117 5 5 7 39 14040 4934 [cl-struct-fastseedtype 236 70 219 72] "USCELA"] [cl-struct-plansys 227 149 7 0 3 20 1920 6371 [cl-struct-fastseedtype 68 227 173 125] "ISVEVE"] [cl-struct-plansys 202 86 6 6 6 37 11840 4810 [cl-struct-fastseedtype 176 202 179 23] "TIORANIN"] [cl-struct-plansys 108 27 3 2 5 26 8736 3180 [cl-struct-fastseedtype 208 108 237 193] "LEARORCE"] [cl-struct-plansys 133 121 1 4 9 42 24192 5253 [cl-struct-fastseedtype 164 133 91 73] "ESUSTI"] [cl-struct-plansys 168 23 7 1 1 13 1560 5032 [cl-struct-fastseedtype 204 168 61 8] "USUSOR"] [cl-struct-plansys 73 121 3 1 6 29 8120 5705 [cl-struct-fastseedtype 200 73 83 139] "MAREGEIS"] [cl-struct-plansys 253 35 3 7 9 47 28952 6909 [cl-struct-fastseedtype 56 253 29 111] "AATE"] [cl-struct-plansys 251 252 4 3 8 40 13440 4859 [cl-struct-fastseedtype 28 251 27 135] "SORI"] [cl-struct-plansys 6 0 0 2 10 43 20640 4102 [cl-struct-fastseedtype 20 6 13 133] "CEMAVE"] [cl-struct-plansys 39 22 6 4 6 35 8960 5415 [cl-struct-fastseedtype 160 39 51 202] "ARUSQUDI"] [cl-struct-plansys 205 250 2 4 8 39 19968 3021 [cl-struct-fastseedtype 96 205 141 208] "EREDVE"] [cl-struct-plansys 159 54 6 2 5 29 5568 6559 [cl-struct-fastseedtype 84 159 27 110] "REGEATGE"] [cl-struct-plansys 3 81 1 3 11 49 24696 4867 [cl-struct-fastseedtype 28 3 29 152] "EDINSO"] [cl-struct-plansys 12 135 7 7 4 31 8184 2828 [cl-struct-fastseedtype 56 12 83 128] "RA"] [cl-struct-plansys 53 160 2 1 7 32 10240 5429 [cl-struct-fastseedtype 72 53 61 234] "ARONAR"] [cl-struct-plansys 138 223 7 1 3 21 2520 5514 [cl-struct-fastseedtype 76 138 91 42] "ARRAESSO"] [cl-struct-plansys 7 73 1 4 11 50 28800 4103 [cl-struct-fastseedtype 228 7 109 229] "CEVEGE"] [cl-struct-plansys 63 35 3 2 8 38 12768 5183 [cl-struct-fastseedtype 144 63 179 153] "ORTEVE"] [cl-struct-plansys 108 214 6 6 4 29 9280 2924 [cl-struct-fastseedtype 240 108 45 64] "GEERRA"] [cl-struct-plansys 244 48 2 0 5 23 5888 4852 [cl-struct-fastseedtype 4 244 219 167] "SOINUSTE"] [cl-struct-plansys 220 104 0 5 10 46 33120 3036 [cl-struct-fastseedtype 108 220 253 16] "ERLAGE"] [cl-struct-plansys 169 67 3 5 8 41 20664 3497 [cl-struct-fastseedtype 168 169 83 66] "XEAAN"] [cl-struct-plansys 140 27 3 3 6 31 12152 4492 [cl-struct-fastseedtype 88 140 93 214] "VEIS"] [cl-struct-plansys 52 224 0 7 11 52 45760 3380 [cl-struct-fastseedtype 124 52 155 146] "ENSOREUS"] [cl-struct-plansys 168 110 6 6 4 29 9280 6568 [cl-struct-fastseedtype 180 168 205 190] "RIVEIS"] [cl-struct-plansys 210 61 7 0 2 16 1536 4562 [cl-struct-fastseedtype 128 210 51 230] "BIVEA"] [cl-struct-plansys 139 175 7 0 3 20 1920 2955 [cl-struct-fastseedtype 128 139 205 48] "ERMASO"] [cl-struct-plansys 195 39 7 6 6 38 9120 4547 [cl-struct-fastseedtype 180 195 155 86] "VELETE"] [cl-struct-plansys 244 92 4 7 7 40 21120 3572 [cl-struct-fastseedtype 188 244 221 146] "ENGEMA"] [cl-struct-plansys 226 107 3 3 8 39 15288 3298 [cl-struct-fastseedtype 24 226 83 49] "ATRIENXE"] [cl-struct-plansys 66 146 2 5 10 48 27648 3650 [cl-struct-fastseedtype 104 66 125 83] "BEUSRIOR"] [cl-struct-plansys 57 190 6 5 5 32 9216 6713 [cl-struct-fastseedtype 172 57 219 31] "ONTIAT"] [cl-struct-plansys 168 113 3 0 4 20 4480 3240 [cl-struct-fastseedtype 132 168 45 49] "ATARZA"] [cl-struct-plansys 160 36 4 6 6 35 16800 6816 [cl-struct-fastseedtype 112 160 179 15] "ARAZAES"] [cl-struct-plansys 105 132 4 2 5 27 7776 3433 [cl-struct-fastseedtype 16 105 109 194] "XEERANRE"] [cl-struct-plansys 78 219 3 4 8 40 17920 5454 [cl-struct-fastseedtype 100 78 91 218] "QUZADI"] [cl-struct-plansys 12 45 7 1 1 13 1560 6156 [cl-struct-fastseedtype 12 12 189 61] "ISTI"] [cl-struct-plansys 117 192 2 1 7 32 10240 6261 [cl-struct-fastseedtype 136 117 83 173] "DIGEBITI"] [cl-struct-plansys 151 6 6 7 8 46 16192 3223 [cl-struct-fastseedtype 120 151 157 129] "LEONED"] [cl-struct-plansys 217 56 0 3 10 44 24640 3545 [cl-struct-fastseedtype 220 217 27 50] "ENZAER"] [cl-struct-plansys 199 80 0 2 11 47 22560 6087 [cl-struct-fastseedtype 84 199 141 92] "TERAED"] [cl-struct-plansys 105 152 0 4 10 45 28800 4457 [cl-struct-fastseedtype 96 105 51 118] "VETITICE"] [cl-struct-plansys 69 87 7 4 3 24 4608 4165 [cl-struct-fastseedtype 160 69 13 21] "LAENIN"] [cl-struct-plansys 212 12 4 2 4 23 6624 3796 [cl-struct-fastseedtype 20 212 27 147] "BERAANXE"] [cl-struct-plansys 226 219 3 3 8 39 15288 3298 [cl-struct-fastseedtype 92 226 157 49] "ATAGE"] [cl-struct-plansys 35 3 3 7 11 55 33880 4387 [cl-struct-fastseedtype 248 35 83 22] "VEISTI"] [cl-struct-plansys 203 119 7 1 4 25 3000 3019 [cl-struct-fastseedtype 136 203 189 128] "ZAERLA"] [cl-struct-plansys 85 16 2 1 7 32 10240 5205 [cl-struct-fastseedtype 12 85 91 41] "ESREDICE"] [cl-struct-plansys 197 13 5 4 5 30 9600 3013 [cl-struct-fastseedtype 36 197 237 96] "BEOR"] [cl-struct-plansys 237 89 1 2 8 36 15552 5357 [cl-struct-fastseedtype 80 237 179 121] "ORSO"] [cl-struct-plansys 97 39 7 6 4 30 7200 4961 [cl-struct-fastseedtype 48 97 173 72] "USATQURA"] [cl-struct-plansys 148 122 2 0 5 23 5888 2964 [cl-struct-fastseedtype 196 148 219 224] "ERBITI"] [cl-struct-plansys 55 102 6 5 7 40 11520 6455 [cl-struct-fastseedtype 172 55 125 142] "REINEN"] [cl-struct-plansys 173 242 2 5 9 44 25344 6061 [cl-struct-fastseedtype 104 173 83 204] "ININBI"] [cl-struct-plansys 30 230 6 3 5 30 6720 2846 [cl-struct-fastseedtype 152 30 221 112] "ERLAZA"] [cl-struct-plansys 235 4 4 7 10 52 27456 4331 [cl-struct-fastseedtype 60 235 155 101] "CELABILE"] [cl-struct-plansys 97 166 6 6 5 33 10560 6497 [cl-struct-fastseedtype 244 97 77 94] "RIBISO"] [cl-struct-plansys 236 39 7 0 0 8 768 5612 [cl-struct-fastseedtype 64 236 51 122] "QUDIRA"] [cl-struct-plansys 251 244 6 0 4 23 2944 6395 [cl-struct-fastseedtype 192 251 77 125] "ISDIBI"] [cl-struct-plansys 208 230 6 6 4 29 9280 3792 [cl-struct-fastseedtype 116 208 155 35] "GEQURE"] [cl-struct-plansys 203 206 6 7 8 46 16192 4043 [cl-struct-fastseedtype 252 203 93 116] "RARERE"] [cl-struct-plansys 209 78 6 3 4 26 5824 6865 [cl-struct-fastseedtype 216 209 83 47] "AERATER"] [cl-struct-plansys 208 81 1 5 9 43 27864 3280 [cl-struct-fastseedtype 168 208 253 113] "ATBEVETE"] [cl-struct-plansys 220 214 6 5 4 28 8064 4572 [cl-struct-fastseedtype 108 220 219 70] "BIORIS"] [cl-struct-plansys 93 29 7 0 1 12 1152 3933 [cl-struct-fastseedtype 196 93 173 116] "RAALE"] [cl-struct-plansys 38 193 1 6 11 52 37440 4646 [cl-struct-fastseedtype 48 38 179 215] "TIONISLA"] [cl-struct-plansys 85 189 5 2 4 24 5760 3413 [cl-struct-fastseedtype 80 85 237 210] "ENCERESO"] [cl-struct-plansys 199 14 6 4 6 35 8960 5831 [cl-struct-fastseedtype 36 199 91 187] "ANERBE"] [cl-struct-plansys 95 19 3 1 8 37 10360 3679 [cl-struct-fastseedtype 76 95 61 3] "GELAED"] [cl-struct-plansys 81 216 2 1 7 32 10240 6737 [cl-struct-fastseedtype 72 81 83 159] "ONUSORLE"] [cl-struct-plansys 33 185 1 7 11 53 41976 3873 [cl-struct-fastseedtype 184 33 29 164] "ZAONCE"] [cl-struct-plansys 104 69 5 3 4 25 7000 6248 [cl-struct-fastseedtype 156 104 27 45] "DIQUER"] [cl-struct-plansys 120 112 0 2 8 35 16800 3960 [cl-struct-fastseedtype 148 120 13 196] "ZADIES"] [cl-struct-plansys 91 233 1 4 11 50 28800 3419 [cl-struct-fastseedtype 32 91 51 242] "ENTIZADI"] [cl-struct-plansys 173 132 4 4 6 33 12672 5293 [cl-struct-fastseedtype 224 173 141 105] "ESANBE"] [cl-struct-plansys 184 179 3 2 5 26 8736 5048 [cl-struct-fastseedtype 212 184 27 8] "USRALAAT"] [cl-struct-plansys 177 53 5 3 5 29 8120 5809 [cl-struct-fastseedtype 156 177 29 91] "ANLERE"] [cl-struct-plansys 235 78 6 7 8 46 16192 6123 [cl-struct-fastseedtype 184 235 83 124] "TEVERI"] [cl-struct-plansys 81 30 6 1 3 20 3200 4689 [cl-struct-fastseedtype 200 81 61 39] "SOTIERA"] [cl-struct-plansys 207 16 2 1 9 40 12800 5071 [cl-struct-fastseedtype 204 207 91 120] "EDEDLEEN"] [cl-struct-plansys 114 161 1 4 10 46 26496 6002 [cl-struct-fastseedtype 100 114 109 108] "INONRI"] [cl-struct-plansys 75 94 6 2 5 29 5568 5195 [cl-struct-fastseedtype 16 75 179 41] "ESBEUS"] [cl-struct-plansys 69 72 0 6 11 51 40800 3141 [cl-struct-fastseedtype 112 69 45 97] "LERELACE"] [cl-struct-plansys 229 149 7 0 1 12 1152 5349 [cl-struct-fastseedtype 132 229 219 105] "ESZARAXE"] [cl-struct-plansys 130 52 4 5 8 42 18144 5762 [cl-struct-fastseedtype 236 130 253 155] "ANBEEN"] [cl-struct-plansys 97 114 2 5 9 44 25344 4449 [cl-struct-fastseedtype 40 97 83 38] "BIORLE"] [cl-struct-plansys 160 129 1 3 8 37 18648 5792 [cl-struct-fastseedtype 216 160 93 27] "ANISOR"] [cl-struct-plansys 81 249 1 7 11 53 41976 4945 [cl-struct-fastseedtype 252 81 155 136] "USRAREMA"] [cl-struct-plansys 11 174 6 6 7 41 13120 6155 [cl-struct-fastseedtype 52 11 205 141] "DISO"] [cl-struct-plansys 182 224 2 0 7 31 7936 6582 [cl-struct-fastseedtype 0 182 51 222] "RIRAES"] [cl-struct-plansys 92 9 3 0 4 20 4480 5212 [cl-struct-fastseedtype 0 92 205 217] "ORRIRA"] [cl-struct-plansys 141 116 4 6 7 39 18720 2957 [cl-struct-fastseedtype 52 141 155 64] "XEER"] [cl-struct-plansys 147 16 0 7 14 64 56320 4243 [cl-struct-fastseedtype 60 147 221 229] "CEESXE"] [cl-struct-plansys 113 2 2 3 8 38 17024 6257 [cl-struct-fastseedtype 152 113 83 253] "ISATRE"] [cl-struct-plansys 78 224 0 5 12 54 38880 2894 [cl-struct-fastseedtype 232 78 125 160] "AONA"] [cl-struct-plansys 47 191 7 5 6 37 7992 6191 [cl-struct-fastseedtype 44 47 219 189] "ISINOR"] [cl-struct-plansys 3 153 3 0 7 32 7168 4867 [cl-struct-fastseedtype 4 3 45 72] "USZAA"] [cl-struct-plansys 91 47 7 6 6 38 9120 6747 [cl-struct-fastseedtype 240 91 179 111] "AANBIAT"] [cl-struct-plansys 49 198 6 2 3 21 4032 3633 [cl-struct-fastseedtype 144 49 109 243] "BEMAERA"] [cl-struct-plansys 239 16 0 4 12 53 33920 6127 [cl-struct-fastseedtype 228 239 91 236] "ININES"] [cl-struct-plansys 162 201 3 1 7 33 9240 5026 [cl-struct-fastseedtype 140 162 189 88] "EDZAON"] [cl-struct-plansys 221 191 7 1 2 17 2040 3293 [cl-struct-fastseedtype 8 221 83 97] "LERITEAN"] [cl-struct-plansys 155 60 4 7 10 52 27456 4507 [cl-struct-fastseedtype 248 155 157 214] "VEALE"] [cl-struct-plansys 167 33 1 3 11 49 24696 5031 [cl-struct-fastseedtype 92 167 27 120] "EDLE"] [cl-struct-plansys 25 96 0 2 9 39 18720 5657 [cl-struct-fastseedtype 212 25 141 187] "ANLAMA"] [cl-struct-plansys 252 11 3 4 6 32 14336 6652 [cl-struct-fastseedtype 224 252 51 62] "RIBILEBI"] [cl-struct-plansys 6 129 1 4 10 46 26496 6406 [cl-struct-fastseedtype 32 6 13 206] "RELAES"] [cl-struct-plansys 77 41 1 2 8 36 15552 6221 [cl-struct-fastseedtype 148 77 27 205] "DIZAONER"] [cl-struct-plansys 112 95 7 3 2 19 3192 3952 [cl-struct-fastseedtype 220 112 157 20] "RAZAAR"] [cl-struct-plansys 99 106 2 7 12 58 40832 3427 [cl-struct-fastseedtype 120 99 83 178] "ENONLA"] [cl-struct-plansys 200 149 7 1 1 13 1560 6344 [cl-struct-fastseedtype 8 200 189 221] "ISANLEQU"] [cl-struct-plansys 250 225 3 1 7 33 9240 4858 [cl-struct-fastseedtype 140 250 91 23] "TIBECEA"] [cl-struct-plansys 15 5 5 4 7 38 12160 4623 [cl-struct-fastseedtype 164 15 237 7] "SOTERA"] [cl-struct-plansys 88 52 4 2 4 23 6624 5208 [cl-struct-fastseedtype 208 88 179 169] "ESVEOR"] [cl-struct-plansys 25 57 1 6 10 48 34560 5145 [cl-struct-fastseedtype 176 25 173 137] "ESTEONBI"] [cl-struct-plansys 230 127 7 0 2 16 1536 3558 [cl-struct-fastseedtype 68 230 219 66] "XEESENRI"] [cl-struct-plansys 190 210 2 5 10 48 27648 5310 [cl-struct-fastseedtype 44 190 125 57] "ORESLE"] [cl-struct-plansys 196 193 1 5 9 43 27864 3012 [cl-struct-fastseedtype 232 196 83 80] "ERVEIN"] [cl-struct-plansys 19 236 4 3 8 40 13440 4115 [cl-struct-fastseedtype 24 19 221 213] "LARAIS"] [cl-struct-plansys 104 189 5 7 6 37 16280 5736 [cl-struct-fastseedtype 188 104 155 251] "ANXEBIZA"] [cl-struct-plansys 164 134 6 6 4 29 9280 6308 [cl-struct-fastseedtype 116 164 77 77] "DIEDAR"] [cl-struct-plansys 47 106 2 0 8 35 8960 3375 [cl-struct-fastseedtype 192 47 51 18] "ENINRE"] [cl-struct-plansys 172 238 6 0 1 11 1408 4524 [cl-struct-fastseedtype 64 172 77 70] "BIBE"] [cl-struct-plansys 249 211 3 6 8 42 23520 6393 [cl-struct-fastseedtype 244 249 155 173] "DIQUXE"] [cl-struct-plansys 74 34 2 7 11 54 38016 4682 [cl-struct-fastseedtype 124 74 93 231] "SORACE"] [cl-struct-plansys 193 133 5 3 5 29 8120 5825 [cl-struct-fastseedtype 88 193 83 155] "ANXEONIS"] [cl-struct-plansys 189 63 7 5 4 29 6264 6589 [cl-struct-fastseedtype 40 189 253 222] "RIANTIAT"] [cl-struct-plansys 49 119 7 5 4 29 6264 3889 [cl-struct-fastseedtype 236 49 219 132] "ZARECE"] [cl-struct-plansys 152 229 7 0 0 8 768 5784 [cl-struct-fastseedtype 68 152 173 171] "MAESIN"] [cl-struct-plansys 65 108 4 6 7 39 18720 4673 [cl-struct-fastseedtype 176 65 179 215] "TIBIONIS"] [cl-struct-plansys 253 159 7 2 2 18 2592 3837 [cl-struct-fastseedtype 208 253 237 35] "GELEGEUS"] [cl-struct-plansys 200 227 3 4 6 32 14336 6344 [cl-struct-fastseedtype 164 200 91 109] "DIORA"] [cl-struct-plansys 213 79 7 1 2 17 2040 6613 [cl-struct-fastseedtype 204 213 61 62] "RIGETI"] [cl-struct-plansys 24 119 7 1 1 13 1560 3608 [cl-struct-fastseedtype 200 24 83 243] "BEGEABI"] [cl-struct-plansys 6 143 7 7 6 39 10296 5126 [cl-struct-fastseedtype 56 6 29 25] "ORRERE"] [cl-struct-plansys 150 206 6 3 5 30 6720 3734 [cl-struct-fastseedtype 28 150 27 19] "BETI"] [cl-struct-plansys 171 32 0 2 11 47 22560 3755 [cl-struct-fastseedtype 20 171 13 67] "GERETE"] [cl-struct-plansys 78 252 4 4 7 37 14208 5454 [cl-struct-fastseedtype 160 78 51 90] "QUCERERE"] [cl-struct-plansys 78 78 6 4 5 31 7936 3406 [cl-struct-fastseedtype 96 78 141 66] "XEONER"] [cl-struct-plansys 146 112 0 2 10 43 20640 3474 [cl-struct-fastseedtype 84 146 27 226] "XEZAOR"] [cl-struct-plansys 32 89 1 3 8 37 18648 6432 [cl-struct-fastseedtype 28 32 29 94] "RITILA"] [cl-struct-plansys 139 85 5 7 9 49 21560 5003 [cl-struct-fastseedtype 56 139 83 184] "EDORTE"] [cl-struct-plansys 46 220 6 1 4 24 3840 3886 [cl-struct-fastseedtype 72 46 61 164] "ZAALELA"] [cl-struct-plansys 213 129 3 1 6 29 8120 4565 [cl-struct-fastseedtype 76 213 91 6] "BIISORTE"] [cl-struct-plansys 156 57 1 4 8 38 21888 3740 [cl-struct-fastseedtype 228 156 109 51] "BEESOR"] [cl-struct-plansys 22 217 1 2 9 40 17280 5142 [cl-struct-fastseedtype 144 22 179 249] "ORESQU"] [cl-struct-plansys 221 250 2 6 9 45 28800 3549 [cl-struct-fastseedtype 240 221 45 194] "XEQUQUTI"] [cl-struct-plansys 151 58 2 0 8 35 8960 5783 [cl-struct-fastseedtype 4 151 219 107] "MAISES"] [cl-struct-plansys 233 64 0 5 11 50 36000 4585 [cl-struct-fastseedtype 108 233 253 102] "BIERLE"] [cl-struct-plansys 216 225 1 5 9 43 27864 5592 [cl-struct-fastseedtype 168 216 83 74] "ARZASO"] [cl-struct-plansys 117 39 7 3 3 23 3864 2933 [cl-struct-fastseedtype 88 117 93 160] "TEEN"] [cl-struct-plansys 47 82 2 7 12 58 40832 6447 [cl-struct-fastseedtype 124 47 155 190] "RIREDI"] [cl-struct-plansys 45 46 6 6 5 33 10560 5933 [cl-struct-fastseedtype 180 45 205 156] "TEORGE"] [cl-struct-plansys 89 195 3 0 5 24 5376 4441 [cl-struct-fastseedtype 128 89 51 22] "VEBEGE"] [cl-struct-plansys 236 163 3 0 4 20 4480 3564 [cl-struct-fastseedtype 128 236 205 194] "XEENLE"] [cl-struct-plansys 22 1 1 6 11 52 37440 5398 [cl-struct-fastseedtype 180 22 155 106] "ARXEZA"] [cl-struct-plansys 241 4 4 7 8 44 23232 5105 [cl-struct-fastseedtype 188 241 221 120] "EDREOR"] [cl-struct-plansys 193 217 1 3 9 41 20664 5313 [cl-struct-fastseedtype 24 193 83 9] "ESGEREAN"] [cl-struct-plansys 27 110 6 5 7 40 11520 6171 [cl-struct-fastseedtype 104 27 125 45] "DITIZA"] [cl-struct-plansys 228 0 0 5 10 46 33120 5860 [cl-struct-fastseedtype 172 228 219 155] "ANLE"] [cl-struct-plansys 29 1 3 0 5 24 5376 6685 [cl-struct-fastseedtype 132 29 45 159] "ONISQU"] [cl-struct-plansys 215 122 2 6 11 53 33920 6871 [cl-struct-fastseedtype 112 215 179 15] "ALEUSQU"] [cl-struct-plansys 186 72 0 2 10 43 20640 4026 [cl-struct-fastseedtype 16 186 109 100] "ZASOCEAT"] [cl-struct-plansys 81 133 5 4 5 30 9600 6481 [cl-struct-fastseedtype 100 81 91 62] "RILACE"] [cl-struct-plansys 249 165 7 1 2 17 2040 3833 [cl-struct-fastseedtype 12 249 189 179] "BEENRI"] [cl-struct-plansys 4 254 6 1 2 16 2560 4100 [cl-struct-fastseedtype 136 4 83 85] "LAEDEN"] [cl-struct-plansys 96 178 2 7 9 46 32384 5728 [cl-struct-fastseedtype 120 96 157 107] "MARIAR"] [cl-struct-plansys 52 74 2 3 7 34 15232 6452 [cl-struct-fastseedtype 220 52 27 254] "RIISER"] [cl-struct-plansys 44 176 0 2 8 35 16800 5420 [cl-struct-fastseedtype 84 44 141 90] "QUTIRI"] [cl-struct-plansys 80 190 6 4 3 23 5888 4432 [cl-struct-fastseedtype 96 80 51 70] "BIRAMABI"] [cl-struct-plansys 134 235 3 4 8 40 17920 4742 [cl-struct-fastseedtype 160 134 13 199] "SOORBI"] [cl-struct-plansys 135 134 6 2 5 29 5568 4743 [cl-struct-fastseedtype 20 135 27 71] "SOLAGEON"] [cl-struct-plansys 191 35 3 3 9 43 16856 4799 [cl-struct-fastseedtype 92 191 157 55] "TIQUAT"] [cl-struct-plansys 98 17 1 7 12 57 45144 6498 [cl-struct-fastseedtype 248 98 83 142] "REXEBE"] [cl-struct-plansys 132 243 3 1 5 25 7000 5508 [cl-struct-fastseedtype 136 132 189 122] "QUBEEN"] [cl-struct-plansys 96 242 2 1 6 28 8960 4192 [cl-struct-fastseedtype 12 96 91 69] "CETIISQU"] [cl-struct-plansys 26 61 5 4 6 34 10880 6426 [cl-struct-fastseedtype 36 26 237 238] "REBIA"] [cl-struct-plansys 132 79 7 2 1 14 2016 5252 [cl-struct-fastseedtype 80 132 179 25] "ORDIMA"] [cl-struct-plansys 146 139 3 6 9 46 25760 5522 [cl-struct-fastseedtype 48 146 173 10] "ARUSZATI"] [cl-struct-plansys 247 196 6 0 4 23 2944 4087 [cl-struct-fastseedtype 196 247 219 228] "ZALERIZA"] [cl-struct-plansys 4 126 6 5 4 28 8064 3844 [cl-struct-fastseedtype 172 4 125 36] "ZASOER"] [cl-struct-plansys 156 208 0 5 10 46 33120 3996 [cl-struct-fastseedtype 104 156 83 20] "RALEEN"] [cl-struct-plansys 199 50 2 3 10 46 20608 5575 [cl-struct-fastseedtype 152 199 221 122] "QURAVE"] [cl-struct-plansys 166 182 6 7 7 42 14784 3238 [cl-struct-fastseedtype 60 166 155 209] "ATREBIBI"] [cl-struct-plansys 166 166 6 6 6 37 11840 6054 [cl-struct-fastseedtype 244 166 77 124] "TEESDI"] [cl-struct-plansys 51 237 7 0 3 20 1920 5427 [cl-struct-fastseedtype 64 51 51 234] "ARARUS"] [cl-struct-plansys 28 40 2 0 5 23 5888 6684 [cl-struct-fastseedtype 192 28 77 79] "ARA"] [cl-struct-plansys 227 0 0 6 13 59 47200 4835 [cl-struct-fastseedtype 116 227 155 119] "TIANVE"] [cl-struct-plansys 136 182 6 7 5 34 11968 5512 [cl-struct-fastseedtype 252 136 93 154] "QUORTE"] [cl-struct-plansys 112 252 4 3 5 28 9408 4720 [cl-struct-fastseedtype 216 112 83 71] "SOLADIES"] [cl-struct-plansys 105 109 5 5 6 35 12600 5737 [cl-struct-fastseedtype 168 105 253 139] "MAXEEDSO"] [cl-struct-plansys 71 88 0 5 13 58 41760 3399 [cl-struct-fastseedtype 108 71 219 2] "XEXEDI"] [cl-struct-plansys 146 237 7 0 2 16 1536 3474 [cl-struct-fastseedtype 196 146 173 34] "XEXETI"] [cl-struct-plansys 29 87 7 6 4 30 7200 4637 [cl-struct-fastseedtype 48 29 179 23] "TIINLEBI"] [cl-struct-plansys 102 193 1 2 9 40 17280 3942 [cl-struct-fastseedtype 80 102 237 180] "RATEEDAR"] [cl-struct-plansys 138 248 0 4 11 49 31360 6794 [cl-struct-fastseedtype 36 138 91 95] "ONLEMA"] [cl-struct-plansys 12 203 3 1 5 25 7000 5132 [cl-struct-fastseedtype 76 12 61 185] "ORERVE"]] [[cl-struct-plansys 4 180 4 2 4 23 6624 6660 [cl-struct-fastseedtype 144 4 166 111] "AUSIS"] [cl-struct-plansys 53 157 5 6 6 36 14400 5685 [cl-struct-fastseedtype 112 53 58 251] "ANDIRI"] [cl-struct-plansys 199 254 6 7 8 46 16192 3783 [cl-struct-fastseedtype 56 199 54 147] "BEEDBEON"] [cl-struct-plansys 159 179 3 5 10 49 24696 5791 [cl-struct-fastseedtype 40 159 26 171] "MAISSO"] [cl-struct-plansys 240 71 7 0 0 8 768 4336 [cl-struct-fastseedtype 64 240 102 53] "LADIBE"] [cl-struct-plansys 53 147 3 0 5 24 5376 4405 [cl-struct-fastseedtype 192 53 26 54] "VERIAR"] [cl-struct-plansys 159 106 2 5 11 52 29952 5279 [cl-struct-fastseedtype 168 159 54 9] "ESBETE"] [cl-struct-plansys 217 188 4 7 8 44 23232 5337 [cl-struct-fastseedtype 56 217 58 169] "ESRILEES"] [cl-struct-plansys 169 193 1 6 10 48 34560 5289 [cl-struct-fastseedtype 112 169 166 169] "ESRASOCE"] [cl-struct-plansys 37 103 7 2 2 18 2592 3109 [cl-struct-fastseedtype 144 37 122 161] "LERELA"] [cl-struct-plansys 197 245 5 3 5 29 8120 5317 [cl-struct-fastseedtype 152 197 182 233] "ESZARA"] [cl-struct-plansys 226 84 6 1 4 24 3840 5858 [cl-struct-fastseedtype 200 226 218 219] "ANENAT"] [cl-struct-plansys 192 209 1 4 8 38 21888 4032 [cl-struct-fastseedtype 32 192 102 164] "ZALAQURA"] [cl-struct-plansys 116 154 2 4 7 35 17920 4212 [cl-struct-fastseedtype 224 116 90 69] "CERARE"] [cl-struct-plansys 169 14 6 1 3 20 3200 6057 [cl-struct-fastseedtype 8 169 182 12] "INZAQUMA"] [cl-struct-plansys 75 122 2 3 10 46 20608 5451 [cl-struct-fastseedtype 216 75 250 138] "ARATUSZA"] [cl-struct-plansys 4 40 0 2 8 35 16800 6148 [cl-struct-fastseedtype 80 4 166 125] "ISARIN"] [cl-struct-plansys 83 44 4 6 9 47 22560 5203 [cl-struct-fastseedtype 176 83 186 41] "ESESBI"] [cl-struct-plansys 249 38 6 7 6 38 13376 5625 [cl-struct-fastseedtype 248 249 54 202] "ARMAAZA"] [cl-struct-plansys 100 175 7 5 3 25 5400 6500 [cl-struct-fastseedtype 104 100 154 254] "RIENLA"] [cl-struct-plansys 134 117 7 0 2 16 1536 6278 [cl-struct-fastseedtype 0 134 102 13] "DIUSTE"] [cl-struct-plansys 178 156 6 0 3 19 2432 4530 [cl-struct-fastseedtype 0 178 154 86] "VEZADI"] [cl-struct-plansys 168 173 5 5 5 31 11160 5544 [cl-struct-fastseedtype 104 168 54 250] "QUAEN"] [cl-struct-plansys 60 242 2 7 9 46 32384 6460 [cl-struct-fastseedtype 120 60 186 126] "RIDIUSLA"] [cl-struct-plansys 149 104 0 6 11 51 40800 5781 [cl-struct-fastseedtype 48 149 166 171] "MAESARON"] [cl-struct-plansys 63 234 2 2 9 41 15744 3647 [cl-struct-fastseedtype 208 63 250 211] "BERIEN"] [cl-struct-plansys 228 18 2 3 7 34 15232 4068 [cl-struct-fastseedtype 88 228 182 244] "RAMAZA"] [cl-struct-plansys 164 196 6 1 2 16 2560 3748 [cl-struct-fastseedtype 8 164 90 83] "BEANEN"] [cl-struct-plansys 193 178 2 4 8 39 19968 3009 [cl-struct-fastseedtype 224 193 102 48] "ERCETIDI"] [cl-struct-plansys 109 151 7 4 3 24 4608 5229 [cl-struct-fastseedtype 32 109 218 169] "ESRECE"] [cl-struct-plansys 29 197 7 1 2 17 2040 3101 [cl-struct-fastseedtype 200 29 182 145] "ATRAZAMA"] [cl-struct-plansys 43 36 4 3 8 40 13440 3883 [cl-struct-fastseedtype 24 43 122 196] "ZARAGETE"] [cl-struct-plansys 220 3 3 2 5 26 8736 3804 [cl-struct-fastseedtype 16 220 166 243] "BEBEGE"] [cl-struct-plansys 105 163 3 6 8 42 23520 2921 [cl-struct-fastseedtype 240 105 58 224] "DITI"] [cl-struct-plansys 4 55 7 7 4 31 8184 5124 [cl-struct-fastseedtype 184 4 54 41] "ESCEBELE"] [cl-struct-plansys 33 147 3 5 8 41 20664 5409 [cl-struct-fastseedtype 168 33 26 26] "QUREAR"] [cl-struct-plansys 243 10 2 0 8 35 8960 6387 [cl-struct-fastseedtype 192 243 102 205] "DIMADI"] [cl-struct-plansys 38 141 7 0 2 16 1536 6694 [cl-struct-fastseedtype 64 38 26 127] "ONENTI"] [cl-struct-plansys 137 215 7 5 4 29 6264 3721 [cl-struct-fastseedtype 40 137 54 147] "BERAER"] [cl-struct-plansys 151 16 0 7 14 64 56320 6039 [cl-struct-fastseedtype 184 151 58 156] "TEZAEDED"] [cl-struct-plansys 88 120 0 6 10 47 37600 4184 [cl-struct-fastseedtype 240 88 166 21] "LABILAEN"] [cl-struct-plansys 82 85 5 2 5 28 6720 6482 [cl-struct-fastseedtype 16 82 122 142] "REVEVE"] [cl-struct-plansys 219 22 6 3 6 34 7616 4827 [cl-struct-fastseedtype 24 219 182 39] "SOORTE"] [cl-struct-plansys 93 28 6 1 3 20 3200 3421 [cl-struct-fastseedtype 72 93 218 146] "ENINTE"] [cl-struct-plansys 155 252 4 4 8 41 15744 3995 [cl-struct-fastseedtype 160 155 102 164] "ZATETEIS"] [cl-struct-plansys 93 124 4 4 6 33 12672 4445 [cl-struct-fastseedtype 96 93 90 22] "VEERGE"] [cl-struct-plansys 106 99 3 1 7 33 9240 6506 [cl-struct-fastseedtype 136 106 182 190] "RIDIVEXE"] [cl-struct-plansys 2 182 6 3 5 30 6720 4098 [cl-struct-fastseedtype 88 2 250 69] "CERISOMA"] [cl-struct-plansys 139 71 7 2 4 26 3744 3211 [cl-struct-fastseedtype 208 139 166 209] "ATZAXE"] [cl-struct-plansys 120 2 2 6 8 41 26240 6520 [cl-struct-fastseedtype 48 120 186 30] "RIEDA"] [cl-struct-plansys 231 47 7 7 7 43 11352 3047 [cl-struct-fastseedtype 120 231 54 176] "ERSOONVE"] [cl-struct-plansys 214 95 7 5 5 33 7128 6358 [cl-struct-fastseedtype 232 214 154 253] "ISBEUS"] [cl-struct-plansys 57 8 2 0 6 27 6912 4153 [cl-struct-fastseedtype 128 57 102 117] "LATIBI"] [cl-struct-plansys 146 102 6 0 3 19 2432 6802 [cl-struct-fastseedtype 128 146 154 175] "AUSAR"] [cl-struct-plansys 65 234 2 5 9 44 25344 3905 [cl-struct-fastseedtype 232 65 54 212] "RAVE"] [cl-struct-plansys 234 22 6 7 7 42 14784 3306 [cl-struct-fastseedtype 248 234 186 1] "LEGEARA"] [cl-struct-plansys 244 239 7 6 3 26 6240 4852 [cl-struct-fastseedtype 176 244 166 231] "SOINUSTE"] [cl-struct-plansys 92 168 0 2 8 35 16800 2908 [cl-struct-fastseedtype 80 92 250 208] "ERLAGE"] [cl-struct-plansys 169 3 3 3 7 35 13720 3497 [cl-struct-fastseedtype 216 169 182 130] "XEATE"] [cl-struct-plansys 14 92 6 1 4 24 3840 5390 [cl-struct-fastseedtype 136 14 90 154] "QUCEDI"] [cl-struct-plansys 77 173 5 4 5 30 9600 2893 [cl-struct-fastseedtype 96 77 102 0] "ANESCE"] [cl-struct-plansys 69 73 1 4 9 42 24192 5445 [cl-struct-fastseedtype 160 69 218 138] "ARORAR"] [cl-struct-plansys 143 234 2 1 9 40 12800 3727 [cl-struct-fastseedtype 72 143 182 147] "BEDIERAT"] [cl-struct-plansys 209 48 0 3 10 44 24640 6865 [cl-struct-fastseedtype 152 209 122 15] "AATENBE"] [cl-struct-plansys 19 242 2 2 9 41 15744 4627 [cl-struct-fastseedtype 144 19 166 23] "TIISES"] [cl-struct-plansys 126 73 1 6 11 52 37440 4222 [cl-struct-fastseedtype 112 126 58 229] "CEINER"] [cl-struct-plansys 162 16 0 7 13 60 52800 6818 [cl-struct-fastseedtype 56 162 54 95] "ONATBEZA"] [cl-struct-plansys 132 19 3 5 7 37 18648 5252 [cl-struct-fastseedtype 40 132 26 169] "ESDI"] [cl-struct-plansys 87 109 7 0 3 20 1920 4183 [cl-struct-fastseedtype 64 87 102 5] "CEARSO"] [cl-struct-plansys 246 39 7 0 2 16 1536 5110 [cl-struct-fastseedtype 192 246 26 232] "USCEZA"] [cl-struct-plansys 210 228 4 5 8 42 18144 6354 [cl-struct-fastseedtype 168 210 54 189] "ISRAZA"] [cl-struct-plansys 54 4 4 7 9 48 25344 6710 [cl-struct-fastseedtype 56 54 58 175] "AESAUS"] [cl-struct-plansys 104 207 7 6 3 26 6240 3176 [cl-struct-fastseedtype 112 104 166 33] "LEORGERI"] [cl-struct-plansys 94 227 3 2 7 34 11424 5726 [cl-struct-fastseedtype 144 94 122 155] "ANISTI"] [cl-struct-plansys 80 215 7 3 2 19 3192 4176 [cl-struct-fastseedtype 152 80 182 5] "CEDIZA"] [cl-struct-plansys 183 132 6 1 5 28 4480 5303 [cl-struct-fastseedtype 200 183 218 105] "ESCESO"] [cl-struct-plansys 215 199 7 4 5 32 6144 4055 [cl-struct-fastseedtype 32 215 102 68] "ZAXERICE"] [cl-struct-plansys 37 254 6 4 4 27 6912 4645 [cl-struct-fastseedtype 224 37 90 7] "SOMAED"] [cl-struct-plansys 140 88 2 1 6 28 8960 2956 [cl-struct-fastseedtype 8 140 182 16] "ERLAENED"] [cl-struct-plansys 152 146 2 3 7 34 15232 2968 [cl-struct-fastseedtype 216 152 250 32] "INBIBE"] [cl-struct-plansys 115 6 6 2 5 29 5568 4211 [cl-struct-fastseedtype 80 115 166 197] "CEONED"] [cl-struct-plansys 124 120 0 6 10 47 37600 3708 [cl-struct-fastseedtype 176 124 186 51] "BEUSED"] [cl-struct-plansys 52 216 0 7 11 52 45760 4404 [cl-struct-fastseedtype 248 52 54 54] "VEZAAES"] [cl-struct-plansys 41 175 7 5 4 29 6264 5929 [cl-struct-fastseedtype 104 41 154 28] "TELAAN"] [cl-struct-plansys 77 59 3 0 5 24 5376 6221 [cl-struct-fastseedtype 0 77 102 125] "ISCEER"] [cl-struct-plansys 83 208 2 0 8 35 8960 4947 [cl-struct-fastseedtype 0 83 154 40] "USMASO"] [cl-struct-plansys 59 199 7 5 6 37 7992 6459 [cl-struct-fastseedtype 104 59 54 78] "REERQU"] [cl-struct-plansys 121 218 2 7 10 50 35200 3961 [cl-struct-fastseedtype 120 121 186 164] "ZAEDVERA"] [cl-struct-plansys 180 22 6 6 4 29 9280 3764 [cl-struct-fastseedtype 48 180 166 195] "GEREBIED"] [cl-struct-plansys 88 6 6 2 2 17 3264 6232 [cl-struct-fastseedtype 208 88 250 237] "DIDIRA"] [cl-struct-plansys 207 148 4 3 8 40 13440 3023 [cl-struct-fastseedtype 88 207 182 176] "ERRARA"] [cl-struct-plansys 89 148 6 1 3 20 3200 3161 [cl-struct-fastseedtype 8 89 90 1] "LEREUS"] [cl-struct-plansys 56 72 0 4 9 41 26240 2872 [cl-struct-fastseedtype 224 56 102 112] "ERENANRI"] [cl-struct-plansys 254 155 3 4 8 40 17920 5886 [cl-struct-fastseedtype 32 254 218 139] "MACEA"] [cl-struct-plansys 96 175 7 1 1 13 1560 4192 [cl-struct-fastseedtype 200 96 182 53] "LACETEED"] [cl-struct-plansys 88 220 4 3 5 28 9408 5464 [cl-struct-fastseedtype 24 88 122 122] "QUALEMA"] [cl-struct-plansys 171 129 1 2 10 44 19008 5803 [cl-struct-fastseedtype 16 171 166 219] "ANUSA"] [cl-struct-plansys 114 143 7 6 5 34 8160 5490 [cl-struct-fastseedtype 240 114 58 10] "ARINES"] [cl-struct-plansys 159 137 1 7 13 61 48312 4255 [cl-struct-fastseedtype 184 159 54 53] "LARIBEBI"] [cl-struct-plansys 198 51 3 5 9 45 22680 5062 [cl-struct-fastseedtype 168 198 26 88] "EDATER"] [cl-struct-plansys 26 112 2 0 7 31 7936 6170 [cl-struct-fastseedtype 192 26 102 221] "ISUSLE"] [cl-struct-plansys 167 97 3 0 7 32 7168 3239 [cl-struct-fastseedtype 64 167 26 113] "ATORAT"] [cl-struct-plansys 124 145 1 5 9 43 27864 4732 [cl-struct-fastseedtype 40 124 54 135] "SOLAED"] [cl-struct-plansys 180 152 0 7 11 52 45760 3508 [cl-struct-fastseedtype 184 180 58 226] "XEABITI"] [cl-struct-plansys 215 198 6 6 7 41 13120 6359 [cl-struct-fastseedtype 240 215 166 205] "DIMAATMA"] [cl-struct-plansys 75 17 1 2 10 44 19008 4939 [cl-struct-fastseedtype 16 75 122 200] "USCEED"] [cl-struct-plansys 38 56 0 3 11 48 26880 3622 [cl-struct-fastseedtype 24 38 182 131] "GEXEIN"] [cl-struct-plansys 242 140 6 1 4 24 3840 3058 [cl-struct-fastseedtype 72 242 218 96] "ONEN"] [cl-struct-plansys 114 50 2 4 9 43 22016 3954 [cl-struct-fastseedtype 160 114 102 132] "ZAESRE"] [cl-struct-plansys 206 32 0 4 11 49 31360 5070 [cl-struct-fastseedtype 96 206 90 24] "EDSODI"] [cl-struct-plansys 13 237 7 1 2 17 2040 3341 [cl-struct-fastseedtype 136 13 182 2] "XERIREA"] [cl-struct-plansys 15 14 6 3 6 34 7616 5647 [cl-struct-fastseedtype 88 15 250 27] "ANORCEQU"] [cl-struct-plansys 186 101 5 2 5 28 6720 5306 [cl-struct-fastseedtype 208 186 166 89] "ORORRE"] [cl-struct-plansys 97 142 6 6 5 33 10560 4961 [cl-struct-fastseedtype 48 97 186 104] "USTILE"] [cl-struct-plansys 226 33 1 7 12 57 45144 6114 [cl-struct-fastseedtype 120 226 54 92] "TEONAN"] [cl-struct-plansys 91 159 7 5 6 37 7992 5723 [cl-struct-fastseedtype 232 91 154 91] "ANVERE"] [cl-struct-plansys 192 14 6 0 1 11 1408 4288 [cl-struct-fastseedtype 128 192 102 37] "CERAQU"] [cl-struct-plansys 243 218 2 0 8 35 8960 3315 [cl-struct-fastseedtype 128 243 154 193] "LEAZA"] [cl-struct-plansys 148 68 4 5 6 34 14688 5012 [cl-struct-fastseedtype 232 148 54 104] "USLERI"] [cl-struct-plansys 231 62 6 7 8 46 16192 4839 [cl-struct-fastseedtype 248 231 186 103] "SOREISBE"] [cl-struct-plansys 211 221 5 6 8 44 17600 6867 [cl-struct-fastseedtype 176 211 166 63] "ONATZALA"] [cl-struct-plansys 53 4 4 2 5 27 7776 5429 [cl-struct-fastseedtype 80 53 250 42] "ARZACE"] [cl-struct-plansys 84 197 5 3 4 25 7000 6484 [cl-struct-fastseedtype 216 84 182 126] "RIEDIN"] [cl-struct-plansys 131 108 6 1 5 28 4480 4995 [cl-struct-fastseedtype 136 131 90 136] "USEDGE"] [cl-struct-plansys 132 131 3 4 6 32 14336 2948 [cl-struct-fastseedtype 96 132 102 128] "USDIVE"] [cl-struct-plansys 150 141 5 4 6 34 10880 6038 [cl-struct-fastseedtype 160 150 218 172] "INERRA"] [cl-struct-plansys 146 20 6 1 4 24 3840 4754 [cl-struct-fastseedtype 72 146 182 119] "TIRIUSRI"] [cl-struct-plansys 190 40 0 3 11 48 26880 4286 [cl-struct-fastseedtype 152 190 122 5] "CEINERXE"] [cl-struct-plansys 162 176 0 2 10 43 20640 6818 [cl-struct-fastseedtype 144 162 166 63] "ONENLA"] [cl-struct-plansys 71 117 5 6 8 44 17600 6727 [cl-struct-fastseedtype 112 71 58 79] "AMAXE"] [cl-struct-plansys 253 162 2 7 10 50 35200 5885 [cl-struct-fastseedtype 56 253 54 171] "MAARBEES"] [cl-struct-plansys 233 243 3 5 8 41 20664 4841 [cl-struct-fastseedtype 40 233 26 39] "SOGEBE"] [cl-struct-plansys 62 19 3 0 6 28 6272 4158 [cl-struct-fastseedtype 64 62 102 85] "LASOAN"] [cl-struct-plansys 55 59 3 0 7 32 7168 5431 [cl-struct-fastseedtype 192 55 26 26] "QUINRI"] [cl-struct-plansys 133 222 6 5 5 32 9216 3205 [cl-struct-fastseedtype 168 133 54 241] "ATLAIN"] [cl-struct-plansys 19 204 4 7 10 52 27456 4115 [cl-struct-fastseedtype 56 19 58 53] "LARAISSO"] [cl-struct-plansys 167 93 5 6 8 44 17600 5287 [cl-struct-fastseedtype 112 167 166 25] "ORRIONTI"] [cl-struct-plansys 23 223 7 2 4 26 3744 4119 [cl-struct-fastseedtype 144 23 122 21] "LAINOR"] [cl-struct-plansys 91 57 1 3 11 49 24696 3163 [cl-struct-fastseedtype 152 91 182 161] "LEVERA"] [cl-struct-plansys 12 52 6 1 2 16 2560 4620 [cl-struct-fastseedtype 200 12 218 119] "TIEDIS"] [cl-struct-plansys 110 61 5 4 6 34 10880 3950 [cl-struct-fastseedtype 32 110 102 100] "ZAAXEVE"] [cl-struct-plansys 86 226 2 4 9 43 22016 5206 [cl-struct-fastseedtype 224 86 90 73] "ESXEXE"] [cl-struct-plansys 239 34 2 1 9 40 12800 4079 [cl-struct-fastseedtype 8 239 182 148] "RABIARCE"] [cl-struct-plansys 101 42 2 3 8 38 17024 4453 [cl-struct-fastseedtype 216 101 250 54] "VESOZAXE"] [cl-struct-plansys 98 100 4 2 6 31 8928 6242 [cl-struct-fastseedtype 80 98 166 141] "DIRAZA"] [cl-struct-plansys 37 68 4 6 7 39 18720 6181 [cl-struct-fastseedtype 176 37 186 189] "ISSOAR"] [cl-struct-plansys 239 10 2 7 12 58 40832 3567 [cl-struct-fastseedtype 248 239 54 34] "XEISARE"] [cl-struct-plansys 110 47 7 5 5 33 7128 5486 [cl-struct-fastseedtype 104 110 154 186] "QUEDLE"] [cl-struct-plansys 148 129 3 0 4 20 4480 6292 [cl-struct-fastseedtype 0 148 102 109] "DIXEZA"] [cl-struct-plansys 116 132 6 0 1 11 1408 5492 [cl-struct-fastseedtype 0 116 154 122] "QUENLE"] [cl-struct-plansys 78 97 1 5 11 51 33048 3406 [cl-struct-fastseedtype 104 78 54 34] "XEATXE"] [cl-struct-plansys 54 66 2 7 11 54 38016 5430 [cl-struct-fastseedtype 120 54 186 74] "ARGEZABE"] [cl-struct-plansys 83 68 4 6 9 47 22560 5715 [cl-struct-fastseedtype 48 83 166 91] "ANBEXEAT"] [cl-struct-plansys 241 162 2 2 7 33 12672 4849 [cl-struct-fastseedtype 208 241 250 135] "SOTEVE"] [cl-struct-plansys 58 150 6 3 5 30 6720 5946 [cl-struct-fastseedtype 88 58 182 236] "INISZA"] [cl-struct-plansys 142 228 6 1 4 24 3840 6798 [cl-struct-fastseedtype 8 142 90 47] "ALERI"] [cl-struct-plansys 47 94 6 4 6 35 8960 2863 [cl-struct-fastseedtype 224 47 102 48] "ERONONA"] [cl-struct-plansys 15 31 7 4 5 32 6144 6159 [cl-struct-fastseedtype 32 15 218 237] "DITEOR"] [cl-struct-plansys 35 25 3 1 8 37 10360 5155 [cl-struct-fastseedtype 200 35 182 89] "ORVERACE"] [cl-struct-plansys 5 20 4 3 6 32 10752 2821 [cl-struct-fastseedtype 24 5 122 176] "ERARONQU"] [cl-struct-plansys 250 127 7 2 3 22 3168 3834 [cl-struct-fastseedtype 16 250 166 67] "GEISAN"] [cl-struct-plansys 251 251 3 6 10 50 28000 4091 [cl-struct-fastseedtype 240 251 58 180] "RAMAAN"] [cl-struct-plansys 186 91 3 7 10 51 31416 3258 [cl-struct-fastseedtype 184 186 54 193] "LETIBEMA"] [cl-struct-plansys 235 83 3 5 10 49 24696 4587 [cl-struct-fastseedtype 168 235 26 22] "VERAVE"] [cl-struct-plansys 193 86 6 0 2 15 1920 6337 [cl-struct-fastseedtype 192 193 102 109] "DICELA"] [cl-struct-plansys 168 181 7 0 0 8 768 3752 [cl-struct-fastseedtype 64 168 26 227] "GEMA"] [cl-struct-plansys 239 203 3 5 10 49 24696 5871 [cl-struct-fastseedtype 40 239 54 251] "ANVE"] [cl-struct-plansys 81 160 0 7 12 56 49280 4945 [cl-struct-fastseedtype 184 81 58 168] "USQURAVE"] [cl-struct-plansys 214 148 4 6 8 43 20640 4310 [cl-struct-fastseedtype 240 214 166 5] "CEERDIZA"] [cl-struct-plansys 196 77 5 2 3 20 4800 3524 [cl-struct-fastseedtype 16 196 122 130] "XERAQU"] [cl-struct-plansys 241 218 2 3 8 38 17024 6897 [cl-struct-fastseedtype 24 241 182 95] "ONMATE"] [cl-struct-plansys 7 124 6 1 5 28 4480 6407 [cl-struct-fastseedtype 72 7 218 174] "REENUS"] [cl-struct-plansys 201 232 0 4 10 45 28800 4041 [cl-struct-fastseedtype 160 201 102 228] "ZAVEZAON"] [cl-struct-plansys 191 68 4 4 8 41 15744 5567 [cl-struct-fastseedtype 96 191 90 154] "QURITI"] [cl-struct-plansys 48 247 7 1 1 13 1560 4400 [cl-struct-fastseedtype 136 48 182 198] "BIABITE"] [cl-struct-plansys 156 230 6 3 3 22 4928 3228 [cl-struct-fastseedtype 88 156 250 113] "ATRAGEES"] [cl-struct-plansys 105 3 3 2 6 30 10080 3177 [cl-struct-fastseedtype 208 105 166 97] "LEREQU"] [cl-struct-plansys 202 154 2 6 10 49 31360 3530 [cl-struct-fastseedtype 48 202 186 50] "ENVEBE"] [cl-struct-plansys 93 147 3 7 9 47 28952 4957 [cl-struct-fastseedtype 120 93 54 136] "USORON"] [cl-struct-plansys 96 95 7 5 3 25 5400 5216 [cl-struct-fastseedtype 232 96 154 57] "ORORRA"] [cl-struct-plansys 199 148 6 0 4 23 2944 4295 [cl-struct-fastseedtype 128 199 102 85] "LAATRE"] [cl-struct-plansys 212 206 6 0 1 11 1408 3796 [cl-struct-fastseedtype 128 212 154 83] "BEVERI"] [cl-struct-plansys 103 30 6 5 7 40 11520 5991 [cl-struct-fastseedtype 232 103 54 124] "TEXEBI"] [cl-struct-plansys 100 230 6 7 5 34 11968 6244 [cl-struct-fastseedtype 248 100 186 77] "DIORMAEN"] [cl-struct-plansys 50 75 3 6 9 46 25760 4658 [cl-struct-fastseedtype 176 50 166 23] "TIVERE"] [cl-struct-plansys 142 224 0 2 10 43 20640 3982 [cl-struct-fastseedtype 80 142 250 4] "ZABESO"] [cl-struct-plansys 127 7 7 3 5 31 5208 5503 [cl-struct-fastseedtype 216 127 182 250] "QULETE"] [cl-struct-plansys 120 252 6 1 2 16 2560 4472 [cl-struct-fastseedtype 136 120 90 246] "VEMAOR"] [cl-struct-plansys 59 217 1 4 11 50 28800 2875 [cl-struct-fastseedtype 96 59 102 128] "LAATSO"] [cl-struct-plansys 103 81 1 4 11 50 28800 6503 [cl-struct-fastseedtype 160 103 218 78] "RESORI"] [cl-struct-plansys 21 190 6 1 3 20 3200 5653 [cl-struct-fastseedtype 72 21 182 219] "ANAMA"] [cl-struct-plansys 43 160 0 3 12 52 29120 5675 [cl-struct-fastseedtype 152 43 122 123] "ANSOREAT"] [cl-struct-plansys 177 238 6 2 3 21 4032 4785 [cl-struct-fastseedtype 144 177 166 231] "SOSOLE"] [cl-struct-plansys 144 33 1 6 9 44 31680 5264 [cl-struct-fastseedtype 112 144 58 57] "ORARRA"] [cl-struct-plansys 216 180 4 7 7 40 21120 4824 [cl-struct-fastseedtype 56 216 54 119] "TIGEBERE"] [cl-struct-plansys 206 83 3 5 9 45 22680 4302 [cl-struct-fastseedtype 40 206 26 37] "CEBIOR"] [cl-struct-plansys 165 57 3 0 5 24 5376 4261 [cl-struct-fastseedtype 64 165 102 37] "CEZAA"] [cl-struct-plansys 248 207 7 0 0 8 768 6136 [cl-struct-fastseedtype 192 248 26 204] "INBEED"] [cl-struct-plansys 184 88 0 5 10 46 33120 4280 [cl-struct-fastseedtype 168 184 54 165] "CEVERA"] [cl-struct-plansys 112 20 4 7 7 40 21120 5744 [cl-struct-fastseedtype 56 112 58 59] "ANONMABI"] [cl-struct-plansys 102 107 3 6 9 46 25760 3174 [cl-struct-fastseedtype 112 102 166 145] "ATGEANER"] [cl-struct-plansys 80 91 3 2 5 26 8736 6736 [cl-struct-fastseedtype 144 80 122 15] "AANAN"] [cl-struct-plansys 230 27 3 3 8 39 15288 6374 [cl-struct-fastseedtype 152 230 182 189] "ISONZA"] [cl-struct-plansys 225 100 6 1 3 20 3200 4321 [cl-struct-fastseedtype 200 225 218 5] "CEMABE"] [cl-struct-plansys 133 51 3 4 7 36 16128 3973 [cl-struct-fastseedtype 32 133 102 4] "ZATEBISO"] [cl-struct-plansys 7 70 6 4 6 35 8960 5639 [cl-struct-fastseedtype 224 7 90 11] "MAORIN"] [cl-struct-plansys 210 108 6 1 4 24 3840 5074 [cl-struct-fastseedtype 8 210 182 152] "EDTIXEEN"] [cl-struct-plansys 178 66 2 3 9 42 18816 6066 [cl-struct-fastseedtype 216 178 250 204] "INXEXEAT"] [cl-struct-plansys 209 66 2 2 7 33 12672 4305 [cl-struct-fastseedtype 80 209 166 213] "LAESER"] [cl-struct-plansys 78 144 0 6 12 55 44000 4686 [cl-struct-fastseedtype 176 78 186 199] "SOBITE"] [cl-struct-plansys 42 188 4 7 9 48 25344 6442 [cl-struct-fastseedtype 248 42 54 142] "REVEABE"] [cl-struct-plansys 51 47 7 5 6 37 7992 4915 [cl-struct-fastseedtype 104 51 154 216] "EDANSO"] [cl-struct-plansys 91 71 7 0 3 20 1920 6235 [cl-struct-fastseedtype 0 91 102 221] "ISONED"] [cl-struct-plansys 21 184 2 0 6 27 6912 5909 [cl-struct-fastseedtype 0 21 154 76] "INORAN"] [cl-struct-plansys 225 123 3 5 8 41 20664 4577 [cl-struct-fastseedtype 104 225 54 118] "VEENAR"] [cl-struct-plansys 115 42 2 7 12 58 40832 2931 [cl-struct-fastseedtype 120 115 186 112] "ERREENEN"] [cl-struct-plansys 114 242 2 6 10 49 31360 3698 [cl-struct-fastseedtype 48 114 166 115] "BEEDRIAR"] [cl-struct-plansys 10 190 6 2 4 25 4800 3082 [cl-struct-fastseedtype 208 10 250 161] "LEMAED"] [cl-struct-plansys 37 24 0 3 10 44 24640 4901 [cl-struct-fastseedtype 88 37 182 168] "USBIRA"] [cl-struct-plansys 67 180 6 1 5 28 4480 6211 [cl-struct-fastseedtype 8 67 90 221] "ISRARA"] [cl-struct-plansys 166 244 4 4 7 37 14208 2982 [cl-struct-fastseedtype 224 166 102 112] "ERINGE"] [cl-struct-plansys 160 35 3 4 6 32 14336 6816 [cl-struct-fastseedtype 32 160 218 207] "ABEGE"] [cl-struct-plansys 102 3 3 1 7 33 9240 6246 [cl-struct-fastseedtype 200 102 182 253] "ISSOINEN"] [cl-struct-plansys 50 204 4 3 7 36 12096 4402 [cl-struct-fastseedtype 24 50 122 102] "BICEISES"] [cl-struct-plansys 201 253 5 2 4 24 5760 5833 [cl-struct-fastseedtype 16 201 166 43] "MAENSO"] [cl-struct-plansys 4 231 7 6 3 26 6240 6404 [cl-struct-fastseedtype 240 4 58 222] "RIARDI"] [cl-struct-plansys 85 173 5 7 7 41 18040 6229 [cl-struct-fastseedtype 184 85 54 205] "DIERBEER"] [cl-struct-plansys 144 243 3 5 7 37 18648 3984 [cl-struct-fastseedtype 168 144 26 84] "RATITE"] [cl-struct-plansys 232 188 6 0 1 11 1408 6376 [cl-struct-fastseedtype 192 232 102 125] "ISXEES"] [cl-struct-plansys 41 137 3 0 5 24 5376 4137 [cl-struct-fastseedtype 64 41 26 213] "LASOCE"] [cl-struct-plansys 226 133 5 5 7 39 14040 6882 [cl-struct-fastseedtype 40 226 54 239] "ATIUS"] [cl-struct-plansys 110 40 0 7 13 60 52800 6510 [cl-struct-fastseedtype 184 110 58 238] "RECEXELA"] [cl-struct-plansys 85 226 2 6 9 45 28800 6229 [cl-struct-fastseedtype 240 85 166 189] "ISLAESIS"] [cl-struct-plansys 189 9 1 2 8 36 15552 6077 [cl-struct-fastseedtype 16 189 122 188] "TEGETE"] [cl-struct-plansys 60 252 4 3 5 28 9408 5692 [cl-struct-fastseedtype 24 60 182 187] "ANRAIN"] [cl-struct-plansys 156 236 6 1 2 16 2560 6044 [cl-struct-fastseedtype 72 156 218 124] "TECERI"] [cl-struct-plansys 160 30 6 4 3 23 5888 4000 [cl-struct-fastseedtype 160 160 102 196] "ZAGEUSER"] [cl-struct-plansys 48 232 0 4 9 41 26240 5936 [cl-struct-fastseedtype 96 48 90 156] "TELALE"] [cl-struct-plansys 211 129 3 1 8 37 10360 5587 [cl-struct-fastseedtype 136 211 182 10] "ARRIES"] [cl-struct-plansys 169 62 6 3 4 26 5824 4777 [cl-struct-fastseedtype 88 169 250 71] "SOALEED"] [cl-struct-plansys 152 33 1 2 7 32 13824 5272 [cl-struct-fastseedtype 208 152 166 233] "ESGEBI"] [cl-struct-plansys 179 38 6 6 7 41 13120 6067 [cl-struct-fastseedtype 48 179 186 124] "TELACE"] [cl-struct-plansys 88 133 5 7 6 37 16280 3928 [cl-struct-fastseedtype 120 88 54 52] "RAENONCE"] [cl-struct-plansys 229 159 7 5 4 29 6264 4837 [cl-struct-fastseedtype 232 229 154 151] "TITEQU"] [cl-struct-plansys 78 154 2 0 7 31 7936 4174 [cl-struct-fastseedtype 128 78 102 5] "CEREXE"] [cl-struct-plansys 53 66 2 0 6 27 6912 4149 [cl-struct-fastseedtype 128 53 154 101] "CEISED"] [cl-struct-plansys 186 120 0 5 12 54 38880 3002 [cl-struct-fastseedtype 232 186 54 16] "ERGERE"] [cl-struct-plansys 97 14 6 7 6 38 13376 3681 [cl-struct-fastseedtype 248 97 186 179] "BEZAORAT"] [cl-struct-plansys 17 57 1 6 10 48 34560 6673 [cl-struct-fastseedtype 176 17 166 111] "AANTESO"] [cl-struct-plansys 103 60 4 2 7 35 10080 6503 [cl-struct-fastseedtype 80 103 250 94] "RIXEES"] [cl-struct-plansys 42 201 1 3 10 45 22680 4394 [cl-struct-fastseedtype 216 42 182 246] "VEARIN"] [cl-struct-plansys 237 12 6 1 3 20 3200 4077 [cl-struct-fastseedtype 136 237 90 228] "ZARIA"] [cl-struct-plansys 114 175 7 4 4 28 5376 2930 [cl-struct-fastseedtype 96 114 102 0] "XELAED"] [cl-struct-plansys 184 149 5 4 4 26 8320 3000 [cl-struct-fastseedtype 160 184 218 112] "ERRIUS"] [cl-struct-plansys 24 232 2 1 6 28 8960 6680 [cl-struct-fastseedtype 72 24 182 191] "ONEDED"] [cl-struct-plansys 24 152 0 3 9 40 22400 3096 [cl-struct-fastseedtype 152 24 122 113] "ATXEIN"]] [[cl-struct-plansys 8 105 1 4 8 38 21888 6408 [cl-struct-fastseedtype 33 8 77 222] "RIAVE"] [cl-struct-plansys 99 54 6 5 7 40 11520 5219 [cl-struct-fastseedtype 233 99 133 233] "ESGEER"] [cl-struct-plansys 61 208 0 3 10 44 24640 3389 [cl-struct-fastseedtype 217 61 45 146] "ENERZA"] [cl-struct-plansys 164 112 2 0 5 23 5888 4260 [cl-struct-fastseedtype 1 164 213 181] "LAARZACE"] [cl-struct-plansys 165 36 4 6 7 39 18720 4005 [cl-struct-fastseedtype 49 165 141 148] "RARIED"] [cl-struct-plansys 138 152 0 7 13 60 52800 3210 [cl-struct-fastseedtype 249 138 101 129] "LEZAER"] [cl-struct-plansys 214 176 0 5 12 54 38880 5846 [cl-struct-fastseedtype 169 214 237 43] "MABELALA"] [cl-struct-plansys 149 181 5 2 4 24 5760 6549 [cl-struct-fastseedtype 81 149 181 254] "RIARRIBI"] [cl-struct-plansys 38 222 6 0 3 19 2432 6438 [cl-struct-fastseedtype 193 38 205 78] "REBEES"] [cl-struct-plansys 107 201 3 1 8 37 10360 4971 [cl-struct-fastseedtype 137 107 69 136] "USTEER"] [cl-struct-plansys 245 176 0 7 12 56 49280 3829 [cl-struct-fastseedtype 249 245 173 99] "GEARZAEN"] [cl-struct-plansys 210 100 4 4 7 37 14208 3026 [cl-struct-fastseedtype 33 210 149 64] "VEES"] [cl-struct-plansys 64 137 1 2 7 32 13824 3136 [cl-struct-fastseedtype 209 64 13 65] "LEMADIOR"] [cl-struct-plansys 217 141 5 3 5 29 8120 3545 [cl-struct-fastseedtype 153 217 37 114] "ENEDZA"] [cl-struct-plansys 111 36 6 1 5 28 4480 6255 [cl-struct-fastseedtype 201 111 109 109] "DILEON"] [cl-struct-plansys 78 223 7 6 5 34 8160 6734 [cl-struct-fastseedtype 113 78 117 47] "AISQU"] [cl-struct-plansys 103 218 2 4 10 47 24064 6759 [cl-struct-fastseedtype 97 103 77 159] "ONLEUSES"] [cl-struct-plansys 105 232 0 5 11 50 36000 3689 [cl-struct-fastseedtype 41 105 5 51] "BEZAED"] [cl-struct-plansys 216 32 0 3 9 40 22400 6360 [cl-struct-fastseedtype 25 216 45 125] "ISLAMA"] [cl-struct-plansys 190 203 3 0 6 28 6272 6846 [cl-struct-fastseedtype 65 190 85 255] "ONUSBI"] [cl-struct-plansys 206 70 6 6 6 37 11840 6350 [cl-struct-fastseedtype 113 206 141 157] "ISENRIRE"] [cl-struct-plansys 110 30 6 7 7 42 14784 6510 [cl-struct-fastseedtype 57 110 229 62] "RIMAEDXE"] [cl-struct-plansys 130 120 0 5 12 54 38880 4482 [cl-struct-fastseedtype 233 130 237 198] "BIXEIN"] [cl-struct-plansys 149 12 4 2 5 27 7776 3989 [cl-struct-fastseedtype 145 149 53 100] "ZABION"] [cl-struct-plansys 106 255 7 0 2 16 1536 6762 [cl-struct-fastseedtype 1 106 205 111] "AORBETE"] [cl-struct-plansys 252 179 3 1 5 25 7000 5372 [cl-struct-fastseedtype 201 252 197 137] "ESORZA"] [cl-struct-plansys 132 192 0 7 11 52 45760 6532 [cl-struct-fastseedtype 57 132 173 126] "RIGEBI"] [cl-struct-plansys 8 198 6 4 3 23 5888 3336 [cl-struct-fastseedtype 97 8 21 146] "ENANRE"] [cl-struct-plansys 238 251 3 2 7 34 11424 5614 [cl-struct-fastseedtype 17 238 13 74] "ARBEMA"] [cl-struct-plansys 232 107 3 3 6 31 12152 4840 [cl-struct-fastseedtype 217 232 165 135] "SOANINER"] [cl-struct-plansys 176 76 6 1 2 16 2560 5040 [cl-struct-fastseedtype 9 176 109 216] "EDRAISAR"] [cl-struct-plansys 10 93 5 6 7 40 16000 5898 [cl-struct-fastseedtype 177 10 245 60] "TECEINRE"] [cl-struct-plansys 206 236 4 4 7 37 14208 3022 [cl-struct-fastseedtype 161 206 77 96] "ANAR"] [cl-struct-plansys 198 74 2 5 10 48 27648 6086 [cl-struct-fastseedtype 105 198 133 44] "INISER"] [cl-struct-plansys 154 48 0 3 11 48 26880 5018 [cl-struct-fastseedtype 89 154 45 8] "USENLAER"] [cl-struct-plansys 80 117 7 0 0 8 768 4944 [cl-struct-fastseedtype 129 80 213 152] "EDRIUSON"] [cl-struct-plansys 63 72 0 6 13 59 47200 4415 [cl-struct-fastseedtype 177 63 141 230] "BIRERA"] [cl-struct-plansys 233 148 4 7 8 44 23232 6121 [cl-struct-fastseedtype 121 233 101 236] "INAR"] [cl-struct-plansys 151 64 0 5 13 58 41760 3223 [cl-struct-fastseedtype 41 151 237 65] "LEORENDI"] [cl-struct-plansys 77 242 2 2 7 33 12672 5197 [cl-struct-fastseedtype 209 77 181 89] "ORQULELA"] [cl-struct-plansys 53 65 3 0 5 24 5376 2869 [cl-struct-fastseedtype 65 53 205 16] "ERSODI"] [cl-struct-plansys 102 205 7 1 3 21 2520 5734 [cl-struct-fastseedtype 9 102 69 187] "ANREER"] [cl-struct-plansys 186 16 0 7 13 60 52800 5306 [cl-struct-fastseedtype 121 186 173 185] "ORZAEDVE"] [cl-struct-plansys 54 248 0 4 11 49 31360 3638 [cl-struct-fastseedtype 161 54 149 179] "BEXEIN"] [cl-struct-plansys 99 204 4 2 7 35 10080 3683 [cl-struct-fastseedtype 81 99 13 19] "BEGEORER"] [cl-struct-plansys 16 185 1 3 8 37 18648 6160 [cl-struct-fastseedtype 25 16 37 13] "DIVERA"] [cl-struct-plansys 216 244 6 1 2 16 2560 3800 [cl-struct-fastseedtype 73 216 109 163] "GEAMA"] [cl-struct-plansys 254 235 3 6 9 46 25760 5630 [cl-struct-fastseedtype 241 254 117 90] "QUCERI"] [cl-struct-plansys 61 158 6 4 4 27 6912 3133 [cl-struct-fastseedtype 225 61 77 33] "LEISTEAN"] [cl-struct-plansys 123 92 4 5 9 46 19872 4475 [cl-struct-fastseedtype 169 123 5 214] "VEREZA"] [cl-struct-plansys 132 0 0 3 9 40 22400 3716 [cl-struct-fastseedtype 153 132 45 51] "BETIA"] [cl-struct-plansys 90 111 7 0 2 16 1536 3418 [cl-struct-fastseedtype 193 90 85 130] "XEINES"] [cl-struct-plansys 248 41 1 6 9 44 31680 6904 [cl-struct-fastseedtype 241 248 141 111] "AENQUTE"] [cl-struct-plansys 252 250 2 7 9 46 32384 5372 [cl-struct-fastseedtype 185 252 229 137] "ESLEUSBE"] [cl-struct-plansys 19 8 0 5 13 58 41760 5907 [cl-struct-fastseedtype 105 19 237 156] "TEEDUS"] [cl-struct-plansys 190 104 0 2 10 43 20640 6846 [cl-struct-fastseedtype 17 190 53 223] "ONBIGE"] [cl-struct-plansys 136 163 3 0 4 20 4480 3208 [cl-struct-fastseedtype 129 136 205 49] "ATISTISO"] [cl-struct-plansys 167 23 7 1 4 25 3000 6055 [cl-struct-fastseedtype 73 167 197 28] "TEANED"] [cl-struct-plansys 152 160 0 7 11 52 45760 3992 [cl-struct-fastseedtype 185 152 173 20] "RADIQU"] [cl-struct-plansys 92 250 2 4 7 35 17920 4188 [cl-struct-fastseedtype 225 92 21 165] "CETERI"] [cl-struct-plansys 160 254 6 2 2 17 3264 6048 [cl-struct-fastseedtype 145 160 13 156] "TEANTI"] [cl-struct-plansys 79 119 7 3 5 31 5208 3407 [cl-struct-fastseedtype 89 79 165 2] "XEESANUS"] [cl-struct-plansys 232 27 3 1 5 25 7000 6632 [cl-struct-fastseedtype 137 232 109 206] "REENESMA"] [cl-struct-plansys 43 137 1 6 12 56 40320 4651 [cl-struct-fastseedtype 49 43 245 135] "SOISERLA"] [cl-struct-plansys 181 240 0 4 10 45 28800 3509 [cl-struct-fastseedtype 33 181 77 226] "XESOON"] [cl-struct-plansys 136 31 7 5 3 25 5400 6792 [cl-struct-fastseedtype 233 136 133 47] "ATIA"] [cl-struct-plansys 150 143 7 3 4 27 4536 6550 [cl-struct-fastseedtype 217 150 45 254] "RIZAORIS"] [cl-struct-plansys 221 185 3 0 5 24 5376 5853 [cl-struct-fastseedtype 1 221 213 187] "ANENMAQU"] [cl-struct-plansys 250 235 3 6 9 46 25760 5114 [cl-struct-fastseedtype 49 250 141 56] "EDRIAT"] [cl-struct-plansys 167 81 1 7 13 61 48312 4775 [cl-struct-fastseedtype 249 167 101 23] "TIERA"] [cl-struct-plansys 247 207 7 5 6 37 7992 4855 [cl-struct-fastseedtype 169 247 237 215] "TIONREBI"] [cl-struct-plansys 230 110 6 2 4 25 4800 4070 [cl-struct-fastseedtype 81 230 181 244] "RAARCEGE"] [cl-struct-plansys 99 37 7 0 3 20 1920 3427 [cl-struct-fastseedtype 193 99 205 210] "ENANEN"] [cl-struct-plansys 192 146 2 1 6 28 8960 6592 [cl-struct-fastseedtype 137 192 69 174] "REA"] [cl-struct-plansys 30 111 7 7 6 39 10296 6686 [cl-struct-fastseedtype 249 30 173 143] "ARIDIAN"] [cl-struct-plansys 123 205 5 4 7 38 12160 4475 [cl-struct-fastseedtype 33 123 149 102] "BIREER"] [cl-struct-plansys 165 144 0 2 9 39 18720 4261 [cl-struct-fastseedtype 209 165 13 229] "CEANBIBI"] [cl-struct-plansys 166 166 6 3 5 30 6720 5030 [cl-struct-fastseedtype 153 166 37 104] "USRAGE"] [cl-struct-plansys 224 195 3 1 5 25 7000 5344 [cl-struct-fastseedtype 201 224 109 89] "ORISED"] [cl-struct-plansys 143 56 0 6 13 59 47200 4239 [cl-struct-fastseedtype 113 143 117 197] "CEDILE"] [cl-struct-plansys 52 225 1 4 8 38 21888 3636 [cl-struct-fastseedtype 97 52 77 163] "GEORATRE"] [cl-struct-plansys 238 145 1 5 11 51 33048 5358 [cl-struct-fastseedtype 41 238 5 57] "OREDONAT"] [cl-struct-plansys 209 223 7 3 3 23 3864 5329 [cl-struct-fastseedtype 25 209 45 105] "ESORRA"] [cl-struct-plansys 215 84 6 0 4 23 2944 4311 [cl-struct-fastseedtype 65 215 85 69] "CEERDI"] [cl-struct-plansys 67 141 5 6 8 44 17600 3139 [cl-struct-fastseedtype 113 67 141 65] "LEENTIMA"] [cl-struct-plansys 235 151 7 7 7 43 11352 4075 [cl-struct-fastseedtype 57 235 229 148] "RATITIGE"] [cl-struct-plansys 67 151 7 5 6 37 7992 3395 [cl-struct-fastseedtype 233 67 237 242] "ENRECE"] [cl-struct-plansys 198 5 5 2 5 28 6720 5574 [cl-struct-fastseedtype 145 198 53 154] "QUBIBI"] [cl-struct-plansys 199 198 6 0 4 23 2944 3783 [cl-struct-fastseedtype 1 199 205 243] "BELETEAT"] [cl-struct-plansys 177 60 6 1 3 20 3200 6833 [cl-struct-fastseedtype 201 177 197 111] "AISONDI"] [cl-struct-plansys 77 127 7 7 5 35 9240 5453 [cl-struct-fastseedtype 57 77 173 42] "ARTIA"] [cl-struct-plansys 145 111 7 4 3 24 4608 5009 [cl-struct-fastseedtype 97 145 21 248] "EDEDXEBE"] [cl-struct-plansys 115 130 2 2 9 41 15744 6515 [cl-struct-fastseedtype 17 115 13 238] "REGEZA"] [cl-struct-plansys 21 68 4 3 6 32 10752 6165 [cl-struct-fastseedtype 217 21 165 61] "ISTIMALE"] [cl-struct-plansys 193 235 3 1 6 29 8120 4033 [cl-struct-fastseedtype 9 193 109 68] "ZAERVEMA"] [cl-struct-plansys 43 246 6 6 7 41 13120 3371 [cl-struct-fastseedtype 177 43 245 18] "ENLABEAN"] [cl-struct-plansys 187 115 3 4 9 44 19712 4027 [cl-struct-fastseedtype 161 187 77 100] "ZABEBE"] [cl-struct-plansys 171 179 3 5 10 49 24696 3499 [cl-struct-fastseedtype 105 171 133 242] "ENATA"] [cl-struct-plansys 51 239 7 3 5 31 5208 3891 [cl-struct-fastseedtype 89 51 45 116] "RAVERIES"] [cl-struct-plansys 73 62 6 0 2 15 1920 6473 [cl-struct-fastseedtype 129 73 213 30] "RIBIARA"] [cl-struct-plansys 212 15 7 6 3 26 6240 5588 [cl-struct-fastseedtype 177 212 141 138] "ARREDI"] [cl-struct-plansys 198 205 5 7 8 45 19800 3526 [cl-struct-fastseedtype 121 198 101 2] "XEVEON"] [cl-struct-plansys 248 95 7 5 3 25 5400 6392 [cl-struct-fastseedtype 41 248 237 237] "DICEMARI"] [cl-struct-plansys 94 43 3 2 7 34 11424 6750 [cl-struct-fastseedtype 209 94 181 207] "AQUUSEN"] [cl-struct-plansys 178 136 2 0 7 31 7936 4018 [cl-struct-fastseedtype 65 178 205 148] "RAAVE"] [cl-struct-plansys 123 22 6 1 5 28 4480 3195 [cl-struct-fastseedtype 9 123 69 97] "LEENA"] [cl-struct-plansys 35 207 7 7 7 43 11352 4131 [cl-struct-fastseedtype 121 35 173 229] "CEEDLEON"] [cl-struct-plansys 159 225 1 4 11 50 28800 5279 [cl-struct-fastseedtype 161 159 149 89] "ORQUBE"] [cl-struct-plansys 8 211 3 2 5 26 8736 4616 [cl-struct-fastseedtype 81 8 13 183] "TIBEENIS"] [cl-struct-plansys 157 82 2 3 8 38 17024 3741 [cl-struct-fastseedtype 25 157 37 131] "GEENBE"] [cl-struct-plansys 137 147 3 1 6 29 8120 6793 [cl-struct-fastseedtype 73 137 109 143] "AMAZA"] [cl-struct-plansys 255 196 4 6 9 47 22560 3071 [cl-struct-fastseedtype 241 255 117 112] "ERLACE"] [cl-struct-plansys 74 165 5 4 6 34 10880 4170 [cl-struct-fastseedtype 225 74 77 37] "CELACE"] [cl-struct-plansys 192 133 5 5 5 31 11160 6080 [cl-struct-fastseedtype 169 192 5 92] "TEXEONIS"] [cl-struct-plansys 189 191 7 3 3 23 3864 6845 [cl-struct-fastseedtype 153 189 45 31] "ONANED"] [cl-struct-plansys 51 120 2 0 8 35 8960 4915 [cl-struct-fastseedtype 193 51 85 72] "USRAER"] [cl-struct-plansys 173 112 0 6 11 51 40800 3757 [cl-struct-fastseedtype 241 173 141 19] "BEENBEOR"] [cl-struct-plansys 57 243 3 7 9 47 28952 6713 [cl-struct-fastseedtype 185 57 229 95] "ONDISORA"] [cl-struct-plansys 20 39 7 5 3 25 5400 4884 [cl-struct-fastseedtype 105 20 237 200] "USZALE"] [cl-struct-plansys 175 225 1 2 10 44 19008 4271 [cl-struct-fastseedtype 17 175 53 149] "LABIAR"] [cl-struct-plansys 37 106 2 0 6 27 6912 4133 [cl-struct-fastseedtype 129 37 205 181] "LACETE"] [cl-struct-plansys 28 32 2 1 6 28 8960 3356 [cl-struct-fastseedtype 73 28 197 130] "XEONONLE"] [cl-struct-plansys 161 95 7 7 5 35 9240 2977 [cl-struct-fastseedtype 185 161 173 192] "LEGE"] [cl-struct-plansys 165 35 3 4 7 36 16128 5797 [cl-struct-fastseedtype 225 165 21 139] "MARACECE"] [cl-struct-plansys 101 133 5 2 4 24 5760 2917 [cl-struct-fastseedtype 145 101 13 64] "MAER"] [cl-struct-plansys 60 208 0 3 9 40 22400 4924 [cl-struct-fastseedtype 89 60 165 56] "EDCEQUOR"] [cl-struct-plansys 57 186 2 1 7 32 10240 5433 [cl-struct-fastseedtype 137 57 109 58] "QUREXEIN"] [cl-struct-plansys 12 162 2 6 8 41 26240 6156 [cl-struct-fastseedtype 49 12 245 221] "ISDITIXE"] [cl-struct-plansys 226 119 7 4 4 28 5376 4578 [cl-struct-fastseedtype 33 226 77 230] "BIONUS"] [cl-struct-plansys 45 8 0 5 11 50 36000 4141 [cl-struct-fastseedtype 233 45 133 117] "LAMARE"] [cl-struct-plansys 111 78 6 3 6 34 7616 5487 [cl-struct-fastseedtype 217 111 45 106] "ARUSXEVE"] [cl-struct-plansys 150 2 2 0 7 31 7936 3222 [cl-struct-fastseedtype 1 150 213 193] "LEQUENA"] [cl-struct-plansys 207 178 2 6 11 53 33920 6095 [cl-struct-fastseedtype 49 207 141 220] "TERIAR"] [cl-struct-plansys 68 10 2 7 9 46 32384 6212 [cl-struct-fastseedtype 249 68 101 173] "DITERE"] [cl-struct-plansys 152 238 6 5 4 28 8064 3736 [cl-struct-fastseedtype 169 152 237 131] "GEMASOTI"] [cl-struct-plansys 183 39 7 2 4 26 3744 5559 [cl-struct-fastseedtype 81 183 181 234] "ARARIN"] [cl-struct-plansys 32 108 6 0 1 11 1408 4384 [cl-struct-fastseedtype 193 32 205 86] "VEGEAN"] [cl-struct-plansys 149 91 3 1 6 29 8120 3989 [cl-struct-fastseedtype 137 149 69 212] "RAZARE"] [cl-struct-plansys 199 46 6 7 8 46 16192 5831 [cl-struct-fastseedtype 249 199 173 187] "ANENVEZA"] [cl-struct-plansys 164 54 6 4 3 23 5888 6052 [cl-struct-fastseedtype 33 164 149 140] "INBITI"] [cl-struct-plansys 138 151 7 2 3 22 3168 5258 [cl-struct-fastseedtype 209 138 13 137] "ESMAONBE"] [cl-struct-plansys 243 191 7 3 5 31 5208 6643 [cl-struct-fastseedtype 153 243 37 94] "RIERXE"] [cl-struct-plansys 209 98 2 1 7 32 10240 4305 [cl-struct-fastseedtype 201 209 109 69] "CEORAT"] [cl-struct-plansys 80 145 1 6 9 44 31680 5712 [cl-struct-fastseedtype 113 80 117 91] "ANISUS"] [cl-struct-plansys 129 232 0 4 10 45 28800 4737 [cl-struct-fastseedtype 97 129 77 167] "SOATQUBE"] [cl-struct-plansys 243 58 2 5 11 52 29952 6899 [cl-struct-fastseedtype 41 243 5 63] "ONINRIAR"] [cl-struct-plansys 74 158 6 3 5 30 6720 4170 [cl-struct-fastseedtype 25 74 45 85] "LAISIS"] [cl-struct-plansys 112 221 7 0 0 8 768 5744 [cl-struct-fastseedtype 65 112 85 139] "MAEDRA"] [cl-struct-plansys 56 212 4 6 6 35 16800 4152 [cl-struct-fastseedtype 113 56 141 229] "CEENERUS"] [cl-struct-plansys 232 16 0 7 11 52 45760 5608 [cl-struct-fastseedtype 57 232 229 234] "ARGEVEZA"] [cl-struct-plansys 132 182 6 5 4 28 8064 6532 [cl-struct-fastseedtype 233 132 237 30] "RIQURI"] [cl-struct-plansys 119 254 6 2 5 29 5568 2935 [cl-struct-fastseedtype 145 119 53 208] "ERBIDI"] [cl-struct-plansys 164 141 7 0 0 8 768 4772 [cl-struct-fastseedtype 1 164 205 119] "TIESCEBI"] [cl-struct-plansys 230 197 7 1 3 21 2520 4326 [cl-struct-fastseedtype 201 230 197 85] "LALERIVE"] [cl-struct-plansys 150 62 6 7 7 42 14784 4502 [cl-struct-fastseedtype 57 150 173 214] "VEMAED"] [cl-struct-plansys 154 24 0 4 11 49 31360 6554 [cl-struct-fastseedtype 97 154 21 94] "RIERESED"] [cl-struct-plansys 120 9 1 2 7 32 13824 3448 [cl-struct-fastseedtype 17 120 13 146] "ENBEIS"] [cl-struct-plansys 194 29 5 3 6 33 9240 3778 [cl-struct-fastseedtype 217 194 165 243] "BEBEAREN"] [cl-struct-plansys 82 138 2 1 8 36 11520 2898 [cl-struct-fastseedtype 9 82 109 176] "ERINAIN"] [cl-struct-plansys 204 143 7 6 3 26 6240 5068 [cl-struct-fastseedtype 177 204 245 232] "USCEQUUS"] [cl-struct-plansys 40 250 2 4 7 35 17920 4904 [cl-struct-fastseedtype 161 40 77 104] "USMATE"] [cl-struct-plansys 16 28 4 5 6 34 14688 4880 [cl-struct-fastseedtype 105 16 133 184] "EDCERE"] [cl-struct-plansys 76 174 6 3 3 22 4928 2892 [cl-struct-fastseedtype 89 76 45 224] "QUSOXE"] [cl-struct-plansys 194 7 7 0 2 16 1536 4034 [cl-struct-fastseedtype 129 194 213 164] "ZAREVEES"] [cl-struct-plansys 233 214 6 6 5 33 10560 6633 [cl-struct-fastseedtype 177 233 141 46] "REREBI"] [cl-struct-plansys 35 6 6 7 8 46 16192 4899 [cl-struct-fastseedtype 121 35 101 24] "EDXERI"] [cl-struct-plansys 217 126 6 5 5 32 9216 5337 [cl-struct-fastseedtype 41 217 237 153] "ORATZAA"] [cl-struct-plansys 239 100 4 2 7 35 10080 4335 [cl-struct-fastseedtype 209 239 181 69] "CEQUAA"] [cl-struct-plansys 175 207 7 0 3 20 1920 5039 [cl-struct-fastseedtype 65 175 205 24] "EDTION"] [cl-struct-plansys 16 95 7 1 1 13 1560 4624 [cl-struct-fastseedtype 9 16 69 7] "SOVERE"] [cl-struct-plansys 12 142 6 7 5 34 11968 3084 [cl-struct-fastseedtype 121 12 173 17] "ATINARUS"] [cl-struct-plansys 136 202 2 4 7 35 17920 6792 [cl-struct-fastseedtype 161 136 149 255] "ONENQU"] [cl-struct-plansys 45 218 2 2 7 33 12672 5677 [cl-struct-fastseedtype 81 45 13 91] "ANGEMAAR"] [cl-struct-plansys 170 235 3 3 8 39 15288 5290 [cl-struct-fastseedtype 25 170 37 249] "ORREEN"] [cl-struct-plansys 186 50 2 1 8 36 11520 5818 [cl-struct-fastseedtype 73 186 109 123] "ANSOIS"] [cl-struct-plansys 128 157 5 6 5 32 12800 4480 [cl-struct-fastseedtype 241 128 117 134] "BICEIN"] [cl-struct-plansys 215 172 4 4 8 41 15744 5335 [cl-struct-fastseedtype 225 215 77 41] "ESDIRECE"] [cl-struct-plansys 133 174 6 5 5 32 9216 3461 [cl-struct-fastseedtype 169 133 5 226] "XEVERIVE"] [cl-struct-plansys 118 126 6 3 5 30 6720 5750 [cl-struct-fastseedtype 153 118 45 11] "MAONLE"] [cl-struct-plansys 140 129 3 0 4 20 4480 6540 [cl-struct-fastseedtype 193 140 85 14] "RETETI"] [cl-struct-plansys 226 183 7 6 5 34 8160 4834 [cl-struct-fastseedtype 241 226 141 183] "TIENINVE"] [cl-struct-plansys 246 236 4 7 9 48 25344 4342 [cl-struct-fastseedtype 185 246 229 53] "LAORBILA"] [cl-struct-plansys 149 70 6 5 5 32 9216 3989 [cl-struct-fastseedtype 105 149 237 244] "RAERQU"] [cl-struct-plansys 32 90 2 2 6 29 11136 5664 [cl-struct-fastseedtype 17 32 53 75] "MABIAT"] [cl-struct-plansys 66 49 3 0 6 28 6272 5186 [cl-struct-fastseedtype 129 66 205 57] "ORDIESAT"] [cl-struct-plansys 17 41 3 1 6 29 8120 4881 [cl-struct-fastseedtype 73 17 197 232] "USGERIAR"] [cl-struct-plansys 42 30 6 7 7 42 14784 5930 [cl-struct-fastseedtype 185 42 173 108] "INLAIN"] [cl-struct-plansys 110 76 4 4 7 37 14208 3182 [cl-struct-fastseedtype 225 110 21 113] "ATININAR"] [cl-struct-plansys 170 12 4 2 6 31 8928 4010 [cl-struct-fastseedtype 145 170 13 228] "ZAANES"] [cl-struct-plansys 169 41 1 3 9 41 20664 6569 [cl-struct-fastseedtype 89 169 165 110] "RELEORAR"] [cl-struct-plansys 10 89 3 1 7 33 9240 4362 [cl-struct-fastseedtype 137 10 109 166] "BIARANDI"] [cl-struct-plansys 109 187 3 6 8 42 23520 3693 [cl-struct-fastseedtype 49 109 245 51] "BEISRIA"] [cl-struct-plansys 143 254 6 4 6 35 8960 5519 [cl-struct-fastseedtype 33 143 77 234] "ARTIAT"] [cl-struct-plansys 82 241 1 5 11 51 33048 5714 [cl-struct-fastseedtype 233 82 133 187] "ANONDI"] [cl-struct-plansys 200 13 5 3 4 25 7000 4552 [cl-struct-fastseedtype 217 200 45 214] "VEINMAA"] [cl-struct-plansys 207 75 3 0 7 32 7168 4815 [cl-struct-fastseedtype 1 207 213 199] "SOXEORZA"] [cl-struct-plansys 36 121 1 6 9 44 31680 2852 [cl-struct-fastseedtype 49 36 141 128] "RIGE"] [cl-struct-plansys 97 195 3 7 9 47 28952 3681 [cl-struct-fastseedtype 249 97 101 67] "GEUSDI"] [cl-struct-plansys 185 13 5 5 6 35 12600 6841 [cl-struct-fastseedtype 169 185 237 47] "ATIUS"] [cl-struct-plansys 8 224 0 2 8 35 16800 2824 [cl-struct-fastseedtype 81 8 181 224] "ARBEIS"] [cl-struct-plansys 93 179 3 0 5 24 5376 5469 [cl-struct-fastseedtype 193 93 205 218] "QUMAZA"] [cl-struct-plansys 234 36 6 1 4 24 3840 5610 [cl-struct-fastseedtype 137 234 69 250] "QUUSDI"] [cl-struct-plansys 240 237 5 7 6 37 16280 4848 [cl-struct-fastseedtype 249 240 173 231] "SOBIONDI"] [cl-struct-plansys 77 159 7 4 3 24 4608 3405 [cl-struct-fastseedtype 33 77 149 178] "ENRIRI"] [cl-struct-plansys 239 158 6 2 5 29 5568 6383 [cl-struct-fastseedtype 209 239 13 45] "DIANED"] [cl-struct-plansys 192 216 0 3 9 40 22400 4032 [cl-struct-fastseedtype 153 192 37 84] "RAINLE"] [cl-struct-plansys 66 1 3 1 7 33 9240 3138 [cl-struct-fastseedtype 201 66 109 49] "ATLAAR"] [cl-struct-plansys 145 234 2 6 9 45 28800 3217 [cl-struct-fastseedtype 113 145 117 241] "ATDIA"] [cl-struct-plansys 78 239 7 4 4 28 5376 5710 [cl-struct-fastseedtype 97 78 77 171] "MAESGEED"] [cl-struct-plansys 120 227 3 5 7 37 18648 4216 [cl-struct-fastseedtype 41 120 5 69] "CEISGE"] [cl-struct-plansys 67 93 5 3 7 37 10360 3139 [cl-struct-fastseedtype 25 67 45 65] "LELEBI"] [cl-struct-plansys 137 102 6 0 2 15 1920 3209 [cl-struct-fastseedtype 65 137 85 209] "ATAN"] [cl-struct-plansys 173 27 3 6 8 42 23520 5293 [cl-struct-fastseedtype 113 173 141 137] "ESENESCE"] [cl-struct-plansys 101 137 1 7 11 53 41976 2917 [cl-struct-fastseedtype 57 101 229 64] "ALACE"] [cl-struct-plansys 69 213 5 5 6 35 12600 5445 [cl-struct-fastseedtype 233 69 237 74] "ARBITI"] [cl-struct-plansys 168 247 7 2 1 14 2016 4520 [cl-struct-fastseedtype 145 168 53 6] "BIBIRA"] [cl-struct-plansys 1 84 6 0 2 15 1920 5633 [cl-struct-fastseedtype 1 1 205 251] "ANATREAN"] [cl-struct-plansys 155 78 6 1 5 28 4480 5787 [cl-struct-fastseedtype 201 155 197 59] "ANCEISON"] [cl-struct-plansys 95 253 5 7 9 49 21560 3423 [cl-struct-fastseedtype 57 95 173 130] "XEONLE"] [cl-struct-plansys 35 193 1 4 11 50 28800 3875 [cl-struct-fastseedtype 97 35 21 196] "ZAUSERIS"] [cl-struct-plansys 253 144 0 2 9 39 18720 4605 [cl-struct-fastseedtype 17 253 13 54] "VEGEVE"] [cl-struct-plansys 239 246 6 3 6 34 7616 5359 [cl-struct-fastseedtype 217 239 165 169] "ESAESGE"] [cl-struct-plansys 99 41 3 1 8 37 10360 5987 [cl-struct-fastseedtype 9 99 109 28] "TEUSUSDI"] [cl-struct-plansys 237 40 0 6 11 51 40800 6637 [cl-struct-fastseedtype 177 237 245 190] "RILALELA"] [cl-struct-plansys 21 129 1 4 9 42 24192 5909 [cl-struct-fastseedtype 161 21 77 108] "INGECE"] [cl-struct-plansys 245 133 5 5 6 35 12600 6645 [cl-struct-fastseedtype 105 245 133 126] "RIORDI"] [cl-struct-plansys 229 109 5 3 5 29 8120 6117 [cl-struct-fastseedtype 89 229 45 76] "INRIERAN"] [cl-struct-plansys 187 208 2 0 8 35 8960 5563 [cl-struct-fastseedtype 129 187 213 42] "ARVEISRI"] [cl-struct-plansys 126 157 5 6 7 40 16000 3454 [cl-struct-fastseedtype 177 126 141 210] "ENREON"] [cl-struct-plansys 0 63 7 7 4 31 8184 6400 [cl-struct-fastseedtype 121 0 101 46] "REREIS"] [cl-struct-plansys 58 157 5 5 7 39 14040 4154 [cl-struct-fastseedtype 41 58 237 69] "CEISIS"] [cl-struct-plansys 0 157 5 2 3 20 4800 5632 [cl-struct-fastseedtype 209 0 181 187] "ANQUVEIN"] [cl-struct-plansys 44 22 6 0 1 11 1408 5932 [cl-struct-fastseedtype 65 44 205 156] "TEONUS"] [cl-struct-plansys 37 168 2 1 7 32 10240 6181 [cl-struct-fastseedtype 9 37 69 173] "DIQUDI"] [cl-struct-plansys 117 77 5 7 7 41 18040 6261 [cl-struct-fastseedtype 121 117 173 61] "ISBEAT"] [cl-struct-plansys 241 179 3 4 7 36 16128 4337 [cl-struct-fastseedtype 161 241 149 165] "CEARLE"] [cl-struct-plansys 210 225 1 2 9 40 17280 6866 [cl-struct-fastseedtype 81 210 13 255] "ONBEZATI"] [cl-struct-plansys 55 132 4 3 8 40 13440 6711 [cl-struct-fastseedtype 25 55 37 111] "AARAT"] [cl-struct-plansys 107 209 3 1 8 37 10360 4715 [cl-struct-fastseedtype 73 107 109 103] "SOGEVE"] [cl-struct-plansys 129 118 6 6 5 33 10560 6017 [cl-struct-fastseedtype 241 129 117 156] "TELABE"] [cl-struct-plansys 228 179 3 4 6 32 14336 6372 [cl-struct-fastseedtype 225 228 77 45] "DICETIAR"] [cl-struct-plansys 202 215 7 5 5 33 7128 5066 [cl-struct-fastseedtype 169 202 5 104] "USARISA"] [cl-struct-plansys 175 61 5 3 7 37 10360 4783 [cl-struct-fastseedtype 153 175 45 247] "TIGEAR"] [cl-struct-plansys 101 138 2 0 6 27 6912 3941 [cl-struct-fastseedtype 193 101 85 212] "RAZARI"] [cl-struct-plansys 151 254 6 6 7 41 13120 5783 [cl-struct-fastseedtype 241 151 141 91] "ANENCEBE"] [cl-struct-plansys 51 229 5 7 9 49 21560 5683 [cl-struct-fastseedtype 185 51 229 11] "MACECEVE"] [cl-struct-plansys 150 101 5 5 7 39 14040 2966 [cl-struct-fastseedtype 105 150 237 32] "TEBE"] [cl-struct-plansys 17 211 3 2 6 30 10080 3089 [cl-struct-fastseedtype 17 17 53 1] "LEBIED"] [cl-struct-plansys 223 248 2 0 8 35 8960 6367 [cl-struct-fastseedtype 129 223 205 189] "ISLAENBI"] [cl-struct-plansys 134 50 2 1 8 36 11520 6534 [cl-struct-fastseedtype 73 134 197 78] "RESOISBE"] [cl-struct-plansys 51 221 5 7 9 49 21560 4915 [cl-struct-fastseedtype 185 51 173 24] "EDESLA"] [cl-struct-plansys 183 117 5 4 7 38 12160 4791 [cl-struct-fastseedtype 225 183 21 87] "TIZABEA"] [cl-struct-plansys 111 147 3 2 8 38 12768 4975 [cl-struct-fastseedtype 145 111 13 136] "USMAXE"] [cl-struct-plansys 150 130 2 3 9 42 18816 3990 [cl-struct-fastseedtype 89 150 165 164] "ZAISEDAN"] [cl-struct-plansys 91 248 2 1 9 40 12800 3419 [cl-struct-fastseedtype 137 91 109 18] "ENBIRARE"] [cl-struct-plansys 78 212 4 6 8 43 20640 5198 [cl-struct-fastseedtype 49 78 245 137] "ESDICETE"]] [[cl-struct-plansys 16 210 2 0 5 23 5888 6160 [cl-struct-fastseedtype 66 16 154 189] "ISREMA"] [cl-struct-plansys 203 110 6 2 5 29 5568 5579 [cl-struct-fastseedtype 210 203 10 218] "QUBEOR"] [cl-struct-plansys 167 185 1 6 12 56 40320 4263 [cl-struct-fastseedtype 178 167 90 117] "LAVEBE"] [cl-struct-plansys 64 243 3 0 4 20 4480 5440 [cl-struct-fastseedtype 2 64 170 10] "ARRIESEN"] [cl-struct-plansys 210 137 1 4 10 46 26496 5586 [cl-struct-fastseedtype 98 210 26 154] "QULAXE"] [cl-struct-plansys 241 98 2 6 9 45 28800 5361 [cl-struct-fastseedtype 242 241 202 73] "ESISOR"] [cl-struct-plansys 193 217 1 2 8 36 15552 5057 [cl-struct-fastseedtype 82 193 218 56] "EDBEDI"] [cl-struct-plansys 218 205 5 4 6 34 10880 6106 [cl-struct-fastseedtype 162 218 106 124] "TECETEIS"] [cl-struct-plansys 29 124 6 0 2 15 1920 6429 [cl-struct-fastseedtype 130 29 154 190] "RIEDRA"] [cl-struct-plansys 75 52 4 2 7 35 10080 4683 [cl-struct-fastseedtype 18 75 138 215] "TIVEORSO"] [cl-struct-plansys 167 121 1 6 12 56 40320 5031 [cl-struct-fastseedtype 242 167 90 120] "EDORQU"] [cl-struct-plansys 204 186 2 0 5 23 5888 3020 [cl-struct-fastseedtype 66 204 42 32] "BIEN"] [cl-struct-plansys 89 146 2 4 8 39 19968 3673 [cl-struct-fastseedtype 162 89 26 147] "BEONINON"] [cl-struct-plansys 127 108 4 6 9 47 22560 5759 [cl-struct-fastseedtype 50 127 74 107] "MAVELEGE"] [cl-struct-plansys 3 65 1 2 10 44 19008 5635 [cl-struct-fastseedtype 146 3 218 155] "ANTE"] [cl-struct-plansys 252 128 0 4 9 41 26240 6396 [cl-struct-fastseedtype 226 252 234 93] "ISANLA"] [cl-struct-plansys 110 53 7 0 2 16 1536 6766 [cl-struct-fastseedtype 194 110 154 127] "ONZAENVE"] [cl-struct-plansys 182 18 2 2 8 37 14208 6326 [cl-struct-fastseedtype 82 182 10 237] "DILAOR"] [cl-struct-plansys 252 89 1 6 9 44 31680 5884 [cl-struct-fastseedtype 50 252 90 11] "MAUSRA"] [cl-struct-plansys 212 104 2 0 5 23 5888 6356 [cl-struct-fastseedtype 130 212 170 157] "ISQUINZA"] [cl-struct-plansys 196 76 4 4 5 29 11136 6084 [cl-struct-fastseedtype 226 196 26 236] "INISRE"] [cl-struct-plansys 152 174 6 6 4 29 9280 3992 [cl-struct-fastseedtype 114 152 202 68] "ZAMAES"] [cl-struct-plansys 57 105 1 2 8 36 15552 6457 [cl-struct-fastseedtype 210 57 218 46] "REATQU"] [cl-struct-plansys 59 58 2 4 10 47 24064 4667 [cl-struct-fastseedtype 34 59 106 71] "SOISONZA"] [cl-struct-plansys 68 63 7 0 0 8 768 2884 [cl-struct-fastseedtype 2 68 154 64] "ZAUS"] [cl-struct-plansys 77 72 0 2 9 39 18720 5453 [cl-struct-fastseedtype 146 77 138 90] "QUERORZA"] [cl-struct-plansys 228 153 1 6 9 44 31680 6628 [cl-struct-fastseedtype 114 228 90 110] "REMARI"] [cl-struct-plansys 152 62 6 0 1 11 1408 3736 [cl-struct-fastseedtype 194 152 42 195] "GEQUVE"] [cl-struct-plansys 84 246 6 4 3 23 5888 4180 [cl-struct-fastseedtype 34 84 26 229] "CEAUSIS"] [cl-struct-plansys 125 104 0 6 11 51 40800 4477 [cl-struct-fastseedtype 178 125 74 22] "VETEERZA"] [cl-struct-plansys 164 144 0 2 8 35 16800 3236 [cl-struct-fastseedtype 18 164 218 49] "ATBITELA"] [cl-struct-plansys 213 60 4 4 6 33 12672 5077 [cl-struct-fastseedtype 98 213 234 120] "EDMAOR"] [cl-struct-plansys 221 217 3 0 5 24 5376 3293 [cl-struct-fastseedtype 66 221 154 65] "LEEDTIER"] [cl-struct-plansys 80 23 7 2 1 14 2016 2896 [cl-struct-fastseedtype 210 80 10 96] "SOED"] [cl-struct-plansys 160 120 0 6 10 47 37600 3232 [cl-struct-fastseedtype 178 160 90 225] "LEQUTEOR"] [cl-struct-plansys 89 124 6 0 2 15 1920 2905 [cl-struct-fastseedtype 2 89 170 208] "ERBIERSO"] [cl-struct-plansys 71 208 0 4 12 53 33920 6471 [cl-struct-fastseedtype 98 71 26 190] "RILAAN"] [cl-struct-plansys 110 219 3 6 9 46 25760 6766 [cl-struct-fastseedtype 242 110 202 31] "ONESED"] [cl-struct-plansys 130 248 0 2 10 43 20640 3970 [cl-struct-fastseedtype 82 130 218 228] "ZAONBI"] [cl-struct-plansys 11 198 6 4 6 35 8960 3339 [cl-struct-fastseedtype 162 11 106 50] "ENCEGEQU"] [cl-struct-plansys 122 67 3 0 6 28 6272 3450 [cl-struct-fastseedtype 130 122 154 194] "XEIS"] [cl-struct-plansys 0 189 5 2 3 20 4800 6144 [cl-struct-fastseedtype 18 0 138 61] "ISQUEDER"] [cl-struct-plansys 112 56 0 6 10 47 37600 3952 [cl-struct-fastseedtype 242 112 90 164] "ZADIGE"] [cl-struct-plansys 85 99 3 0 5 24 5376 4437 [cl-struct-fastseedtype 66 85 42 6] "BIRIOR"] [cl-struct-plansys 222 25 1 4 10 46 26496 4830 [cl-struct-fastseedtype 162 222 26 183] "TIACEIN"] [cl-struct-plansys 172 69 5 6 5 32 12800 3244 [cl-struct-fastseedtype 50 172 74 161] "LEENRA"] [cl-struct-plansys 20 224 0 2 8 35 16800 4628 [cl-struct-fastseedtype 146 20 218 135] "SOTEORIS"] [cl-struct-plansys 29 25 1 4 9 42 24192 3613 [cl-struct-fastseedtype 226 29 234 179] "BEMATE"] [cl-struct-plansys 91 188 6 0 4 23 2944 3675 [cl-struct-fastseedtype 194 91 154 3] "GETEANAN"] [cl-struct-plansys 155 123 3 2 8 38 12768 3739 [cl-struct-fastseedtype 82 155 10 51] "BEESED"] [cl-struct-plansys 149 24 0 6 11 51 40800 4757 [cl-struct-fastseedtype 50 149 90 247] "TIZAATDI"] [cl-struct-plansys 205 49 3 0 5 24 5376 3789 [cl-struct-fastseedtype 130 205 170 163] "GEXEBEOR"] [cl-struct-plansys 89 19 3 4 7 36 16128 2905 [cl-struct-fastseedtype 226 89 26 16] "ERISSO"] [cl-struct-plansys 117 231 7 6 4 30 7200 5493 [cl-struct-fastseedtype 114 117 202 218] "QUTIUS"] [cl-struct-plansys 154 136 0 2 10 43 20640 5530 [cl-struct-fastseedtype 210 154 218 90] "QUISBE"] [cl-struct-plansys 76 115 3 4 6 32 14336 6220 [cl-struct-fastseedtype 34 76 106 61] "ISISBILE"] [cl-struct-plansys 193 134 6 0 2 15 1920 4033 [cl-struct-fastseedtype 2 193 154 68] "ZAINAT"] [cl-struct-plansys 98 145 1 2 9 40 17280 2914 [cl-struct-fastseedtype 146 98 138 128] "RAEDDI"] [cl-struct-plansys 77 88 0 6 11 51 40800 5453 [cl-struct-fastseedtype 114 77 90 26] "QUONSO"] [cl-struct-plansys 1 39 7 0 1 12 1152 5121 [cl-struct-fastseedtype 194 1 42 233] "ESENIS"] [cl-struct-plansys 249 253 5 4 5 30 9600 5369 [cl-struct-fastseedtype 34 249 26 9] "ESONLEAR"] [cl-struct-plansys 10 1 1 6 11 52 37440 5898 [cl-struct-fastseedtype 178 10 74 12] "INEDALA"] [cl-struct-plansys 85 47 7 2 2 18 2592 6229 [cl-struct-fastseedtype 18 85 218 157] "ISXELAVE"] [cl-struct-plansys 214 21 5 4 6 34 10880 6614 [cl-struct-fastseedtype 98 214 234 14] "REAN"] [cl-struct-plansys 234 224 2 0 7 31 7936 4330 [cl-struct-fastseedtype 66 234 154 197] "CEERLA"] [cl-struct-plansys 149 64 0 2 9 39 18720 4501 [cl-struct-fastseedtype 210 149 10 102] "BIANTI"] [cl-struct-plansys 217 55 7 6 4 30 7200 6361 [cl-struct-fastseedtype 178 217 90 77] "DIRICEEN"] [cl-struct-plansys 50 133 7 0 2 16 1536 4402 [cl-struct-fastseedtype 2 50 170 22] "VERETITE"] [cl-struct-plansys 252 23 7 4 2 20 3840 3580 [cl-struct-fastseedtype 98 252 26 226] "XELARA"] [cl-struct-plansys 171 212 4 6 9 47 22560 4267 [cl-struct-fastseedtype 242 171 202 117] "LALATI"] [cl-struct-plansys 131 23 7 2 4 26 3744 2947 [cl-struct-fastseedtype 82 131 218 144] "ERMAON"] [cl-struct-plansys 252 63 7 4 2 20 3840 5116 [cl-struct-fastseedtype 162 252 106 104] "USCEARTI"] [cl-struct-plansys 23 10 2 0 8 35 8960 4375 [cl-struct-fastseedtype 130 23 154 198] "BIUSBI"] [cl-struct-plansys 117 198 6 2 3 21 4032 3701 [cl-struct-fastseedtype 18 117 138 35] "GERITIOR"] [cl-struct-plansys 121 247 7 6 4 30 7200 2937 [cl-struct-fastseedtype 242 121 90 208] "ERLEIN"] [cl-struct-plansys 158 140 6 0 3 19 2432 6046 [cl-struct-fastseedtype 66 158 42 108] "INVE"] [cl-struct-plansys 163 160 0 4 12 53 33920 5795 [cl-struct-fastseedtype 162 163 26 219] "ANONRIOR"] [cl-struct-plansys 153 158 6 6 5 33 10560 4761 [cl-struct-fastseedtype 50 153 74 87] "TIREONCE"] [cl-struct-plansys 101 127 7 2 2 18 2592 3685 [cl-struct-fastseedtype 146 101 218 115] "BEEDENRI"] [cl-struct-plansys 254 50 2 4 9 43 22016 5374 [cl-struct-fastseedtype 226 254 234 137] "ESANGE"] [cl-struct-plansys 136 67 3 0 4 20 4480 4744 [cl-struct-fastseedtype 194 136 154 135] "SORAZA"] [cl-struct-plansys 64 100 4 2 4 23 6624 5184 [cl-struct-fastseedtype 82 64 10 249] "ORISTI"] [cl-struct-plansys 110 215 7 6 5 34 8160 3694 [cl-struct-fastseedtype 50 110 90 227] "GEUSQUBI"] [cl-struct-plansys 134 122 2 0 7 31 7936 5254 [cl-struct-fastseedtype 130 134 170 41] "ESARQURE"] [cl-struct-plansys 46 218 2 4 9 43 22016 3886 [cl-struct-fastseedtype 226 46 26 52] "RAIS"] [cl-struct-plansys 18 160 0 6 12 55 44000 2834 [cl-struct-fastseedtype 114 18 202 240] "ERGESO"] [cl-struct-plansys 59 167 7 2 4 26 3744 4411 [cl-struct-fastseedtype 210 59 218 134] "BIESIN"] [cl-struct-plansys 29 44 4 4 6 33 12672 3613 [cl-struct-fastseedtype 34 29 106 179] "BEISDIRI"] [cl-struct-plansys 126 205 7 0 2 16 1536 4990 [cl-struct-fastseedtype 2 126 154 72] "USRAQU"] [cl-struct-plansys 55 90 2 2 9 41 15744 4407 [cl-struct-fastseedtype 146 55 138 38] "BIEDTIVE"] [cl-struct-plansys 246 23 7 6 5 34 8160 4598 [cl-struct-fastseedtype 114 246 90 198] "BIBEER"] [cl-struct-plansys 42 144 2 0 7 31 7936 6698 [cl-struct-fastseedtype 194 42 42 143] "AARZA"] [cl-struct-plansys 222 4 4 4 7 37 14208 6366 [cl-struct-fastseedtype 34 222 26 45] "DIAQUTI"] [cl-struct-plansys 87 26 2 6 11 53 33920 3415 [cl-struct-fastseedtype 178 87 74 130] "XERAREBI"] [cl-struct-plansys 70 206 6 2 4 25 4800 5190 [cl-struct-fastseedtype 18 70 218 9] "ESRIRETI"] [cl-struct-plansys 151 110 6 4 6 35 8960 3991 [cl-struct-fastseedtype 98 151 234 36] "ZAMASO"] [cl-struct-plansys 55 231 7 0 3 20 1920 5175 [cl-struct-fastseedtype 66 55 154 73] "ESUSESQU"] [cl-struct-plansys 154 233 1 2 9 40 17280 6042 [cl-struct-fastseedtype 210 154 10 236] "INAVE"] [cl-struct-plansys 82 246 6 6 6 37 11840 5202 [cl-struct-fastseedtype 178 82 90 185] "ORXEREMA"] [cl-struct-plansys 203 14 6 0 4 23 2944 6091 [cl-struct-fastseedtype 2 203 170 220] "TEVERIAT"] [cl-struct-plansys 241 94 6 4 4 27 6912 4593 [cl-struct-fastseedtype 98 241 26 6] "BILADI"] [cl-struct-plansys 168 77 5 6 5 32 12800 5800 [cl-struct-fastseedtype 242 168 202 75] "MALEVE"] [cl-struct-plansys 196 54 6 2 2 17 3264 6084 [cl-struct-fastseedtype 82 196 218 60] "TETIED"] [cl-struct-plansys 173 56 0 4 10 45 28800 6573 [cl-struct-fastseedtype 162 173 106 30] "RICEATRA"] [cl-struct-plansys 244 209 3 0 4 20 4480 5620 [cl-struct-fastseedtype 130 244 154 202] "ARERA"] [cl-struct-plansys 170 79 7 2 3 22 3168 5290 [cl-struct-fastseedtype 18 170 138 137] "ESXEVEXE"] [cl-struct-plansys 194 182 6 6 6 37 11840 6082 [cl-struct-fastseedtype 242 194 90 252] "TELALA"] [cl-struct-plansys 167 53 7 0 3 20 1920 3495 [cl-struct-fastseedtype 66 167 42 82] "ENRESO"] [cl-struct-plansys 168 39 7 4 2 20 3840 6824 [cl-struct-fastseedtype 162 168 26 255] "ONATIBI"] [cl-struct-plansys 70 119 7 6 5 34 8160 6214 [cl-struct-fastseedtype 50 70 74 141] "DIARRIVE"] [cl-struct-plansys 246 30 6 2 4 25 4800 6902 [cl-struct-fastseedtype 146 246 218 95] "ONRAMAON"] [cl-struct-plansys 159 203 3 4 9 44 19712 6815 [cl-struct-fastseedtype 226 159 234 223] "ONMAAR"] [cl-struct-plansys 245 202 2 0 6 27 6912 5877 [cl-struct-fastseedtype 194 245 154 11] "MAINDICE"] [cl-struct-plansys 165 205 5 2 4 24 5760 6821 [cl-struct-fastseedtype 82 165 10 63] "ONATVE"] [cl-struct-plansys 135 150 6 6 7 41 13120 6791 [cl-struct-fastseedtype 50 135 90 207] "AINGEON"] [cl-struct-plansys 255 67 3 0 7 32 7168 6911 [cl-struct-fastseedtype 130 255 170 47] "AENLEGE"] [cl-struct-plansys 67 161 1 4 11 50 28800 4931 [cl-struct-fastseedtype 226 67 26 88] "EDISOR"] [cl-struct-plansys 111 217 1 6 12 56 40320 4463 [cl-struct-fastseedtype 114 111 202 134] "BIABI"] [cl-struct-plansys 28 198 6 2 2 17 3264 3356 [cl-struct-fastseedtype 210 28 218 178] "ENLACE"] [cl-struct-plansys 174 101 5 4 6 34 10880 5294 [cl-struct-fastseedtype 34 174 106 169] "ESISRAAN"] [cl-struct-plansys 123 20 6 0 4 23 2944 6011 [cl-struct-fastseedtype 2 123 154 76] "INTEGE"] [cl-struct-plansys 204 163 3 2 5 26 8736 6092 [cl-struct-fastseedtype 146 204 138 76] "INTEVEON"] [cl-struct-plansys 223 214 6 6 7 41 13120 3551 [cl-struct-fastseedtype 114 223 90 114] "ENSOOR"] [cl-struct-plansys 19 121 3 0 7 32 7168 4115 [cl-struct-fastseedtype 194 19 42 181] "LAXEMA"] [cl-struct-plansys 3 11 3 4 9 44 19712 3075 [cl-struct-fastseedtype 34 3 26 81] "ATONBEZA"] [cl-struct-plansys 100 179 3 6 7 38 21280 4964 [cl-struct-fastseedtype 178 100 74 120] "EDERDITI"] [cl-struct-plansys 119 109 5 2 6 32 7680 4215 [cl-struct-fastseedtype 18 119 218 117] "LAQUSOED"] [cl-struct-plansys 24 71 7 4 2 20 3840 5400 [cl-struct-fastseedtype 98 24 234 186] "QUANRE"] [cl-struct-plansys 196 238 6 0 1 11 1408 6340 [cl-struct-fastseedtype 66 196 154 205] "DIENON"] [cl-struct-plansys 95 18 2 2 9 41 15744 3423 [cl-struct-fastseedtype 210 95 10 242] "ENGELA"] [cl-struct-plansys 11 181 5 6 8 44 17600 4107 [cl-struct-fastseedtype 178 11 90 37] "CEBITIZA"] [cl-struct-plansys 36 23 7 0 0 8 768 3364 [cl-struct-fastseedtype 2 36 170 34] "XERICEBI"] [cl-struct-plansys 38 165 5 4 6 34 10880 5414 [cl-struct-fastseedtype 98 38 26 42] "ARLABI"] [cl-struct-plansys 101 70 6 6 5 33 10560 3173 [cl-struct-fastseedtype 242 101 202 161] "LEDILA"] [cl-struct-plansys 69 85 5 2 4 24 5760 4933 [cl-struct-fastseedtype 82 69 218 232] "USGEAT"] [cl-struct-plansys 30 177 1 4 10 46 26496 3870 [cl-struct-fastseedtype 162 30 106 84] "RACEEDAT"] [cl-struct-plansys 17 152 2 0 6 27 6912 6417 [cl-struct-fastseedtype 130 17 154 206] "REEDED"] [cl-struct-plansys 159 88 0 2 11 47 22560 6815 [cl-struct-fastseedtype 18 159 138 111] "ABILAMA"] [cl-struct-plansys 75 117 5 6 8 44 17600 4939 [cl-struct-fastseedtype 242 75 90 40] "USESRI"] [cl-struct-plansys 112 94 6 0 1 11 1408 4976 [cl-struct-fastseedtype 66 112 42 184] "EDBIRE"] [cl-struct-plansys 237 174 6 4 4 27 6912 3821 [cl-struct-fastseedtype 162 237 26 35] "GEONERBE"] [cl-struct-plansys 179 208 0 6 13 59 47200 3763 [cl-struct-fastseedtype 50 179 74 67] "GEBIISSO"] [cl-struct-plansys 199 189 5 2 6 32 7680 5831 [cl-struct-fastseedtype 146 199 218 75] "MAERZA"] [cl-struct-plansys 0 228 4 4 5 29 11136 4096 [cl-struct-fastseedtype 226 0 234 181] "LAANAT"] [cl-struct-plansys 162 81 3 0 6 28 6272 6818 [cl-struct-fastseedtype 194 162 154 143] "AZAVEAR"] [cl-struct-plansys 202 182 6 2 4 25 4800 4298 [cl-struct-fastseedtype 82 202 10 5] "CECELA"] [cl-struct-plansys 224 85 5 6 5 32 12800 5856 [cl-struct-fastseedtype 50 224 90 187] "ANERINED"] [cl-struct-plansys 56 140 6 0 1 11 1408 4152 [cl-struct-fastseedtype 130 56 170 181] "LAQUUSED"] [cl-struct-plansys 152 104 0 4 9 41 26240 6040 [cl-struct-fastseedtype 226 152 26 124] "TEISEN"] [cl-struct-plansys 140 146 2 6 8 41 26240 6028 [cl-struct-fastseedtype 114 140 202 156] "TEANCE"] [cl-struct-plansys 61 229 5 2 4 24 5760 6461 [cl-struct-fastseedtype 210 61 218 222] "RILERI"] [cl-struct-plansys 255 30 6 4 6 35 8960 6911 [cl-struct-fastseedtype 34 255 106 31] "ONISANED"] [cl-struct-plansys 184 91 3 0 4 20 4480 3000 [cl-struct-fastseedtype 2 184 154 80] "ERZAIN"] [cl-struct-plansys 33 108 4 2 5 27 7776 3361 [cl-struct-fastseedtype 146 33 138 242] "ENLAUS"] [cl-struct-plansys 8 149 5 6 5 32 12800 6408 [cl-struct-fastseedtype 114 8 90 30] "RIANXE"] [cl-struct-plansys 188 226 2 0 5 23 5888 5820 [cl-struct-fastseedtype 194 188 42 91] "ANQUEN"] [cl-struct-plansys 104 18 2 4 7 35 17920 4200 [cl-struct-fastseedtype 34 104 26 117] "LAAINAT"] [cl-struct-plansys 49 204 4 6 7 39 18720 6449 [cl-struct-fastseedtype 178 49 74 238] "REININUS"] [cl-struct-plansys 232 12 4 2 4 23 6624 3304 [cl-struct-fastseedtype 18 232 218 225] "LEVEOR"] [cl-struct-plansys 89 160 0 4 10 45 28800 2905 [cl-struct-fastseedtype 98 89 234 208] "ERMALA"] [cl-struct-plansys 145 245 7 0 1 12 1152 3217 [cl-struct-fastseedtype 66 145 154 81] "ATEDANZA"] [cl-struct-plansys 228 187 3 2 5 26 8736 5092 [cl-struct-fastseedtype 210 228 10 120] "EDTIRA"] [cl-struct-plansys 4 116 4 6 6 35 16800 3076 [cl-struct-fastseedtype 178 4 90 145] "ATARIS"] [cl-struct-plansys 61 160 2 0 6 27 6912 4925 [cl-struct-fastseedtype 2 61 170 232] "USBIINAN"] [cl-struct-plansys 155 236 4 4 8 41 15744 6555 [cl-struct-fastseedtype 98 155 26 78] "RELAON"] [cl-struct-plansys 226 191 7 6 5 34 8160 4834 [cl-struct-fastseedtype 242 226 202 119] "TIORRA"] [cl-struct-plansys 6 116 4 2 6 31 8928 3846 [cl-struct-fastseedtype 82 6 218 148] "RAAAR"] [cl-struct-plansys 79 170 2 4 10 47 24064 5455 [cl-struct-fastseedtype 162 79 106 10] "ARCEONRE"] [cl-struct-plansys 110 95 7 0 2 16 1536 3438 [cl-struct-fastseedtype 130 110 154 210] "ENLE"] [cl-struct-plansys 84 225 1 2 7 32 13824 4180 [cl-struct-fastseedtype 18 84 138 213] "LAARRARA"] [cl-struct-plansys 20 52 4 6 6 35 16800 3860 [cl-struct-fastseedtype 242 20 90 84] "RAISSO"] [cl-struct-plansys 249 7 7 0 1 12 1152 6649 [cl-struct-fastseedtype 66 249 42 158] "RIRILA"] [cl-struct-plansys 114 53 5 4 6 34 10880 4722 [cl-struct-fastseedtype 162 114 26 71] "SOAES"] [cl-struct-plansys 224 169 1 6 9 44 31680 5344 [cl-struct-fastseedtype 50 224 74 121] "ORXETEED"] [cl-struct-plansys 216 92 4 2 4 23 6624 4824 [cl-struct-fastseedtype 146 216 218 55] "TIINISLE"] [cl-struct-plansys 33 125 5 4 5 30 9600 5665 [cl-struct-fastseedtype 226 33 234 11] "MAMAED"] [cl-struct-plansys 143 216 2 0 8 35 8960 3727 [cl-struct-fastseedtype 194 143 154 19] "BETEONA"] [cl-struct-plansys 175 31 7 2 4 26 3744 5807 [cl-struct-fastseedtype 82 175 10 75] "MAORRA"] [cl-struct-plansys 121 20 4 6 7 39 18720 4729 [cl-struct-fastseedtype 50 121 90 167] "SORALAAT"] [cl-struct-plansys 49 85 7 0 1 12 1152 5681 [cl-struct-fastseedtype 130 49 170 187] "ANXEADI"] [cl-struct-plansys 45 47 7 4 3 24 4608 2861 [cl-struct-fastseedtype 226 45 26 160] "ISMA"] [cl-struct-plansys 105 203 3 6 8 42 23520 3433 [cl-struct-fastseedtype 114 105 202 50] "ENSOZA"] [cl-struct-plansys 158 4 4 2 6 31 8928 5534 [cl-struct-fastseedtype 210 158 218 10] "ARDITI"] [cl-struct-plansys 16 87 7 4 2 20 3840 4112 [cl-struct-fastseedtype 34 16 106 21] "LAISXELA"] [cl-struct-plansys 53 162 2 0 6 27 6912 3893 [cl-struct-fastseedtype 2 53 154 84] "RAINLA"] [cl-struct-plansys 54 181 5 2 5 28 6720 4918 [cl-struct-fastseedtype 146 54 138 24] "EDZARAAT"] [cl-struct-plansys 113 84 4 6 7 39 18720 5489 [cl-struct-fastseedtype 114 113 90 202] "ARAMA"] [cl-struct-plansys 37 203 3 0 5 24 5376 3109 [cl-struct-fastseedtype 194 37 42 129] "LEENOR"] [cl-struct-plansys 13 25 1 4 9 42 24192 5133 [cl-struct-fastseedtype 34 13 26 153] "ORONCERI"] [cl-struct-plansys 190 101 5 6 7 40 16000 4030 [cl-struct-fastseedtype 178 190 74 228] "ZAUSMAOR"] [cl-struct-plansys 153 171 3 2 6 30 10080 6297 [cl-struct-fastseedtype 18 153 218 77] "DIENORQU"] [cl-struct-plansys 90 121 1 4 10 46 26496 4442 [cl-struct-fastseedtype 98 90 234 102] "BIANTE"] [cl-struct-plansys 158 252 6 0 3 19 2432 4254 [cl-struct-fastseedtype 66 158 154 213] "LAERZAES"] [cl-struct-plansys 41 228 4 2 5 27 7776 6441 [cl-struct-fastseedtype 210 41 10 126] "RIMABE"] [cl-struct-plansys 61 51 3 6 8 42 23520 6205 [cl-struct-fastseedtype 178 61 90 253] "ISREESVE"] [cl-struct-plansys 22 169 3 0 6 28 6272 6422 [cl-struct-fastseedtype 2 22 170 46] "REREBEER"] [cl-struct-plansys 80 51 3 4 6 32 14336 3408 [cl-struct-fastseedtype 98 80 26 114] "ENLAED"] [cl-struct-plansys 31 184 0 6 13 59 47200 6175 [cl-struct-fastseedtype 242 31 202 205] "DICEBE"] [cl-struct-plansys 7 147 3 2 8 38 12768 2823 [cl-struct-fastseedtype 82 7 218 64] "ANGE"] [cl-struct-plansys 64 35 3 4 6 32 14336 2880 [cl-struct-fastseedtype 162 64 106 64] "CEBIMA"] [cl-struct-plansys 11 38 6 0 4 23 2944 4363 [cl-struct-fastseedtype 130 11 154 214] "VEUSAR"] [cl-struct-plansys 201 234 2 2 7 33 12672 5833 [cl-struct-fastseedtype 18 201 138 187] "ANREBEIS"] [cl-struct-plansys 29 243 3 6 8 42 23520 2845 [cl-struct-fastseedtype 242 29 90 128] "ATER"] [cl-struct-plansys 66 48 2 0 7 31 7936 3906 [cl-struct-fastseedtype 66 66 42 4] "ZAVETE"] [cl-struct-plansys 55 188 4 4 8 41 15744 5687 [cl-struct-fastseedtype 162 55 26 107] "MAONXEDI"] [cl-struct-plansys 205 2 2 6 9 45 28800 6861 [cl-struct-fastseedtype 50 205 74 47] "ARIANES"] [cl-struct-plansys 41 251 3 2 6 30 10080 3625 [cl-struct-fastseedtype 146 41 218 35] "GEUSVEXE"] [cl-struct-plansys 2 150 6 4 5 31 7936 3074 [cl-struct-fastseedtype 226 2 234 225] "LEANON"] [cl-struct-plansys 188 95 7 0 0 8 768 4796 [cl-struct-fastseedtype 194 188 154 151] "TIRAUSRA"] [cl-struct-plansys 84 8 0 2 8 35 16800 3156 [cl-struct-fastseedtype 82 84 10 17] "ATDIBE"] [cl-struct-plansys 82 211 3 6 9 46 25760 3666 [cl-struct-fastseedtype 50 82 90 147] "BEEDRIAR"] [cl-struct-plansys 234 158 6 0 3 19 2432 3306 [cl-struct-fastseedtype 130 234 170 65] "LEARVEXE"] [cl-struct-plansys 2 246 6 4 5 31 7936 3842 [cl-struct-fastseedtype 226 2 26 196] "ZAISZA"] [cl-struct-plansys 6 132 4 6 8 43 20640 4870 [cl-struct-fastseedtype 114 6 202 72] "USBEGE"] [cl-struct-plansys 63 35 3 2 8 38 12768 4415 [cl-struct-fastseedtype 210 63 218 54] "VEORER"] [cl-struct-plansys 225 16 0 4 10 45 28800 5857 [cl-struct-fastseedtype 34 225 106 139] "MAISESEN"] [cl-struct-plansys 242 233 3 0 6 28 6272 5106 [cl-struct-fastseedtype 2 242 154 88] "EDRARI"] [cl-struct-plansys 11 126 6 2 5 29 5568 6411 [cl-struct-fastseedtype 146 11 138 190] "RIUSBEQU"] [cl-struct-plansys 26 19 3 6 9 46 25760 4378 [cl-struct-fastseedtype 114 26 90 118] "VEGERA"] [cl-struct-plansys 78 52 6 0 3 19 2432 4686 [cl-struct-fastseedtype 194 78 42 39] "SOAR"] [cl-struct-plansys 242 32 0 4 11 49 31360 6386 [cl-struct-fastseedtype 34 242 26 189] "ISARIMA"] [cl-struct-plansys 11 126 6 6 7 41 13120 5387 [cl-struct-fastseedtype 178 11 74 90] "QUZAARAR"] [cl-struct-plansys 138 74 2 2 8 37 14208 5258 [cl-struct-fastseedtype 18 138 218 185] "ORREENAN"] [cl-struct-plansys 27 210 2 4 10 47 24064 5915 [cl-struct-fastseedtype 98 27 234 124] "TEMAGE"] [cl-struct-plansys 235 3 3 0 7 32 7168 5355 [cl-struct-fastseedtype 66 235 154 89] "ORUSDIRE"] [cl-struct-plansys 46 141 5 2 5 28 6720 3886 [cl-struct-fastseedtype 210 46 10 4] "ZAONEN"] [cl-struct-plansys 182 242 2 6 10 49 31360 5302 [cl-struct-fastseedtype 178 182 90 105] "ESENENA"] [cl-struct-plansys 175 50 2 0 8 35 8960 4015 [cl-struct-fastseedtype 2 175 170 244] "RAVEQUCE"] [cl-struct-plansys 69 122 2 4 8 39 19968 4421 [cl-struct-fastseedtype 98 69 26 150] "VELAAT"] [cl-struct-plansys 28 49 1 6 9 44 31680 3612 [cl-struct-fastseedtype 242 28 202 163] "GEATEN"] [cl-struct-plansys 72 178 2 2 6 29 11136 5960 [cl-struct-fastseedtype 82 72 218 236] "INSOTE"] [cl-struct-plansys 241 28 4 4 6 33 12672 4593 [cl-struct-fastseedtype 162 241 106 246] "VECEDIUS"] [cl-struct-plansys 232 237 7 0 0 8 768 5608 [cl-struct-fastseedtype 130 232 154 218] "QUERBE"] [cl-struct-plansys 254 115 3 2 7 34 11424 3326 [cl-struct-fastseedtype 18 254 138 33] "LEENENBI"] [cl-struct-plansys 102 178 2 6 10 49 31360 5990 [cl-struct-fastseedtype 242 102 90 172] "INCEOR"] [cl-struct-plansys 75 217 3 0 7 32 7168 5451 [cl-struct-fastseedtype 66 75 42 234] "ARREGE"] [cl-struct-plansys 60 67 3 4 6 32 14336 6716 [cl-struct-fastseedtype 162 60 26 143] "AAANQU"] [cl-struct-plansys 122 219 3 6 9 46 25760 4218 [cl-struct-fastseedtype 50 122 74 101] "CEQUQUQU"] [cl-struct-plansys 186 154 2 2 8 37 14208 6842 [cl-struct-fastseedtype 146 186 218 15] "AZAAGE"] [cl-struct-plansys 163 47 7 4 5 32 6144 4771 [cl-struct-fastseedtype 226 163 234 55] "TIMABI"] [cl-struct-plansys 41 230 6 0 2 15 1920 5673 [cl-struct-fastseedtype 194 41 154 27] "ANINATOR"] [cl-struct-plansys 185 113 1 2 8 36 15552 4793 [cl-struct-fastseedtype 82 185 10 87] "TILEEN"] [cl-struct-plansys 107 146 2 6 11 53 33920 6763 [cl-struct-fastseedtype 50 107 90 127] "ONTESOGE"] [cl-struct-plansys 99 103 7 0 3 20 1920 4707 [cl-struct-fastseedtype 130 99 170 71] "SOENISTI"] [cl-struct-plansys 23 189 5 4 7 38 12160 4887 [cl-struct-fastseedtype 226 23 26 232] "USISIS"] [cl-struct-plansys 99 189 5 6 8 44 17600 6499 [cl-struct-fastseedtype 114 99 202 222] "RIONXE"] [cl-struct-plansys 32 66 2 2 6 29 11136 3360 [cl-struct-fastseedtype 210 32 218 98] "XECEES"] [cl-struct-plansys 114 73 1 4 10 46 26496 3186 [cl-struct-fastseedtype 34 114 106 129] "LEISERA"] [cl-struct-plansys 239 48 2 0 8 35 8960 6127 [cl-struct-fastseedtype 2 239 154 92] "TETESO"] [cl-struct-plansys 160 199 7 2 1 14 2016 4000 [cl-struct-fastseedtype 146 160 138 228] "ZAINENGE"] [cl-struct-plansys 3 210 2 6 11 53 33920 3331 [cl-struct-fastseedtype 114 3 90 34] "XETIIS"] [cl-struct-plansys 55 29 7 0 3 20 1920 6199 [cl-struct-fastseedtype 194 55 42 77] "DIXESO"] [cl-struct-plansys 23 39 7 4 5 32 6144 3095 [cl-struct-fastseedtype 34 23 26 225] "LEONTIED"] [cl-struct-plansys 24 23 7 6 3 26 6240 2840 [cl-struct-fastseedtype 178 24 74 80] "ERESAN"] [cl-struct-plansys 187 233 1 2 10 44 19008 4283 [cl-struct-fastseedtype 18 187 218 37] "CEARMATE"] [cl-struct-plansys 156 171 3 4 6 32 14336 3484 [cl-struct-fastseedtype 98 156 234 18] "ENANAR"]] [[cl-struct-plansys 32 165 7 0 0 8 768 5664 [cl-struct-fastseedtype 132 32 53 123] "ANLEIS"] [cl-struct-plansys 153 221 5 5 6 35 12600 5017 [cl-struct-fastseedtype 168 153 27 184] "EDRE"] [cl-struct-plansys 103 128 0 2 11 47 22560 4711 [cl-struct-fastseedtype 144 103 5 23] "TIONIS"] [cl-struct-plansys 147 125 5 7 9 49 21560 2963 [cl-struct-fastseedtype 252 147 243 16] "ERXESOAN"] [cl-struct-plansys 251 199 7 1 4 25 3000 4347 [cl-struct-fastseedtype 76 251 165 229] "CEUSES"] [cl-struct-plansys 23 137 3 0 7 32 7168 5143 [cl-struct-fastseedtype 192 23 219 73] "ESMAIN"] [cl-struct-plansys 129 76 4 7 8 44 23232 5249 [cl-struct-fastseedtype 184 129 149 185] "ORUSGE"] [cl-struct-plansys 4 195 3 6 7 38 21280 2820 [cl-struct-fastseedtype 244 4 83 80] "EREDDIVE"] [cl-struct-plansys 214 109 5 2 5 28 6720 6358 [cl-struct-fastseedtype 212 214 85 93] "ISXEVELA"] [cl-struct-plansys 213 214 6 3 4 26 5824 6357 [cl-struct-fastseedtype 152 213 219 77] "DIORIS"] [cl-struct-plansys 100 29 5 4 4 26 8320 5732 [cl-struct-fastseedtype 160 100 101 123] "ANISIS"] [cl-struct-plansys 85 221 5 5 6 35 12600 6741 [cl-struct-fastseedtype 172 85 243 47] "AXEUS"] [cl-struct-plansys 123 232 0 3 12 52 29120 4475 [cl-struct-fastseedtype 28 123 69 198] "BIESMAAN"] [cl-struct-plansys 91 107 3 6 10 50 28000 2907 [cl-struct-fastseedtype 48 91 27 240] "ERVELE"] [cl-struct-plansys 168 1 3 1 5 25 7000 2984 [cl-struct-fastseedtype 72 168 117 96] "ARBE"] [cl-struct-plansys 126 19 3 4 8 40 17920 5758 [cl-struct-fastseedtype 36 126 211 155] "ANDISO"] [cl-struct-plansys 16 71 7 4 2 20 3840 3856 [cl-struct-fastseedtype 36 16 117 132] "ZAANER"] [cl-struct-plansys 208 112 2 1 6 28 8960 6096 [cl-struct-fastseedtype 136 208 155 28] "TEISARZA"] [cl-struct-plansys 196 74 2 6 8 41 26240 6084 [cl-struct-fastseedtype 176 196 197 108] "INTEDI"] [cl-struct-plansys 150 173 5 3 6 33 9240 6806 [cl-struct-fastseedtype 92 150 243 191] "ONGEQU"] [cl-struct-plansys 29 90 2 5 9 44 25344 5661 [cl-struct-fastseedtype 236 29 229 123] "ANBEDI"] [cl-struct-plansys 253 141 5 4 5 30 9600 6909 [cl-struct-fastseedtype 160 253 91 127] "ONARSOAT"] [cl-struct-plansys 17 135 7 3 3 23 3864 3857 [cl-struct-fastseedtype 216 17 85 164] "ZAISRA"] [cl-struct-plansys 214 244 4 2 6 31 8928 5078 [cl-struct-fastseedtype 84 214 83 8] "USENAT"] [cl-struct-plansys 140 177 1 6 9 44 31680 2956 [cl-struct-fastseedtype 116 140 149 16] "ERREMAAR"] [cl-struct-plansys 74 235 3 7 10 51 31416 3914 [cl-struct-fastseedtype 120 74 91 132] "ZAQUESSO"] [cl-struct-plansys 199 137 3 0 7 32 7168 5831 [cl-struct-fastseedtype 192 199 37 11] "MATERE"] [cl-struct-plansys 150 46 6 1 4 24 3840 2966 [cl-struct-fastseedtype 12 150 243 32] "CETEGE"] [cl-struct-plansys 163 156 4 7 10 52 27456 4515 [cl-struct-fastseedtype 188 163 133 38] "BISOATON"] [cl-struct-plansys 191 49 1 2 10 44 19008 4799 [cl-struct-fastseedtype 16 191 155 87] "TIUSRIRI"] [cl-struct-plansys 254 94 6 5 6 36 10368 4350 [cl-struct-fastseedtype 104 254 53 165] "CEXECE"] [cl-struct-plansys 77 165 7 0 1 12 1152 4173 [cl-struct-fastseedtype 132 77 211 245] "LAESINMA"] [cl-struct-plansys 11 44 6 0 4 23 2944 3083 [cl-struct-fastseedtype 196 11 181 33] "LEQUSO"] [cl-struct-plansys 3 135 7 5 6 37 7992 4099 [cl-struct-fastseedtype 104 3 27 229] "CEERED"] [cl-struct-plansys 173 88 0 2 9 39 18720 4525 [cl-struct-fastseedtype 208 173 133 118] "VETELE"] [cl-struct-plansys 148 161 1 7 10 49 38808 3476 [cl-struct-fastseedtype 188 148 243 178] "ENUSERIN"] [cl-struct-plansys 204 48 2 1 6 28 8960 4556 [cl-struct-fastseedtype 140 204 37 230] "BIGEVE"] [cl-struct-plansys 95 150 6 0 4 23 2944 4959 [cl-struct-fastseedtype 128 95 219 216] "EDREBI"] [cl-struct-plansys 173 6 6 7 6 38 13376 3757 [cl-struct-fastseedtype 248 173 21 131] "GETIUS"] [cl-struct-plansys 35 104 0 6 13 59 47200 3875 [cl-struct-fastseedtype 180 35 83 196] "ZAEREDRE"] [cl-struct-plansys 78 56 0 2 10 43 20640 4686 [cl-struct-fastseedtype 20 78 213 215] "TIRIZAAN"] [cl-struct-plansys 187 132 4 3 8 40 13440 6587 [cl-struct-fastseedtype 88 187 219 158] "RIRIOR"] [cl-struct-plansys 182 57 1 4 10 46 26496 6582 [cl-struct-fastseedtype 224 182 229 206] "RERIZA"] [cl-struct-plansys 210 68 4 5 8 42 18144 4306 [cl-struct-fastseedtype 108 210 243 213] "LADILA"] [cl-struct-plansys 88 149 5 3 4 25 7000 5464 [cl-struct-fastseedtype 92 88 197 218] "QUUSANRI"] [cl-struct-plansys 158 251 3 6 9 46 25760 3742 [cl-struct-fastseedtype 240 158 27 99] "GEISON"] [cl-struct-plansys 96 255 7 1 1 13 1560 6240 [cl-struct-fastseedtype 136 96 245 93] "ISRITE"] [cl-struct-plansys 152 124 4 4 5 29 11136 3736 [cl-struct-fastseedtype 228 152 211 211] "BEESVE"] [cl-struct-plansys 19 86 6 4 6 35 8960 3347 [cl-struct-fastseedtype 100 19 245 82] "ENTEXE"] [cl-struct-plansys 50 34 2 1 8 36 11520 3122 [cl-struct-fastseedtype 72 50 155 17] "ATBIARXE"] [cl-struct-plansys 34 170 2 6 10 49 31360 3874 [cl-struct-fastseedtype 240 34 69 52] "RALEOR"] [cl-struct-plansys 143 89 1 3 11 49 24696 5263 [cl-struct-fastseedtype 28 143 243 233] "ESENMA"] [cl-struct-plansys 7 75 3 5 10 49 24696 3847 [cl-struct-fastseedtype 44 7 101 36] "ZAVEXE"] [cl-struct-plansys 61 162 2 4 8 39 19968 4413 [cl-struct-fastseedtype 96 61 91 86] "VEVEESVE"] [cl-struct-plansys 86 201 1 3 10 45 22680 4182 [cl-struct-fastseedtype 24 86 213 85] "LALALE"] [cl-struct-plansys 237 33 1 2 8 36 15552 4077 [cl-struct-fastseedtype 20 237 83 132] "ZAENZA"] [cl-struct-plansys 27 4 4 6 9 47 22560 3611 [cl-struct-fastseedtype 180 27 21 179] "BEBELEUS"] [cl-struct-plansys 40 161 1 7 10 49 38808 6184 [cl-struct-fastseedtype 56 40 91 157] "ISSODIEN"] [cl-struct-plansys 50 45 7 0 2 16 1536 4402 [cl-struct-fastseedtype 0 50 165 198] "BICERI"] [cl-struct-plansys 10 30 6 1 4 24 3840 6410 [cl-struct-fastseedtype 204 10 243 78] "REEDATED"] [cl-struct-plansys 152 209 1 7 10 49 38808 3736 [cl-struct-fastseedtype 252 152 5 227] "GEDIESQU"] [cl-struct-plansys 250 201 1 2 9 40 17280 3578 [cl-struct-fastseedtype 208 250 155 18] "ENTIZARE"] [cl-struct-plansys 206 228 4 5 8 42 18144 5582 [cl-struct-fastseedtype 168 206 181 138] "ARRITI"] [cl-struct-plansys 96 151 7 0 0 8 768 4192 [cl-struct-fastseedtype 68 96 211 53] "LADIGESO"] [cl-struct-plansys 39 195 3 0 7 32 7168 4903 [cl-struct-fastseedtype 4 39 53 24] "EDXEXE"] [cl-struct-plansys 93 66 2 5 9 44 25344 3421 [cl-struct-fastseedtype 40 93 27 162] "XELE"] [cl-struct-plansys 36 64 0 2 8 35 16800 4388 [cl-struct-fastseedtype 16 36 5 166] "BIARRA"] [cl-struct-plansys 133 212 4 7 8 44 23232 3973 [cl-struct-fastseedtype 124 133 243 100] "ZAONESDI"] [cl-struct-plansys 205 169 3 1 6 29 8120 4557 [cl-struct-fastseedtype 204 205 165 54] "VEREEN"] [cl-struct-plansys 151 178 2 0 8 35 8960 4759 [cl-struct-fastseedtype 64 151 219 247] "TILEER"] [cl-struct-plansys 10 208 0 7 13 60 52800 5898 [cl-struct-fastseedtype 56 10 149 28] "TETIRI"] [cl-struct-plansys 50 30 6 6 6 37 11840 4914 [cl-struct-fastseedtype 116 50 83 72] "USORBELA"] [cl-struct-plansys 245 19 3 2 6 30 10080 3573 [cl-struct-fastseedtype 84 245 85 162] "XEMAGEAT"] [cl-struct-plansys 145 67 3 3 7 35 13720 6801 [cl-struct-fastseedtype 24 145 219 127] "ONRACE"] [cl-struct-plansys 57 101 5 4 5 30 9600 3385 [cl-struct-fastseedtype 32 57 101 242] "ENERTE"] [cl-struct-plansys 63 188 4 5 9 46 19872 5695 [cl-struct-fastseedtype 44 63 243 139] "MASOEN"] [cl-struct-plansys 101 82 2 3 8 38 17024 6757 [cl-struct-fastseedtype 156 101 69 63] "ONTITEEN"] [cl-struct-plansys 210 156 4 6 8 43 20640 4562 [cl-struct-fastseedtype 176 210 27 102] "BIRADI"] [cl-struct-plansys 72 13 7 1 1 13 1560 5704 [cl-struct-fastseedtype 200 72 117 43] "MALEVE"] [cl-struct-plansys 163 246 6 4 6 35 8960 5795 [cl-struct-fastseedtype 164 163 211 27] "ANLALA"] [cl-struct-plansys 70 117 5 4 6 34 10880 3142 [cl-struct-fastseedtype 164 70 117 113] "ATDICE"] [cl-struct-plansys 132 229 7 1 1 13 1560 4484 [cl-struct-fastseedtype 8 132 155 150] "VEONQUAT"] [cl-struct-plansys 177 26 2 6 9 45 28800 5809 [cl-struct-fastseedtype 48 177 197 203] "MATIRA"] [cl-struct-plansys 119 20 4 3 8 40 13440 3703 [cl-struct-fastseedtype 220 119 243 35] "GEERIN"] [cl-struct-plansys 32 76 4 5 6 34 14688 5920 [cl-struct-fastseedtype 108 32 229 28] "TEESBI"] [cl-struct-plansys 109 198 6 4 4 27 6912 6253 [cl-struct-fastseedtype 32 109 91 189] "ISATANAR"] [cl-struct-plansys 202 27 3 3 8 39 15288 4810 [cl-struct-fastseedtype 88 202 85 215] "TIISON"] [cl-struct-plansys 243 95 7 2 4 26 3744 3059 [cl-struct-fastseedtype 212 243 83 16] "ERGESO"] [cl-struct-plansys 218 103 7 6 5 34 8160 4314 [cl-struct-fastseedtype 244 218 149 165] "CESOUSLA"] [cl-struct-plansys 245 104 0 7 12 56 49280 4597 [cl-struct-fastseedtype 248 245 91 70] "BIZALEIN"] [cl-struct-plansys 204 225 3 0 4 20 4480 3532 [cl-struct-fastseedtype 64 204 37 82] "ENONIS"] [cl-struct-plansys 111 29 7 1 4 25 3000 5999 [cl-struct-fastseedtype 140 111 243 140] "INQUVERI"] [cl-struct-plansys 190 22 6 7 7 42 14784 6846 [cl-struct-fastseedtype 60 190 133 239] "AZAENBI"] [cl-struct-plansys 38 114 2 2 8 37 14208 6182 [cl-struct-fastseedtype 144 38 155 93] "ISVEQURE"] [cl-struct-plansys 206 122 2 5 10 48 27648 3022 [cl-struct-fastseedtype 232 206 53 64] "ESED"] [cl-struct-plansys 99 152 2 0 8 35 8960 4195 [cl-struct-fastseedtype 4 99 211 133] "CELEAREN"] [cl-struct-plansys 114 106 2 0 7 31 7936 6514 [cl-struct-fastseedtype 68 114 181 94] "RIANIN"] [cl-struct-plansys 166 12 4 5 8 42 18144 6822 [cl-struct-fastseedtype 232 166 27 239] "AXEED"] [cl-struct-plansys 202 56 0 2 10 43 20640 4298 [cl-struct-fastseedtype 80 202 133 165] "CEUSED"] [cl-struct-plansys 102 24 0 7 13 60 52800 4454 [cl-struct-fastseedtype 60 102 243 38] "BICEENON"] [cl-struct-plansys 255 50 2 1 9 40 12800 4863 [cl-struct-fastseedtype 12 255 37 215] "TIUSON"] [cl-struct-plansys 191 223 7 0 3 20 1920 4543 [cl-struct-fastseedtype 0 191 219 166] "BIZAAR"] [cl-struct-plansys 150 170 2 7 11 54 38016 4502 [cl-struct-fastseedtype 120 150 21 134] "BISOGE"] [cl-struct-plansys 49 227 3 6 8 42 23520 5937 [cl-struct-fastseedtype 52 49 83 220] "TEATRIIN"] [cl-struct-plansys 204 254 6 2 2 17 3264 6092 [cl-struct-fastseedtype 148 204 213 188] "TEUSATVE"] [cl-struct-plansys 86 17 1 3 10 45 22680 2902 [cl-struct-fastseedtype 216 86 219 240] "ERORLE"] [cl-struct-plansys 235 161 1 4 11 50 28800 4331 [cl-struct-fastseedtype 96 235 229 229] "CEENGE"] [cl-struct-plansys 155 67 3 5 10 49 24696 3227 [cl-struct-fastseedtype 236 155 243 81] "ATATON"] [cl-struct-plansys 162 31 7 3 4 27 4536 3746 [cl-struct-fastseedtype 220 162 197 243] "BEVEINVE"] [cl-struct-plansys 246 76 4 6 8 43 20640 5366 [cl-struct-fastseedtype 112 246 27 249] "ORTEMA"] [cl-struct-plansys 97 43 3 1 6 29 8120 4961 [cl-struct-fastseedtype 8 97 245 200] "USLAON"] [cl-struct-plansys 158 127 7 4 4 28 5376 3742 [cl-struct-fastseedtype 100 158 211 115] "BEATZA"] [cl-struct-plansys 169 164 4 4 6 33 12672 6825 [cl-struct-fastseedtype 228 169 245 223] "ONDITI"] [cl-struct-plansys 197 183 7 1 2 17 2040 5829 [cl-struct-fastseedtype 200 197 155 171] "MAESQUA"] [cl-struct-plansys 111 154 2 6 11 53 33920 3695 [cl-struct-fastseedtype 112 111 69 51] "BEIS"] [cl-struct-plansys 80 224 0 3 9 40 22400 6224 [cl-struct-fastseedtype 156 80 243 109] "DIRIIS"] [cl-struct-plansys 105 93 5 5 6 35 12600 4201 [cl-struct-fastseedtype 172 105 101 101] "CEINAN"] [cl-struct-plansys 140 251 3 4 6 32 14336 3980 [cl-struct-fastseedtype 224 140 91 180] "RATEISRE"] [cl-struct-plansys 110 125 5 3 6 33 9240 4974 [cl-struct-fastseedtype 152 110 213 40] "USLAIN"] [cl-struct-plansys 234 172 4 2 6 31 8928 6122 [cl-struct-fastseedtype 148 234 83 172] "INGEQU"] [cl-struct-plansys 202 218 2 6 10 49 31360 5066 [cl-struct-fastseedtype 52 202 21 232] "USINRIBE"] [cl-struct-plansys 179 62 6 7 8 46 16192 6835 [cl-struct-fastseedtype 184 179 91 127] "ONENCEVE"] [cl-struct-plansys 150 165 7 0 2 16 1536 6294 [cl-struct-fastseedtype 128 150 165 173] "DIESDI"] [cl-struct-plansys 196 45 7 1 1 13 1560 5572 [cl-struct-fastseedtype 76 196 243 218] "QUINMABE"] [cl-struct-plansys 19 107 3 7 11 55 33880 5907 [cl-struct-fastseedtype 124 19 5 76] "INMAARXE"] [cl-struct-plansys 66 42 2 2 8 37 14208 4930 [cl-struct-fastseedtype 80 66 155 56] "EDCEON"] [cl-struct-plansys 255 32 0 5 13 58 41760 4351 [cl-struct-fastseedtype 40 255 181 197] "CECEAR"] [cl-struct-plansys 85 170 2 0 6 27 6912 4181 [cl-struct-fastseedtype 196 85 211 229] "CEBILEDI"] [cl-struct-plansys 237 33 3 0 5 24 5376 4333 [cl-struct-fastseedtype 132 237 53 245] "LAZASO"] [cl-struct-plansys 224 231 7 5 3 25 5400 6112 [cl-struct-fastseedtype 168 224 27 204] "INBE"] [cl-struct-plansys 160 64 0 2 8 35 16800 4256 [cl-struct-fastseedtype 144 160 5 117] "LAVEMA"] [cl-struct-plansys 54 107 3 7 10 51 31416 4918 [cl-struct-fastseedtype 252 54 243 248] "EDANMA"] [cl-struct-plansys 96 203 3 1 5 25 7000 4704 [cl-struct-fastseedtype 76 96 165 199] "SOBEAN"] [cl-struct-plansys 214 27 3 0 6 28 6272 4310 [cl-struct-fastseedtype 192 214 219 229] "CEEDRA"] [cl-struct-plansys 82 148 4 7 9 48 25344 6738 [cl-struct-fastseedtype 184 82 149 191] "ONSOOR"] [cl-struct-plansys 31 185 1 6 12 56 40320 2847 [cl-struct-fastseedtype 244 31 83 128] "ORORBE"] [cl-struct-plansys 211 249 1 2 10 44 19008 4819 [cl-struct-fastseedtype 212 211 85 39] "SOLAERIN"] [cl-struct-plansys 12 240 0 3 9 40 22400 3084 [cl-struct-fastseedtype 152 12 219 241] "ATREDI"] [cl-struct-plansys 205 237 5 4 5 30 9600 5325 [cl-struct-fastseedtype 160 205 101 169] "ESZAAN"] [cl-struct-plansys 232 219 3 5 7 37 18648 4840 [cl-struct-fastseedtype 172 232 243 39] "SOMATE"] [cl-struct-plansys 16 252 4 3 5 28 9408 4880 [cl-struct-fastseedtype 28 16 69 248] "EDZADIAR"] [cl-struct-plansys 10 13 5 6 7 40 16000 5898 [cl-struct-fastseedtype 48 10 27 28] "TEBEOR"] [cl-struct-plansys 169 89 3 1 6 29 8120 4521 [cl-struct-fastseedtype 72 169 117 54] "VEOROR"] [cl-struct-plansys 137 25 1 4 9 42 24192 5769 [cl-struct-fastseedtype 36 137 211 219] "ANRIGE"] [cl-struct-plansys 61 227 3 4 7 36 16128 6461 [cl-struct-fastseedtype 36 61 117 158] "RIRIQU"] [cl-struct-plansys 247 154 2 1 9 40 12800 3063 [cl-struct-fastseedtype 136 247 155 80] "ERXEARIS"] [cl-struct-plansys 93 42 2 6 9 45 28800 5469 [cl-struct-fastseedtype 176 93 197 106] "ARBEAN"] [cl-struct-plansys 25 187 3 3 7 35 13720 4633 [cl-struct-fastseedtype 92 25 243 199] "SOTERI"] [cl-struct-plansys 226 126 6 5 6 36 10368 6370 [cl-struct-fastseedtype 236 226 229 253] "ISRION"] [cl-struct-plansys 156 63 7 4 2 20 3840 5788 [cl-struct-fastseedtype 160 156 91 59] "ANTIAXE"] [cl-struct-plansys 66 239 7 3 4 27 4536 5442 [cl-struct-fastseedtype 216 66 85 74] "ARTEAR"] [cl-struct-plansys 209 10 2 2 7 33 12672 5073 [cl-struct-fastseedtype 84 209 83 88] "EDBEIS"] [cl-struct-plansys 233 93 5 6 6 36 14400 5609 [cl-struct-fastseedtype 116 233 149 122] "QULECELE"] [cl-struct-plansys 97 37 5 7 7 41 18040 4961 [cl-struct-fastseedtype 120 97 91 72] "USAORER"] [cl-struct-plansys 144 121 3 0 4 20 4480 5264 [cl-struct-fastseedtype 192 144 37 217] "ORGEIN"] [cl-struct-plansys 9 76 6 1 3 20 3200 4873 [cl-struct-fastseedtype 12 9 243 56] "EDREERED"] [cl-struct-plansys 152 208 0 7 11 52 45760 5016 [cl-struct-fastseedtype 188 152 133 248] "EDXEBERE"] [cl-struct-plansys 78 243 3 2 7 34 11424 3662 [cl-struct-fastseedtype 16 78 155 163] "GECEVEON"] [cl-struct-plansys 95 214 6 5 7 40 11520 5727 [cl-struct-fastseedtype 104 95 53 27] "ANATMA"] [cl-struct-plansys 56 203 3 0 4 20 4480 4152 [cl-struct-fastseedtype 132 56 211 85] "LAQUUSED"] [cl-struct-plansys 152 232 2 0 5 23 5888 5784 [cl-struct-fastseedtype 196 152 181 219] "ANISAT"] [cl-struct-plansys 10 209 1 5 11 51 33048 5130 [cl-struct-fastseedtype 104 10 27 57] "ORLAED"] [cl-struct-plansys 166 88 0 2 10 43 20640 4006 [cl-struct-fastseedtype 208 166 133 20] "RABEA"] [cl-struct-plansys 247 207 7 7 7 43 11352 5623 [cl-struct-fastseedtype 188 247 243 218] "QULERAAT"] [cl-struct-plansys 241 116 6 1 3 20 3200 5105 [cl-struct-fastseedtype 140 241 37 8] "USREUS"] [cl-struct-plansys 222 104 2 0 7 31 7936 4062 [cl-struct-fastseedtype 128 222 219 180] "RAANRE"] [cl-struct-plansys 62 142 6 7 7 42 14784 5182 [cl-struct-fastseedtype 248 62 21 201] "ESVERI"] [cl-struct-plansys 254 158 6 6 6 37 11840 4094 [cl-struct-fastseedtype 180 254 83 52] "RAATZAMA"] [cl-struct-plansys 11 4 4 2 7 35 10080 3083 [cl-struct-fastseedtype 20 11 213 225] "LEATRIEN"] [cl-struct-plansys 178 222 6 3 5 30 6720 3506 [cl-struct-fastseedtype 88 178 219 130] "XEBEES"] [cl-struct-plansys 223 73 1 4 11 50 28800 6111 [cl-struct-fastseedtype 224 223 229 60] "TECEXE"] [cl-struct-plansys 37 130 2 5 9 44 25344 6181 [cl-struct-fastseedtype 108 37 243 13] "DIVEES"] [cl-struct-plansys 173 233 1 3 9 41 20664 6061 [cl-struct-fastseedtype 92 173 197 76] "INGEISDI"] [cl-struct-plansys 13 221 5 6 6 36 14400 6669 [cl-struct-fastseedtype 240 13 27 207] "AQUTI"] [cl-struct-plansys 33 151 7 1 2 17 2040 3617 [cl-struct-fastseedtype 136 33 245 115] "BEDIXE"] [cl-struct-plansys 99 194 2 4 10 47 24064 3683 [cl-struct-fastseedtype 228 99 211 83] "BEQUEN"] [cl-struct-plansys 0 50 2 4 7 35 17920 5888 [cl-struct-fastseedtype 100 0 245 172] "INONIN"] [cl-struct-plansys 25 140 6 1 3 20 3200 4121 [cl-struct-fastseedtype 72 25 155 133] "CEMAARAN"] [cl-struct-plansys 123 202 2 6 11 53 33920 3451 [cl-struct-fastseedtype 240 123 69 114] "ENEDSO"] [cl-struct-plansys 210 167 7 3 4 27 4536 3282 [cl-struct-fastseedtype 28 210 243 49] "ATMAA"] [cl-struct-plansys 140 175 7 5 3 25 5400 4492 [cl-struct-fastseedtype 44 140 101 230] "BILERA"] [cl-struct-plansys 156 148 4 4 5 29 11136 3484 [cl-struct-fastseedtype 96 156 91 82] "ENGEATSO"] [cl-struct-plansys 71 113 1 3 11 49 24696 5703 [cl-struct-fastseedtype 24 71 213 59] "ANRATI"] [cl-struct-plansys 168 119 7 2 1 14 2016 4008 [cl-struct-fastseedtype 20 168 83 20] "RABEER"] [cl-struct-plansys 56 240 0 6 10 47 37600 6200 [cl-struct-fastseedtype 180 56 21 93] "ISBIANON"] [cl-struct-plansys 255 27 3 7 11 55 33880 3327 [cl-struct-fastseedtype 56 255 91 161] "LETEISAN"] [cl-struct-plansys 187 93 7 0 3 20 1920 4027 [cl-struct-fastseedtype 0 187 165 212] "RAINTE"] [cl-struct-plansys 61 124 6 1 3 20 3200 4413 [cl-struct-fastseedtype 204 61 243 166] "BILECEDI"] [cl-struct-plansys 77 69 5 7 7 41 18040 4173 [cl-struct-fastseedtype 252 77 5 245] "LAUSMAES"] [cl-struct-plansys 73 203 3 2 6 30 10080 6473 [cl-struct-fastseedtype 208 73 155 158] "RIRATEA"] [cl-struct-plansys 239 156 4 5 9 46 19872 3055 [cl-struct-fastseedtype 168 239 181 64] "DIIS"] [cl-struct-plansys 11 253 7 0 3 20 1920 4107 [cl-struct-fastseedtype 68 11 211 213] "LARIONRA"] [cl-struct-plansys 116 191 7 0 0 8 768 3444 [cl-struct-fastseedtype 4 116 53 18] "ENCEIN"] [cl-struct-plansys 36 204 4 5 6 34 14688 4388 [cl-struct-fastseedtype 40 36 27 54] "VEBI"] [cl-struct-plansys 221 128 0 2 9 39 18720 4061 [cl-struct-fastseedtype 16 221 5 132] "ZALEXE"] [cl-struct-plansys 168 66 2 7 9 46 32384 6056 [cl-struct-fastseedtype 124 168 243 204] "INEDDIEN"] [cl-struct-plansys 178 45 7 1 3 21 2520 5042 [cl-struct-fastseedtype 204 178 165 152] "EDORZA"] [cl-struct-plansys 214 196 6 0 3 19 2432 3798 [cl-struct-fastseedtype 64 214 219 19] "BEREED"] [cl-struct-plansys 91 152 0 7 14 64 56320 3419 [cl-struct-fastseedtype 56 91 149 162] "XEVERA"] [cl-struct-plansys 205 148 4 6 7 39 18720 5069 [cl-struct-fastseedtype 116 205 83 248] "EDQUONEN"] [cl-struct-plansys 114 31 7 2 3 22 3168 6002 [cl-struct-fastseedtype 84 114 85 236] "INRIISUS"] [cl-struct-plansys 72 221 5 3 4 25 7000 3656 [cl-struct-fastseedtype 24 72 219 163] "GEESLA"] [cl-struct-plansys 34 181 5 4 6 34 10880 2850 [cl-struct-fastseedtype 32 34 101 160] "TIQU"] [cl-struct-plansys 82 58 2 5 10 48 27648 3666 [cl-struct-fastseedtype 44 82 243 3] "GEERBI"] [cl-struct-plansys 122 230 6 3 5 30 6720 3194 [cl-struct-fastseedtype 156 122 69 241] "ATENRILE"] [cl-struct-plansys 1 190 6 6 5 33 10560 3329 [cl-struct-fastseedtype 176 1 27 18] "ENATCE"] [cl-struct-plansys 201 229 7 1 2 17 2040 3273 [cl-struct-fastseedtype 200 201 117 129] "LEERTE"] [cl-struct-plansys 46 124 4 4 7 37 14208 5678 [cl-struct-fastseedtype 164 46 211 219] "ANBIAT"] [cl-struct-plansys 243 145 1 4 11 50 28800 5875 [cl-struct-fastseedtype 164 243 117 11] "MAERA"] [cl-struct-plansys 43 143 7 1 4 25 3000 5419 [cl-struct-fastseedtype 8 43 155 74] "ARZAQUAR"] [cl-struct-plansys 202 122 2 6 10 49 31360 5322 [cl-struct-fastseedtype 48 202 197 73] "ESREXE"] [cl-struct-plansys 122 162 2 3 9 42 18816 5754 [cl-struct-fastseedtype 220 122 243 171] "MAESER"] [cl-struct-plansys 101 240 0 5 11 50 36000 6501 [cl-struct-fastseedtype 108 101 229 30] "RIRAED"] [cl-struct-plansys 140 248 0 4 9 41 26240 5260 [cl-struct-fastseedtype 32 140 91 249] "ORRIGEAN"] [cl-struct-plansys 123 3 3 3 9 43 16856 6267 [cl-struct-fastseedtype 88 123 85 253] "ISTELA"] [cl-struct-plansys 110 245 5 2 5 28 6720 2926 [cl-struct-fastseedtype 212 110 83 224] "ZABE"] [cl-struct-plansys 183 147 3 6 10 50 28000 6839 [cl-struct-fastseedtype 244 183 149 143] "AQUXEIN"] [cl-struct-plansys 140 34 2 7 9 46 32384 5516 [cl-struct-fastseedtype 248 140 91 138] "ARORATLA"] [cl-struct-plansys 21 81 3 0 5 24 5376 2837 [cl-struct-fastseedtype 64 21 37 160] "BIAN"] [cl-struct-plansys 98 187 3 1 7 33 9240 3938 [cl-struct-fastseedtype 140 98 243 36] "ZAGEARBE"] [cl-struct-plansys 51 202 2 7 12 58 40832 3123 [cl-struct-fastseedtype 60 51 133 65] "LEONRALA"] [cl-struct-plansys 53 180 4 2 5 27 7776 5173 [cl-struct-fastseedtype 144 53 155 41] "ESBEENA"] [cl-struct-plansys 175 114 2 5 11 52 29952 4527 [cl-struct-fastseedtype 232 175 53 54] "VEEDRI"] [cl-struct-plansys 206 62 6 0 3 19 2432 4302 [cl-struct-fastseedtype 4 206 211 101] "CEENBION"] [cl-struct-plansys 127 166 6 0 4 23 2944 4991 [cl-struct-fastseedtype 68 127 181 152] "EDRIVE"] [cl-struct-plansys 45 214 6 5 5 32 9216 3629 [cl-struct-fastseedtype 232 45 27 195] "GESOED"] [cl-struct-plansys 67 184 0 2 11 47 22560 3651 [cl-struct-fastseedtype 80 67 133 195] "GEONBI"] [cl-struct-plansys 73 198 6 7 6 38 13376 6473 [cl-struct-fastseedtype 60 73 243 206] "RERIVEZA"] [cl-struct-plansys 164 246 6 1 2 16 2560 5284 [cl-struct-fastseedtype 12 164 37 121] "ORBEAT"] [cl-struct-plansys 190 49 3 0 6 28 6272 3518 [cl-struct-fastseedtype 0 190 219 2] "XEATEN"] [cl-struct-plansys 167 178 2 7 12 58 40832 6055 [cl-struct-fastseedtype 120 167 21 76] "INBIOR"] [cl-struct-plansys 140 153 1 6 9 44 31680 6028 [cl-struct-fastseedtype 52 140 83 204] "INENARES"] [cl-struct-plansys 9 74 2 2 7 33 12672 4361 [cl-struct-fastseedtype 148 9 213 70] "BIANMADI"] [cl-struct-plansys 205 235 3 3 7 35 13720 4045 [cl-struct-fastseedtype 216 205 219 84] "RAREAT"] [cl-struct-plansys 148 49 1 4 8 38 21888 3732 [cl-struct-fastseedtype 96 148 229 211] "BEORLE"] [cl-struct-plansys 110 1 1 5 11 51 33048 5230 [cl-struct-fastseedtype 236 110 243 9] "ESQUBE"] [cl-struct-plansys 119 243 3 3 9 43 16856 4215 [cl-struct-fastseedtype 220 119 197 229] "CEATRECE"] [cl-struct-plansys 229 174 6 6 5 33 10560 4325 [cl-struct-fastseedtype 112 229 27 229] "CEORGE"] [cl-struct-plansys 162 67 3 1 7 33 9240 6562 [cl-struct-fastseedtype 8 162 245 94] "RIZACE"] [cl-struct-plansys 233 69 5 4 5 30 9600 3817 [cl-struct-fastseedtype 100 233 211 115] "BEXE"] [cl-struct-plansys 22 0 0 4 11 49 31360 5142 [cl-struct-fastseedtype 228 22 245 185] "ORERLE"] [cl-struct-plansys 44 161 3 1 5 25 7000 6700 [cl-struct-fastseedtype 200 44 155 159] "ONREQUUS"] [cl-struct-plansys 72 58 2 6 8 41 26240 3144 [cl-struct-fastseedtype 112 72 69 241] "ATRARE"] [cl-struct-plansys 19 174 6 3 6 34 7616 4115 [cl-struct-fastseedtype 156 19 243 53] "LATILE"] [cl-struct-plansys 110 65 1 5 11 51 33048 4718 [cl-struct-fastseedtype 172 110 101 167] "SOTIDI"] [cl-struct-plansys 107 109 5 4 7 38 12160 2923 [cl-struct-fastseedtype 224 107 91 48] "ERESCEON"] [cl-struct-plansys 223 165 5 3 7 37 10360 6623 [cl-struct-fastseedtype 152 223 213 142] "RERAXE"] [cl-struct-plansys 37 130 2 2 7 33 12672 5925 [cl-struct-fastseedtype 148 37 83 188] "TEZABI"] [cl-struct-plansys 103 70 6 6 7 41 13120 3431 [cl-struct-fastseedtype 52 103 21 18] "ENONEDAR"] [cl-struct-plansys 10 56 0 7 13 60 52800 3594 [cl-struct-fastseedtype 184 10 91 3] "GESOLAON"] [cl-struct-plansys 159 85 7 0 3 20 1920 5791 [cl-struct-fastseedtype 128 159 165 59] "ANERMA"] [cl-struct-plansys 119 11 3 1 8 37 10360 3447 [cl-struct-fastseedtype 76 119 243 178] "ENLAONUS"] [cl-struct-plansys 72 95 7 7 4 31 8184 6472 [cl-struct-fastseedtype 124 72 5 222] "RIBIINAT"] [cl-struct-plansys 17 172 4 2 5 27 7776 3857 [cl-struct-fastseedtype 80 17 155 68] "ZAXEED"] [cl-struct-plansys 160 88 0 5 10 46 33120 5792 [cl-struct-fastseedtype 40 160 181 251] "ANRAER"] [cl-struct-plansys 128 144 2 0 5 23 5888 4224 [cl-struct-fastseedtype 196 128 211 5] "CETIISQU"]] [[cl-struct-plansys 64 75 3 1 5 25 7000 4416 [cl-struct-fastseedtype 9 64 106 246] "VELEEDER"] [cl-struct-plansys 48 184 0 2 8 35 16800 5424 [cl-struct-fastseedtype 85 48 64 106] "ARENDI"] [cl-struct-plansys 170 237 5 3 6 33 9240 5546 [cl-struct-fastseedtype 93 170 122 234] "ARXETIGE"] [cl-struct-plansys 131 23 7 6 6 38 9120 6019 [cl-struct-fastseedtype 177 131 232 28] "TETIED"] [cl-struct-plansys 44 88 2 0 5 23 5888 5932 [cl-struct-fastseedtype 193 44 154 60] "TELEAR"] [cl-struct-plansys 41 42 2 3 8 38 17024 4137 [cl-struct-fastseedtype 29 41 160 21] "LAESUSSO"] [cl-struct-plansys 39 168 0 6 13 59 47200 5159 [cl-struct-fastseedtype 181 39 202 57] "ORESMAA"] [cl-struct-plansys 175 107 3 3 9 43 16856 4015 [cl-struct-fastseedtype 25 175 104 36] "ZARIENLA"] [cl-struct-plansys 117 18 2 7 10 50 35200 4725 [cl-struct-fastseedtype 185 117 10 199] "SOAMAXE"] [cl-struct-plansys 162 139 3 4 8 40 17920 6306 [cl-struct-fastseedtype 37 162 64 125] "ISMAARBE"] [cl-struct-plansys 243 202 2 1 9 40 12800 5363 [cl-struct-fastseedtype 77 243 90 105] "ESSOZA"] [cl-struct-plansys 20 132 6 0 1 11 1408 2836 [cl-struct-fastseedtype 193 20 40 192] "EDDI"] [cl-struct-plansys 70 45 5 6 7 40 16000 6214 [cl-struct-fastseedtype 241 70 186 205] "DIXETISO"] [cl-struct-plansys 103 87 7 5 6 37 7992 3175 [cl-struct-fastseedtype 109 103 32 1] "LEONUS"] [cl-struct-plansys 233 40 0 4 10 45 28800 3305 [cl-struct-fastseedtype 37 233 42 209] "ATXETEER"] [cl-struct-plansys 80 156 4 5 6 34 14688 2896 [cl-struct-fastseedtype 169 80 40 208] "ERISRIIN"] [cl-struct-plansys 108 222 6 5 4 28 8064 4972 [cl-struct-fastseedtype 105 108 170 8] "USBEUS"] [cl-struct-plansys 36 200 0 6 10 47 37600 3108 [cl-struct-fastseedtype 245 36 64 65] "LERELA"] [cl-struct-plansys 5 149 5 7 7 41 18040 5125 [cl-struct-fastseedtype 61 5 58 201] "ESGEEN"] [cl-struct-plansys 94 50 2 2 8 37 14208 3934 [cl-struct-fastseedtype 209 94 104 244] "RACEED"] [cl-struct-plansys 82 216 0 4 11 49 31360 6738 [cl-struct-fastseedtype 33 82 218 175] "AQUTEBI"] [cl-struct-plansys 102 220 4 7 9 48 25344 6246 [cl-struct-fastseedtype 189 102 160 29] "ISCEGE"] [cl-struct-plansys 99 229 5 2 6 32 7680 5219 [cl-struct-fastseedtype 149 99 138 169] "ESENON"] [cl-struct-plansys 155 255 7 7 7 43 11352 6043 [cl-struct-fastseedtype 57 155 232 140] "INSOAZA"] [cl-struct-plansys 4 79 7 3 2 19 3192 5636 [cl-struct-fastseedtype 25 4 74 123] "ANARE"] [cl-struct-plansys 152 78 6 0 1 11 1408 4504 [cl-struct-fastseedtype 197 152 64 182] "VEISIN"] [cl-struct-plansys 64 236 4 5 6 34 14688 5440 [cl-struct-fastseedtype 45 64 26 202] "ARVELELE"] [cl-struct-plansys 193 1 1 4 9 42 24192 5313 [cl-struct-fastseedtype 225 193 168 185] "ORTEED"] [cl-struct-plansys 46 248 0 2 10 43 20640 3374 [cl-struct-fastseedtype 81 46 250 162] "XEESQU"] [cl-struct-plansys 7 154 2 1 9 40 12800 5639 [cl-struct-fastseedtype 13 7 32 107] "MAINON"] [cl-struct-plansys 247 127 7 0 3 20 1920 3575 [cl-struct-fastseedtype 5 247 234 130] "XEEDEN"] [cl-struct-plansys 238 114 2 1 8 36 11520 5614 [cl-struct-fastseedtype 201 238 168 90] "QUTECETE"] [cl-struct-plansys 28 5 7 1 1 13 1560 6428 [cl-struct-fastseedtype 201 28 234 222] "RILEISIS"] [cl-struct-plansys 221 253 5 2 4 24 5760 6109 [cl-struct-fastseedtype 149 221 64 220] "TETIER"] [cl-struct-plansys 4 112 0 3 9 40 22400 5636 [cl-struct-fastseedtype 29 4 250 43] "MAATIS"] [cl-struct-plansys 157 209 1 6 10 48 34560 6813 [cl-struct-fastseedtype 241 157 232 15] "ARIIN"] [cl-struct-plansys 187 44 6 0 4 23 2944 4795 [cl-struct-fastseedtype 129 187 26 103] "SOAAT"] [cl-struct-plansys 40 113 1 3 8 37 18648 5160 [cl-struct-fastseedtype 93 40 160 233] "ESGELAGE"] [cl-struct-plansys 3 149 5 6 8 44 17600 6147 [cl-struct-fastseedtype 117 3 74 29] "ISVETIMA"] [cl-struct-plansys 171 215 7 3 5 31 5208 5291 [cl-struct-fastseedtype 89 171 104 57] "ORTEVE"] [cl-struct-plansys 150 160 0 7 13 60 52800 3734 [cl-struct-fastseedtype 121 150 138 243] "BEARRABE"] [cl-struct-plansys 211 180 4 4 8 41 15744 3795 [cl-struct-fastseedtype 101 211 64 179] "BEANXEAT"] [cl-struct-plansys 177 194 2 1 7 32 10240 6577 [cl-struct-fastseedtype 13 177 218 174] "REXEXE"] [cl-struct-plansys 83 130 2 0 8 35 8960 4691 [cl-struct-fastseedtype 1 83 40 247] "TIINVE"] [cl-struct-plansys 217 22 6 6 5 33 10560 6105 [cl-struct-fastseedtype 177 217 58 188] "TEINXEAR"] [cl-struct-plansys 170 66 2 5 10 48 27648 5290 [cl-struct-fastseedtype 173 170 32 153] "ORCEES"] [cl-struct-plansys 232 201 1 4 8 38 21888 5096 [cl-struct-fastseedtype 229 232 170 56] "EDMAINER"] [cl-struct-plansys 48 12 4 5 6 34 14688 5168 [cl-struct-fastseedtype 233 48 40 41] "ESBILEAT"] [cl-struct-plansys 81 193 1 5 10 47 30456 5201 [cl-struct-fastseedtype 41 81 42 121] "ORMALA"] [cl-struct-plansys 90 85 5 6 7 40 16000 5722 [cl-struct-fastseedtype 53 90 64 59] "ANMA"] [cl-struct-plansys 166 128 0 7 13 60 52800 3494 [cl-struct-fastseedtype 253 166 186 18] "ENQURA"] [cl-struct-plansys 65 244 4 2 5 27 7776 6721 [cl-struct-fastseedtype 17 65 104 111] "AZARA"] [cl-struct-plansys 104 84 4 4 5 29 11136 3432 [cl-struct-fastseedtype 225 104 90 98] "XEMADI"] [cl-struct-plansys 109 235 3 7 9 47 28952 5229 [cl-struct-fastseedtype 253 109 160 121] "ORBEQUSO"] [cl-struct-plansys 7 186 2 2 9 41 15744 4103 [cl-struct-fastseedtype 85 7 10 149] "LATIBE"] [cl-struct-plansys 223 243 3 7 11 55 33880 5343 [cl-struct-fastseedtype 121 223 232 41] "ESTEBIIN"] [cl-struct-plansys 44 6 6 3 3 22 4928 6700 [cl-struct-fastseedtype 217 44 202 47] "AXEON"] [cl-struct-plansys 82 191 7 0 2 16 1536 3922 [cl-struct-fastseedtype 5 82 64 116] "RACEMACE"] [cl-struct-plansys 69 75 3 5 8 41 20664 4677 [cl-struct-fastseedtype 237 69 154 23] "TIESSOUS"] [cl-struct-plansys 200 7 7 4 2 20 3840 5064 [cl-struct-fastseedtype 33 200 168 120] "EDSOUS"] [cl-struct-plansys 73 136 0 2 9 39 18720 5193 [cl-struct-fastseedtype 17 73 122 25] "ORMADI"] [cl-struct-plansys 82 77 7 1 3 21 2520 5714 [cl-struct-fastseedtype 77 82 32 139] "MAMAUS"] [cl-struct-plansys 190 8 2 0 7 31 7936 3518 [cl-struct-fastseedtype 197 190 106 242] "ENORAR"] [cl-struct-plansys 23 106 2 1 9 40 12800 5655 [cl-struct-fastseedtype 9 23 168 59] "ANISERES"] [cl-struct-plansys 9 16 2 1 7 32 10240 4617 [cl-struct-fastseedtype 137 9 106 215] "TIATENQU"] [cl-struct-plansys 154 210 2 2 8 37 14208 6554 [cl-struct-fastseedtype 213 154 64 94] "RIMAZA"] [cl-struct-plansys 237 196 4 3 6 32 10752 6381 [cl-struct-fastseedtype 221 237 122 125] "ISAANUS"] [cl-struct-plansys 72 155 3 6 7 38 21280 3400 [cl-struct-fastseedtype 49 72 232 18] "ENVEAT"] [cl-struct-plansys 90 81 3 0 6 28 6272 3162 [cl-struct-fastseedtype 65 90 154 161] "LEDIUS"] [cl-struct-plansys 55 72 0 3 12 52 29120 6199 [cl-struct-fastseedtype 157 55 160 205] "DIREBEA"] [cl-struct-plansys 111 83 3 6 10 50 28000 2927 [cl-struct-fastseedtype 53 111 202 16] "ERBEBETI"] [cl-struct-plansys 55 83 3 3 9 43 16856 6455 [cl-struct-fastseedtype 153 55 104 94] "RIESONSO"] [cl-struct-plansys 199 127 7 7 7 43 11352 3015 [cl-struct-fastseedtype 57 199 10 48] "ERVEDIRA"] [cl-struct-plansys 20 109 5 4 4 26 8320 5140 [cl-struct-fastseedtype 165 20 64 249] "ORANESRI"] [cl-struct-plansys 254 137 3 1 7 33 9240 4094 [cl-struct-fastseedtype 205 254 90 4] "ZAINER"] [cl-struct-plansys 33 144 2 0 6 27 6912 6433 [cl-struct-fastseedtype 65 33 40 62] "RIARE"] [cl-struct-plansys 124 78 6 6 4 29 9280 5500 [cl-struct-fastseedtype 113 124 186 186] "QUBIISIS"] [cl-struct-plansys 253 189 5 5 6 35 12600 3325 [cl-struct-fastseedtype 237 253 32 65] "LETEAN"] [cl-struct-plansys 120 59 3 4 6 32 14336 2936 [cl-struct-fastseedtype 165 120 42 176] "ERZAINLE"] [cl-struct-plansys 161 140 4 5 7 38 16416 3489 [cl-struct-fastseedtype 41 161 40 146] "ENBECE"] [cl-struct-plansys 69 243 3 5 8 41 20664 5189 [cl-struct-fastseedtype 233 69 170 249] "ORBEEN"] [cl-struct-plansys 159 114 2 6 11 53 33920 4255 [cl-struct-fastseedtype 117 159 64 69] "CETITE"] [cl-struct-plansys 216 60 4 7 7 40 21120 6104 [cl-struct-fastseedtype 189 216 58 108] "INLEBI"] [cl-struct-plansys 179 198 6 2 5 29 5568 5555 [cl-struct-fastseedtype 81 179 104 250] "QUBELE"] [cl-struct-plansys 143 33 1 4 11 50 28800 3983 [cl-struct-fastseedtype 161 143 218 36] "ZALAARCE"] [cl-struct-plansys 133 138 2 7 10 50 35200 4229 [cl-struct-fastseedtype 61 133 160 229] "CELAAN"] [cl-struct-plansys 59 96 0 2 11 47 22560 2875 [cl-struct-fastseedtype 21 59 138 144] "ERMATI"] [cl-struct-plansys 179 247 7 7 7 43 11352 4531 [cl-struct-fastseedtype 185 179 232 214] "VEXEINCE"] [cl-struct-plansys 101 12 4 3 6 32 10752 3941 [cl-struct-fastseedtype 153 101 74 244] "RABI"] [cl-struct-plansys 27 192 2 0 8 35 8960 3355 [cl-struct-fastseedtype 69 27 64 66] "XEISANAN"] [cl-struct-plansys 219 123 3 5 10 49 24696 4315 [cl-struct-fastseedtype 173 219 26 117] "LAINISRI"] [cl-struct-plansys 94 29 5 4 6 34 10880 4702 [cl-struct-fastseedtype 97 94 168 71] "SOGEES"] [cl-struct-plansys 115 105 1 2 10 44 19008 6771 [cl-struct-fastseedtype 209 115 250 159] "ONTEER"] [cl-struct-plansys 173 144 2 1 7 32 10240 5805 [cl-struct-fastseedtype 141 173 32 187] "ANORXE"] [cl-struct-plansys 22 98 2 0 7 31 7936 3094 [cl-struct-fastseedtype 133 22 234 113] "ATAREN"] [cl-struct-plansys 207 114 2 1 9 40 12800 6095 [cl-struct-fastseedtype 73 207 168 44] "INREARBI"] [cl-struct-plansys 6 106 2 1 8 36 11520 6662 [cl-struct-fastseedtype 73 6 234 223] "ONERTIUS"] [cl-struct-plansys 104 55 7 2 1 14 2016 2920 [cl-struct-fastseedtype 21 104 64 240] "ERASO"] [cl-struct-plansys 103 231 7 3 5 31 5208 6503 [cl-struct-fastseedtype 157 103 250 222] "RIRELAXE"] [cl-struct-plansys 130 117 5 6 7 40 16000 4226 [cl-struct-fastseedtype 113 130 232 37] "CEISCE"] [cl-struct-plansys 9 197 7 0 1 12 1152 5897 [cl-struct-fastseedtype 1 9 26 236] "INQUA"] [cl-struct-plansys 86 175 7 3 4 27 4536 3158 [cl-struct-fastseedtype 221 86 160 193] "LEUSAR"] [cl-struct-plansys 106 224 0 6 12 55 44000 3946 [cl-struct-fastseedtype 245 106 74 20] "RAONBE"] [cl-struct-plansys 83 223 7 3 5 31 5208 3667 [cl-struct-fastseedtype 217 83 104 147] "BEBIDISO"] [cl-struct-plansys 7 173 5 7 9 49 21560 5895 [cl-struct-fastseedtype 249 7 138 124] "TEENVECE"] [cl-struct-plansys 101 182 6 4 4 27 6912 6757 [cl-struct-fastseedtype 229 101 64 79] "AINLETE"] [cl-struct-plansys 220 33 3 1 5 25 7000 5340 [cl-struct-fastseedtype 141 220 218 105] "ESSORE"] [cl-struct-plansys 127 174 6 0 4 23 2944 4223 [cl-struct-fastseedtype 129 127 40 149] "LAXETI"] [cl-struct-plansys 47 215 7 6 6 38 9120 5167 [cl-struct-fastseedtype 49 47 58 201] "ESAUSLE"] [cl-struct-plansys 97 200 0 5 11 50 36000 5217 [cl-struct-fastseedtype 45 97 32 249] "ORXETE"] [cl-struct-plansys 152 124 4 4 5 29 11136 4760 [cl-struct-fastseedtype 101 152 170 55] "TIINTELE"] [cl-struct-plansys 161 28 4 5 7 38 16416 5793 [cl-struct-fastseedtype 105 161 40 11] "MAESVEAR"] [cl-struct-plansys 74 118 6 5 6 36 10368 5450 [cl-struct-fastseedtype 169 74 42 138] "ARARON"] [cl-struct-plansys 244 31 7 6 3 26 6240 6900 [cl-struct-fastseedtype 181 244 64 95] "ONBESO"] [cl-struct-plansys 154 199 7 7 6 39 10296 4250 [cl-struct-fastseedtype 125 154 186 213] "LATIUS"] [cl-struct-plansys 181 168 0 2 9 39 18720 4277 [cl-struct-fastseedtype 145 181 104 149] "LABEIS"] [cl-struct-plansys 198 61 5 4 6 34 10880 4806 [cl-struct-fastseedtype 97 198 90 247] "TIANORIN"] [cl-struct-plansys 172 185 1 7 10 49 38808 3244 [cl-struct-fastseedtype 125 172 160 97] "LESOLARI"] [cl-struct-plansys 254 213 5 2 5 28 6720 6142 [cl-struct-fastseedtype 213 254 10 156] "TEERMA"] [cl-struct-plansys 23 11 3 7 11 55 33880 3607 [cl-struct-fastseedtype 249 23 232 147] "BETIGERE"] [cl-struct-plansys 174 99 3 3 8 39 15288 5038 [cl-struct-fastseedtype 89 174 202 200] "USQUAT"] [cl-struct-plansys 244 81 3 0 4 20 4480 3060 [cl-struct-fastseedtype 133 244 64 32] "BIQULE"] [cl-struct-plansys 1 122 2 5 9 44 25344 3329 [cl-struct-fastseedtype 109 1 154 226] "XEONGEZA"] [cl-struct-plansys 132 67 3 4 6 32 14336 4484 [cl-struct-fastseedtype 161 132 168 38] "BIREOR"] [cl-struct-plansys 174 153 1 2 9 40 17280 4526 [cl-struct-fastseedtype 145 174 122 54] "VERIGE"] [cl-struct-plansys 24 99 3 1 5 25 7000 5656 [cl-struct-fastseedtype 205 24 32 251] "ANTIMA"] [cl-struct-plansys 254 139 3 0 6 28 6272 3326 [cl-struct-fastseedtype 69 254 106 1] "LEMAAR"] [cl-struct-plansys 23 138 2 1 9 40 12800 6167 [cl-struct-fastseedtype 137 23 168 45] "DIERLABE"] [cl-struct-plansys 19 21 7 1 4 25 3000 4883 [cl-struct-fastseedtype 9 19 106 248] "EDINCE"] [cl-struct-plansys 69 44 4 2 5 27 7776 3397 [cl-struct-fastseedtype 85 69 64 146] "ENGEAN"] [cl-struct-plansys 113 219 3 3 7 35 13720 2929 [cl-struct-fastseedtype 93 113 122 80] "ERISONIN"] [cl-struct-plansys 76 95 7 6 3 26 6240 4940 [cl-struct-fastseedtype 177 76 232 72] "USRAAR"] [cl-struct-plansys 199 138 2 0 8 35 8960 4551 [cl-struct-fastseedtype 193 199 154 70] "BIEDBI"] [cl-struct-plansys 134 166 6 3 5 30 6720 4230 [cl-struct-fastseedtype 29 134 160 197] "CEENRIVE"] [cl-struct-plansys 246 62 6 6 6 37 11840 4854 [cl-struct-fastseedtype 181 246 202 39] "SOTEAN"] [cl-struct-plansys 0 123 3 3 6 31 12152 4864 [cl-struct-fastseedtype 25 0 104 216] "EDBEINED"] [cl-struct-plansys 88 44 4 7 7 40 21120 5208 [cl-struct-fastseedtype 185 88 10 217] "ORRIASO"] [cl-struct-plansys 199 143 7 4 5 32 6144 4295 [cl-struct-fastseedtype 37 199 64 181] "LAINUSAR"] [cl-struct-plansys 74 136 2 1 8 36 11520 6730 [cl-struct-fastseedtype 77 74 90 223] "ONENTE"] [cl-struct-plansys 109 220 6 0 2 15 1920 5997 [cl-struct-fastseedtype 193 109 40 252] "TECEA"] [cl-struct-plansys 241 175 7 6 4 30 7200 4849 [cl-struct-fastseedtype 241 241 186 231] "SOESGERA"] [cl-struct-plansys 212 99 3 5 7 37 18648 3284 [cl-struct-fastseedtype 109 212 32 193] "LEEDRE"] [cl-struct-plansys 72 142 6 4 3 23 5888 6728 [cl-struct-fastseedtype 37 72 42 207] "ACETEAT"] [cl-struct-plansys 49 188 4 5 7 38 16416 3889 [cl-struct-fastseedtype 169 49 40 148] "RAXEUSON"] [cl-struct-plansys 95 72 0 5 13 58 41760 5471 [cl-struct-fastseedtype 105 95 170 42] "ARENTE"] [cl-struct-plansys 89 92 4 6 7 39 18720 5209 [cl-struct-fastseedtype 245 89 64 137] "ESONGE"] [cl-struct-plansys 236 35 3 7 8 43 26488 6892 [cl-struct-fastseedtype 61 236 58 79] "ARIQU"] [cl-struct-plansys 71 154 2 2 9 41 15744 2887 [cl-struct-fastseedtype 209 71 104 64] "XEAR"] [cl-struct-plansys 13 170 2 4 8 39 19968 5133 [cl-struct-fastseedtype 33 13 218 217] "ORATEDGE"] [cl-struct-plansys 227 120 0 7 14 64 56320 6371 [cl-struct-fastseedtype 189 227 160 237] "DIESANEN"] [cl-struct-plansys 82 27 3 2 7 34 11424 4690 [cl-struct-fastseedtype 149 82 138 183] "TICEA"] [cl-struct-plansys 12 47 7 7 4 31 8184 2828 [cl-struct-fastseedtype 57 12 232 96] "TEESSO"] [cl-struct-plansys 7 9 1 3 11 49 24696 6151 [cl-struct-fastseedtype 25 7 74 173] "DIRIEN"] [cl-struct-plansys 221 114 2 0 6 27 6912 6621 [cl-struct-fastseedtype 197 221 64 14] "RERIARTI"] [cl-struct-plansys 183 74 2 5 11 52 29952 2999 [cl-struct-fastseedtype 45 183 26 96] "LEORQU"] [cl-struct-plansys 58 121 1 4 10 46 26496 4154 [cl-struct-fastseedtype 225 58 168 21] "LAESQU"] [cl-struct-plansys 249 26 2 2 7 33 12672 6137 [cl-struct-fastseedtype 81 249 250 220] "TEERBI"] [cl-struct-plansys 148 198 6 1 2 16 2560 5780 [cl-struct-fastseedtype 13 148 32 75] "MACECE"] [cl-struct-plansys 118 133 7 0 2 16 1536 2934 [cl-struct-fastseedtype 5 118 234 160] "ANEN"] [cl-struct-plansys 239 178 2 1 9 40 12800 6639 [cl-struct-fastseedtype 201 239 168 62] "RILEAA"] [cl-struct-plansys 47 15 7 1 4 25 3000 2863 [cl-struct-fastseedtype 201 47 234 32] "ATEN"] [cl-struct-plansys 50 177 1 2 9 40 17280 3890 [cl-struct-fastseedtype 149 50 64 68] "ZAUSRI"] [cl-struct-plansys 11 158 6 3 6 34 7616 3083 [cl-struct-fastseedtype 29 11 250 209] "ATANORBI"] [cl-struct-plansys 166 89 1 6 11 52 37440 5798 [cl-struct-fastseedtype 241 166 232 123] "ANANRI"] [cl-struct-plansys 150 158 6 0 3 19 2432 3222 [cl-struct-fastseedtype 129 150 26 177] "ATBIDI"] [cl-struct-plansys 197 45 5 3 5 29 8120 5317 [cl-struct-fastseedtype 93 197 160 217] "ORINMAEN"] [cl-struct-plansys 18 107 3 6 9 46 25760 5650 [cl-struct-fastseedtype 117 18 74 75] "MAESSOTE"] [cl-struct-plansys 60 39 7 3 2 19 3192 6204 [cl-struct-fastseedtype 89 60 104 45] "DIATQUOR"] [cl-struct-plansys 185 250 2 7 10 50 35200 4281 [cl-struct-fastseedtype 121 185 138 69] "CEOREDED"] [cl-struct-plansys 56 248 0 4 9 41 26240 5688 [cl-struct-fastseedtype 101 56 64 43] "MATEUS"] [cl-struct-plansys 72 192 2 1 6 28 8960 3912 [cl-struct-fastseedtype 13 72 218 100] "ZADIQU"] [cl-struct-plansys 236 26 2 0 5 23 5888 3820 [cl-struct-fastseedtype 1 236 40 115] "BEORED"] [cl-struct-plansys 196 216 0 6 10 47 37600 4548 [cl-struct-fastseedtype 177 196 58 22] "VEBERETI"] [cl-struct-plansys 87 142 6 5 7 40 11520 5207 [cl-struct-fastseedtype 173 87 32 153] "ORRIA"] [cl-struct-plansys 135 111 7 4 5 32 6144 4487 [cl-struct-fastseedtype 229 135 170 118] "VEREINAT"] [cl-struct-plansys 81 108 4 5 7 38 16416 6225 [cl-struct-fastseedtype 233 81 40 45] "DIMAMAZA"] [cl-struct-plansys 132 107 3 5 7 37 18648 5764 [cl-struct-fastseedtype 41 132 42 219] "ANARES"] [cl-struct-plansys 207 41 1 6 12 56 40320 3791 [cl-struct-fastseedtype 53 207 64 195] "GETERE"] [cl-struct-plansys 205 78 6 7 6 38 13376 5069 [cl-struct-fastseedtype 253 205 186 216] "EDLATE"] [cl-struct-plansys 106 156 4 2 6 31 8928 5738 [cl-struct-fastseedtype 17 106 104 251] "ANLEBI"] [cl-struct-plansys 99 102 6 4 6 35 8960 5987 [cl-struct-fastseedtype 225 99 90 204] "INTISOAR"] [cl-struct-plansys 42 199 7 7 6 39 10296 5162 [cl-struct-fastseedtype 253 42 160 137] "ESTEERVE"] [cl-struct-plansys 54 48 0 2 10 43 20640 3638 [cl-struct-fastseedtype 85 54 10 227] "GEARGE"] [cl-struct-plansys 144 99 3 7 8 43 26488 6288 [cl-struct-fastseedtype 121 144 232 61] "ISATA"] [cl-struct-plansys 111 0 0 3 12 52 29120 3183 [cl-struct-fastseedtype 217 111 202 161] "LEATGE"] [cl-struct-plansys 215 35 3 0 7 32 7168 6103 [cl-struct-fastseedtype 5 215 64 12] "INBIESTE"] [cl-struct-plansys 252 233 1 5 9 43 27864 6396 [cl-struct-fastseedtype 237 252 154 237] "DIRAONLE"] [cl-struct-plansys 129 191 7 4 3 24 4608 3969 [cl-struct-fastseedtype 33 129 168 20] "RARAAR"] [cl-struct-plansys 84 234 2 2 6 29 11136 3668 [cl-struct-fastseedtype 17 84 122 147] "BEENOR"] [cl-struct-plansys 31 185 3 1 8 37 10360 5663 [cl-struct-fastseedtype 77 31 32 171] "MAZARE"] [cl-struct-plansys 125 78 6 0 2 15 1920 2941 [cl-struct-fastseedtype 197 125 106 80] "ERTEAR"] [cl-struct-plansys 88 234 2 1 6 28 8960 6744 [cl-struct-fastseedtype 9 88 168 95] "ONXEQUTE"] [cl-struct-plansys 92 90 2 1 6 28 8960 5212 [cl-struct-fastseedtype 137 92 106 89] "ORERBIA"] [cl-struct-plansys 47 198 6 2 5 29 5568 4399 [cl-struct-fastseedtype 213 47 64 6] "BITEEN"] [cl-struct-plansys 52 50 2 3 7 34 15232 3636 [cl-struct-fastseedtype 221 52 122 99] "GEARGEAT"] [cl-struct-plansys 145 99 3 6 8 42 23520 6545 [cl-struct-fastseedtype 49 145 232 190] "RIBEGE"] [cl-struct-plansys 117 3 3 0 5 24 5376 5749 [cl-struct-fastseedtype 65 117 154 43] "MAZAZA"] [cl-struct-plansys 20 68 4 3 5 28 9408 6164 [cl-struct-fastseedtype 157 20 160 253] "ISTIESRI"] [cl-struct-plansys 190 105 1 6 11 52 37440 6590 [cl-struct-fastseedtype 53 190 202 126] "RIBIGEUS"] [cl-struct-plansys 8 227 3 3 6 31 12152 3336 [cl-struct-fastseedtype 153 8 104 146] "ENRIORAR"] [cl-struct-plansys 42 25 1 7 12 57 45144 3370 [cl-struct-fastseedtype 57 42 10 194] "XECEATOR"] [cl-struct-plansys 185 241 1 4 9 42 24192 3257 [cl-struct-fastseedtype 165 185 64 177] "ATTESOLA"] [cl-struct-plansys 213 199 7 1 2 17 2040 5589 [cl-struct-fastseedtype 205 213 90 250] "QUTIUS"] [cl-struct-plansys 250 104 2 0 7 31 7936 5626 [cl-struct-fastseedtype 65 250 40 250] "QUTEER"] [cl-struct-plansys 167 80 0 6 13 59 47200 4007 [cl-struct-fastseedtype 113 167 186 84] "RADIESAR"] [cl-struct-plansys 234 73 1 5 11 51 33048 3306 [cl-struct-fastseedtype 237 234 32 129] "LELALE"] [cl-struct-plansys 87 33 1 4 11 50 28800 6487 [cl-struct-fastseedtype 165 87 42 46] "RESOINXE"] [cl-struct-plansys 2 44 4 5 8 42 18144 4354 [cl-struct-fastseedtype 41 2 40 214] "VECEISED"] [cl-struct-plansys 184 221 5 5 5 31 11160 5816 [cl-struct-fastseedtype 233 184 170 155] "ANENBI"] [cl-struct-plansys 84 134 6 6 4 29 9280 6228 [cl-struct-fastseedtype 117 84 64 13] "DIUSAR"] [cl-struct-plansys 63 74 2 7 12 58 40832 3391 [cl-struct-fastseedtype 189 63 58 114] "ENTERE"] [cl-struct-plansys 28 174 6 2 2 17 3264 4380 [cl-struct-fastseedtype 81 28 104 198] "BIERBE"] [cl-struct-plansys 202 115 3 4 8 40 17920 6602 [cl-struct-fastseedtype 161 202 218 206] "REINBIXE"] [cl-struct-plansys 130 166 6 7 7 42 14784 4226 [cl-struct-fastseedtype 61 130 160 53] "LARIVEAR"] [cl-struct-plansys 170 22 6 2 4 25 4800 6570 [cl-struct-fastseedtype 21 170 138 30] "RIRISO"] [cl-struct-plansys 164 167 7 7 4 31 8184 5540 [cl-struct-fastseedtype 185 164 232 42] "ARTIBIUS"] [cl-struct-plansys 232 70 6 3 3 22 4928 4584 [cl-struct-fastseedtype 153 232 74 166] "BILAZA"] [cl-struct-plansys 224 100 6 0 1 11 1408 5600 [cl-struct-fastseedtype 69 224 64 26] "QURIOREN"] [cl-struct-plansys 210 89 1 5 11 51 33048 5842 [cl-struct-fastseedtype 173 210 26 139] "MATILATI"] [cl-struct-plansys 87 21 5 4 7 38 12160 3671 [cl-struct-fastseedtype 97 87 168 35] "GEERMA"] [cl-struct-plansys 190 11 3 2 7 34 11424 5310 [cl-struct-fastseedtype 209 190 250 89] "ORGETE"] [cl-struct-plansys 186 60 6 1 4 24 3840 5818 [cl-struct-fastseedtype 141 186 32 27] "ANENUS"] [cl-struct-plansys 21 232 2 0 6 27 6912 6677 [cl-struct-fastseedtype 133 21 234 15] "ADIEN"] [cl-struct-plansys 80 50 2 1 6 28 8960 2896 [cl-struct-fastseedtype 73 80 168 144] "ERBERAOR"] [cl-struct-plansys 153 244 6 1 3 20 3200 3225 [cl-struct-fastseedtype 73 153 234 161] "LEAMAIS"] [cl-struct-plansys 61 107 3 2 6 30 10080 4925 [cl-struct-fastseedtype 21 61 64 216] "EDLA"] [cl-struct-plansys 238 149 5 3 6 33 9240 4078 [cl-struct-fastseedtype 157 238 250 4] "ZAESISMA"] [cl-struct-plansys 11 125 5 6 8 44 17600 3083 [cl-struct-fastseedtype 113 11 232 17] "ATQUTI"] [cl-struct-plansys 100 183 7 0 0 8 768 4452 [cl-struct-fastseedtype 1 100 26 182] "VEATMA"] [cl-struct-plansys 115 235 3 3 9 43 16856 3187 [cl-struct-fastseedtype 221 115 160 49] "ATATVEOR"] [cl-struct-plansys 249 54 6 6 5 33 10560 3577 [cl-struct-fastseedtype 245 249 74 194] "XEBEAZA"] [cl-struct-plansys 100 175 7 3 2 19 3192 4708 [cl-struct-fastseedtype 217 100 104 7] "SOANSOAR"] [cl-struct-plansys 170 135 7 7 6 39 10296 6570 [cl-struct-fastseedtype 249 170 138 78] "RELEQUAR"] [cl-struct-plansys 74 122 2 4 9 43 22016 4682 [cl-struct-fastseedtype 229 74 64 71] "SODIONBE"] [cl-struct-plansys 243 159 7 1 4 25 3000 6899 [cl-struct-fastseedtype 141 243 218 159] "ONENBI"] [cl-struct-plansys 152 198 6 0 1 11 1408 3224 [cl-struct-fastseedtype 129 152 40 145] "ATAOR"] [cl-struct-plansys 154 25 1 6 11 52 37440 3738 [cl-struct-fastseedtype 49 154 58 163] "GEVERARE"] [cl-struct-plansys 142 148 4 5 8 42 18144 5262 [cl-struct-fastseedtype 45 142 32 121] "ORANXE"] [cl-struct-plansys 183 162 2 4 10 47 24064 4279 [cl-struct-fastseedtype 101 183 170 245] "LAATEXE"] [cl-struct-plansys 66 252 4 5 8 42 18144 6722 [cl-struct-fastseedtype 105 66 40 143] "AREIS"] [cl-struct-plansys 253 160 0 5 11 50 36000 6141 [cl-struct-fastseedtype 169 253 42 108] "INESBE"] [cl-struct-plansys 233 115 3 6 8 42 23520 4841 [cl-struct-fastseedtype 181 233 64 103] "SOZALA"] [cl-struct-plansys 65 21 5 7 7 41 18040 5697 [cl-struct-fastseedtype 125 65 186 27] "ANENER"] [cl-struct-plansys 94 208 0 2 10 43 20640 3166 [cl-struct-fastseedtype 145 94 104 161] "LEERA"] [cl-struct-plansys 65 207 7 4 3 24 4608 3137 [cl-struct-fastseedtype 97 65 90 225] "LEENLAES"] [cl-struct-plansys 233 21 5 7 7 41 18040 3305 [cl-struct-fastseedtype 125 233 160 241] "ATERMADI"] [cl-struct-plansys 173 203 3 2 6 30 10080 5549 [cl-struct-fastseedtype 213 173 10 106] "ARGEAN"] [cl-struct-plansys 72 251 3 7 8 43 26488 4680 [cl-struct-fastseedtype 249 72 232 39] "SOINISAT"] [cl-struct-plansys 113 221 5 3 5 29 8120 5489 [cl-struct-fastseedtype 89 113 202 186] "QUESLA"] [cl-struct-plansys 249 53 7 0 1 12 1152 5113 [cl-struct-fastseedtype 133 249 64 56] "EDSOEDED"] [cl-struct-plansys 56 152 0 5 10 46 33120 4920 [cl-struct-fastseedtype 109 56 154 56] "EDARANIS"] [cl-struct-plansys 189 123 3 4 7 36 16128 3517 [cl-struct-fastseedtype 161 189 168 66] "XEANAN"] [cl-struct-plansys 57 123 3 2 6 30 10080 2873 [cl-struct-fastseedtype 145 57 122 48] "ERCEA"] [cl-struct-plansys 101 79 7 1 2 17 2040 5733 [cl-struct-fastseedtype 205 101 32 155] "ANERAT"] [cl-struct-plansys 61 81 3 0 5 24 5376 6717 [cl-struct-fastseedtype 69 61 106 223] "ONREAR"] [cl-struct-plansys 216 138 2 1 6 28 8960 3288 [cl-struct-fastseedtype 137 216 168 209] "ATLAONBI"]] [[cl-struct-plansys 128 150 6 2 2 17 3264 6272 [cl-struct-fastseedtype 18 128 212 237] "DIZAEN"] [cl-struct-plansys 100 114 2 5 8 40 23040 5732 [cl-struct-fastseedtype 170 100 128 219] "ANENEN"] [cl-struct-plansys 128 242 2 7 9 46 32384 4224 [cl-struct-fastseedtype 186 128 244 37] "CEOR"] [cl-struct-plansys 255 64 0 4 12 53 33920 5119 [cl-struct-fastseedtype 98 255 208 216] "EDEDER"] [cl-struct-plansys 225 240 2 0 6 27 6912 5601 [cl-struct-fastseedtype 130 225 52 234] "ARTESORE"] [cl-struct-plansys 46 135 7 7 6 39 10296 3374 [cl-struct-fastseedtype 58 46 64 114] "ENUSUSGE"] [cl-struct-plansys 99 200 0 5 13 58 41760 3939 [cl-struct-fastseedtype 106 99 148 84] "RAEDRE"] [cl-struct-plansys 14 56 0 6 12 55 44000 4622 [cl-struct-fastseedtype 50 14 208 199] "SOREZAAN"] [cl-struct-plansys 187 228 4 6 9 47 22560 6843 [cl-struct-fastseedtype 114 187 20 175] "AAORED"] [cl-struct-plansys 184 185 3 1 5 25 7000 3256 [cl-struct-fastseedtype 74 184 128 193] "LEBEDIXE"] [cl-struct-plansys 162 173 5 3 6 33 9240 3746 [cl-struct-fastseedtype 154 162 180 131] "GEBEQUAT"] [cl-struct-plansys 81 250 2 0 6 27 6912 6737 [cl-struct-fastseedtype 130 81 80 31] "ONARAN"] [cl-struct-plansys 101 219 3 4 7 36 16128 5989 [cl-struct-fastseedtype 226 101 116 172] "INRE"] [cl-struct-plansys 154 0 0 3 11 48 26880 5274 [cl-struct-fastseedtype 218 154 64 137] "ESZAUSVE"] [cl-struct-plansys 246 72 2 1 8 36 11520 3830 [cl-struct-fastseedtype 74 246 84 99] "GEXEAN"] [cl-struct-plansys 1 251 3 2 6 30 10080 6657 [cl-struct-fastseedtype 82 1 80 159] "ONTEISED"] [cl-struct-plansys 120 61 5 2 3 20 4800 3448 [cl-struct-fastseedtype 210 120 84 82] "ENUSBE"] [cl-struct-plansys 45 211 3 5 8 41 20664 5165 [cl-struct-fastseedtype 234 45 128 9] "ESARXEVE"] [cl-struct-plansys 86 66 2 7 11 54 38016 3670 [cl-struct-fastseedtype 122 86 116 163] "GETEVE"] [cl-struct-plansys 21 54 6 4 4 27 6912 4629 [cl-struct-fastseedtype 162 21 208 135] "SOBEER"] [cl-struct-plansys 204 112 2 0 5 23 5888 3020 [cl-struct-fastseedtype 66 204 180 16] "ERDIARES"] [cl-struct-plansys 137 42 2 7 10 50 35200 3465 [cl-struct-fastseedtype 122 137 64 2] "XEVEXEAN"] [cl-struct-plansys 251 66 2 5 11 52 29952 4091 [cl-struct-fastseedtype 42 251 20 244] "RAATBI"] [cl-struct-plansys 70 32 0 6 12 55 44000 4934 [cl-struct-fastseedtype 114 70 208 152] "EDONED"] [cl-struct-plansys 120 223 7 6 3 26 6240 4728 [cl-struct-fastseedtype 50 120 148 87] "TIARICE"] [cl-struct-plansys 133 126 6 1 3 20 3200 3717 [cl-struct-fastseedtype 138 133 128 179] "BETIERTE"] [cl-struct-plansys 92 240 0 3 9 40 22400 4188 [cl-struct-fastseedtype 90 92 52 5] "CEENRAMA"] [cl-struct-plansys 11 180 6 0 4 23 2944 3339 [cl-struct-fastseedtype 194 11 80 18] "ENENER"] [cl-struct-plansys 212 240 0 4 9 41 26240 4564 [cl-struct-fastseedtype 162 212 244 150] "VEANSO"] [cl-struct-plansys 186 199 7 3 4 27 4536 6330 [cl-struct-fastseedtype 26 186 64 221] "ISRIVEEN"] [cl-struct-plansys 50 246 6 1 4 24 3840 4402 [cl-struct-fastseedtype 10 50 212 134] "BIRESO"] [cl-struct-plansys 157 103 7 2 2 18 2592 3997 [cl-struct-fastseedtype 146 157 80 180] "RAORMAOR"] [cl-struct-plansys 121 11 3 2 6 30 10080 6521 [cl-struct-fastseedtype 146 121 212 62] "RIGETE"] [cl-struct-plansys 127 124 4 5 9 46 19872 6783 [cl-struct-fastseedtype 42 127 128 191] "ONANORRA"] [cl-struct-plansys 116 249 1 7 10 49 38808 4980 [cl-struct-fastseedtype 58 116 244 40] "USVERA"] [cl-struct-plansys 243 52 4 4 8 41 15744 6643 [cl-struct-fastseedtype 226 243 208 190] "RIBIOR"] [cl-struct-plansys 63 153 3 0 7 32 7168 6719 [cl-struct-fastseedtype 2 63 52 191] "ONTILAIN"] [cl-struct-plansys 236 149 5 7 6 37 16280 5612 [cl-struct-fastseedtype 186 236 64 26] "QUTEGEQU"] [cl-struct-plansys 90 163 3 5 9 45 22680 5722 [cl-struct-fastseedtype 234 90 148 155] "ANQUERBI"] [cl-struct-plansys 198 144 0 6 12 55 44000 3270 [cl-struct-fastseedtype 178 198 208 241] "ATESLETE"] [cl-struct-plansys 60 1 1 6 9 44 31680 4924 [cl-struct-fastseedtype 242 60 20 136] "USBIMAQU"] [cl-struct-plansys 218 139 3 1 7 33 9240 6362 [cl-struct-fastseedtype 202 218 128 45] "DIRATERI"] [cl-struct-plansys 94 156 4 3 7 36 12096 6494 [cl-struct-fastseedtype 26 94 180 142] "REESVERE"] [cl-struct-plansys 142 118 6 0 3 19 2432 6286 [cl-struct-fastseedtype 2 142 80 141] "DIATIN"] [cl-struct-plansys 203 172 4 4 8 41 15744 5323 [cl-struct-fastseedtype 98 203 116 9] "ESLEVE"] [cl-struct-plansys 225 86 6 3 4 26 5824 5345 [cl-struct-fastseedtype 90 225 64 185] "ORERMALA"] [cl-struct-plansys 53 139 3 1 6 29 8120 3381 [cl-struct-fastseedtype 202 53 84 178] "ENBEAN"] [cl-struct-plansys 129 91 3 2 6 30 10080 3201 [cl-struct-fastseedtype 210 129 80 81] "ATREXEXE"] [cl-struct-plansys 130 2 2 2 8 37 14208 3714 [cl-struct-fastseedtype 82 130 84 179] "BETIDI"] [cl-struct-plansys 88 109 5 5 5 31 11160 6232 [cl-struct-fastseedtype 106 88 128 253] "ISGEOROR"] [cl-struct-plansys 217 25 1 7 11 53 41976 4569 [cl-struct-fastseedtype 250 217 116 182] "VEESQU"] [cl-struct-plansys 154 58 2 4 9 43 22016 6298 [cl-struct-fastseedtype 34 154 208 125] "ISENES"] [cl-struct-plansys 57 105 3 0 5 24 5376 4153 [cl-struct-fastseedtype 194 57 180 245] "LAORUSTI"] [cl-struct-plansys 87 200 0 7 14 64 56320 5463 [cl-struct-fastseedtype 250 87 64 186] "QUANDIXE"] [cl-struct-plansys 130 237 5 5 7 39 14040 5762 [cl-struct-fastseedtype 170 130 20 75] "MAANUSRE"] [cl-struct-plansys 142 136 0 6 12 55 44000 3470 [cl-struct-fastseedtype 242 142 208 210] "ENARINES"] [cl-struct-plansys 9 76 4 6 7 39 18720 2825 [cl-struct-fastseedtype 178 9 148 64] "VEED"] [cl-struct-plansys 184 224 2 1 6 28 8960 6840 [cl-struct-fastseedtype 10 184 128 47] "AUSASO"] [cl-struct-plansys 167 175 7 3 5 31 5208 2983 [cl-struct-fastseedtype 218 167 52 32] "EDED"] [cl-struct-plansys 216 64 2 0 5 23 5888 3032 [cl-struct-fastseedtype 66 216 80 144] "ERUSAT"] [cl-struct-plansys 74 17 1 4 10 46 26496 3658 [cl-struct-fastseedtype 34 74 244 3] "GEONDI"] [cl-struct-plansys 16 173 5 3 4 25 7000 6160 [cl-struct-fastseedtype 154 16 64 29] "ISANESLE"] [cl-struct-plansys 1 9 3 1 6 29 8120 4097 [cl-struct-fastseedtype 138 1 212 229] "CEERTI"] [cl-struct-plansys 174 215 7 2 3 22 3168 4526 [cl-struct-fastseedtype 18 174 80 118] "VETEBE"] [cl-struct-plansys 147 32 0 2 11 47 22560 6803 [cl-struct-fastseedtype 18 147 212 175] "AGEBI"] [cl-struct-plansys 185 166 6 5 5 32 9216 3769 [cl-struct-fastseedtype 170 185 128 195] "GEGESO"] [cl-struct-plansys 135 160 0 7 14 64 56320 5767 [cl-struct-fastseedtype 186 135 244 75] "MARAUS"] [cl-struct-plansys 8 72 0 4 9 41 26240 3848 [cl-struct-fastseedtype 98 8 208 196] "ZALAXE"] [cl-struct-plansys 188 226 2 0 5 23 5888 4028 [cl-struct-fastseedtype 130 188 52 180] "RABEGEMA"] [cl-struct-plansys 203 195 3 7 11 55 33880 3531 [cl-struct-fastseedtype 58 203 64 226] "XEATRIEN"] [cl-struct-plansys 114 30 6 5 6 36 10368 3442 [cl-struct-fastseedtype 106 114 148 2] "XEBEUSON"] [cl-struct-plansys 159 8 0 6 13 59 47200 5791 [cl-struct-fastseedtype 50 159 208 59] "ANGERIRI"] [cl-struct-plansys 222 190 6 6 6 37 11840 3294 [cl-struct-fastseedtype 114 222 20 129] "LERIISIS"] [cl-struct-plansys 29 125 7 1 2 17 2040 5149 [cl-struct-fastseedtype 74 29 128 185] "ORRAMAOR"] [cl-struct-plansys 57 43 3 3 7 35 13720 5177 [cl-struct-fastseedtype 154 57 180 185] "ORRIENAR"] [cl-struct-plansys 234 18 2 0 7 31 7936 5866 [cl-struct-fastseedtype 130 234 80 27] "ANTIIS"] [cl-struct-plansys 80 29 5 4 4 26 8320 4432 [cl-struct-fastseedtype 226 80 116 134] "BILAIN"] [cl-struct-plansys 71 204 4 3 8 40 13440 5191 [cl-struct-fastseedtype 218 71 64 9] "ESISRELA"] [cl-struct-plansys 149 110 6 1 3 20 3200 3221 [cl-struct-fastseedtype 74 149 84 33] "LECEAN"] [cl-struct-plansys 34 219 3 2 7 34 11424 3618 [cl-struct-fastseedtype 82 34 80 35] "GELESOMA"] [cl-struct-plansys 171 103 7 2 4 26 3744 4011 [cl-struct-fastseedtype 210 171 84 52] "RASOSO"] [cl-struct-plansys 162 39 7 5 5 33 7128 3234 [cl-struct-fastseedtype 234 162 128 17] "ATANERIS"] [cl-struct-plansys 125 144 0 7 12 56 49280 5245 [cl-struct-fastseedtype 122 125 116 233] "ESTIRI"] [cl-struct-plansys 62 94 6 4 5 31 7936 3646 [cl-struct-fastseedtype 162 62 208 147] "BEERXE"] [cl-struct-plansys 199 2 2 0 8 35 8960 5575 [cl-struct-fastseedtype 66 199 180 250] "QUZABIBI"] [cl-struct-plansys 70 134 6 7 7 42 14784 3398 [cl-struct-fastseedtype 122 70 64 146] "ENONEDAR"] [cl-struct-plansys 42 56 0 5 12 54 38880 3370 [cl-struct-fastseedtype 42 42 20 194] "XEZAERTI"] [cl-struct-plansys 247 16 0 6 13 59 47200 6135 [cl-struct-fastseedtype 114 247 208 44] "INLAORAN"] [cl-struct-plansys 187 89 1 6 12 56 40320 5307 [cl-struct-fastseedtype 50 187 148 73] "ESRIXEAR"] [cl-struct-plansys 10 98 2 1 8 36 11520 5642 [cl-struct-fastseedtype 138 10 128 203] "MAEDREBE"] [cl-struct-plansys 19 14 6 3 6 34 7616 5651 [cl-struct-fastseedtype 90 19 52 91] "ANISINZA"] [cl-struct-plansys 196 236 6 0 1 11 1408 6596 [cl-struct-fastseedtype 194 196 80 46] "REONEN"] [cl-struct-plansys 223 210 2 4 10 47 24064 3039 [cl-struct-fastseedtype 162 223 244 144] "ERXEBE"] [cl-struct-plansys 135 179 3 3 9 43 16856 6279 [cl-struct-fastseedtype 26 135 64 125] "ISTITEAT"] [cl-struct-plansys 241 188 6 1 3 20 3200 4081 [cl-struct-fastseedtype 10 241 212 100] "ZAATSO"] [cl-struct-plansys 222 103 7 2 3 22 3168 5086 [cl-struct-fastseedtype 146 222 80 88] "EDRILAIN"] [cl-struct-plansys 204 213 5 2 3 20 4800 3020 [cl-struct-fastseedtype 146 204 212 64] "XEER"] [cl-struct-plansys 20 240 0 5 10 46 33120 4628 [cl-struct-fastseedtype 42 20 128 231] "SOINSOAN"] [cl-struct-plansys 187 231 7 7 7 43 11352 6587 [cl-struct-fastseedtype 58 187 244 142] "REATTE"] [cl-struct-plansys 60 124 4 4 5 29 11136 5436 [cl-struct-fastseedtype 226 60 208 234] "ARGEMA"] [cl-struct-plansys 90 203 3 0 6 28 6272 5210 [cl-struct-fastseedtype 2 90 52 201] "ESREATES"] [cl-struct-plansys 201 17 1 7 11 53 41976 5577 [cl-struct-fastseedtype 186 201 64 202] "ARCEORES"] [cl-struct-plansys 169 57 1 5 10 47 30456 5289 [cl-struct-fastseedtype 234 169 148 137] "ESDITI"] [cl-struct-plansys 151 160 0 6 13 59 47200 4247 [cl-struct-fastseedtype 178 151 208 165] "CERIANON"] [cl-struct-plansys 159 27 3 6 10 50 28000 5535 [cl-struct-fastseedtype 242 159 20 154] "QULAAON"] [cl-struct-plansys 127 143 7 1 4 25 3000 4223 [cl-struct-fastseedtype 202 127 128 101] "CELAQULA"] [cl-struct-plansys 53 90 2 3 8 38 17024 3893 [cl-struct-fastseedtype 26 53 180 4] "ZARARESO"] [cl-struct-plansys 103 206 6 0 4 23 2944 5223 [cl-struct-fastseedtype 2 103 80 201] "ESRIRE"] [cl-struct-plansys 246 46 6 4 5 31 7936 3830 [cl-struct-fastseedtype 98 246 116 35] "GEUSXE"] [cl-struct-plansys 206 98 2 3 9 42 18816 5326 [cl-struct-fastseedtype 90 206 64 121] "ORESATRA"] [cl-struct-plansys 20 241 3 1 5 25 7000 2836 [cl-struct-fastseedtype 202 20 84 176] "ERVEAN"] [cl-struct-plansys 226 123 3 2 7 34 11424 4322 [cl-struct-fastseedtype 210 226 80 21] "LABEINLA"] [cl-struct-plansys 245 108 4 2 5 27 7776 4341 [cl-struct-fastseedtype 82 245 84 213] "LAVELE"] [cl-struct-plansys 13 1 1 5 10 47 30456 4109 [cl-struct-fastseedtype 106 13 128 69] "CERASO"] [cl-struct-plansys 64 167 7 7 4 31 8184 5952 [cl-struct-fastseedtype 250 64 116 60] "TEZAXE"] [cl-struct-plansys 3 162 2 4 10 47 24064 5123 [cl-struct-fastseedtype 34 3 208 201] "ESAAN"] [cl-struct-plansys 116 59 3 0 4 20 4480 6772 [cl-struct-fastseedtype 194 116 180 31] "ONERZARA"] [cl-struct-plansys 84 100 4 7 7 40 21120 5460 [cl-struct-fastseedtype 250 84 64 138] "ARZAGEAT"] [cl-struct-plansys 241 35 3 5 8 41 20664 5361 [cl-struct-fastseedtype 170 241 20 89] "ORREEDON"] [cl-struct-plansys 127 184 0 6 13 59 47200 4479 [cl-struct-fastseedtype 242 127 208 166] "BIONBIIN"] [cl-struct-plansys 140 6 6 6 4 29 9280 3468 [cl-struct-fastseedtype 178 140 148 114] "ENCEZAIS"] [cl-struct-plansys 125 4 6 1 3 20 3200 4733 [cl-struct-fastseedtype 10 125 128 135] "SOESDIRI"] [cl-struct-plansys 158 13 5 3 6 33 9240 4510 [cl-struct-fastseedtype 218 158 52 182] "VEGEEDAT"] [cl-struct-plansys 209 184 2 0 6 27 6912 6097 [cl-struct-fastseedtype 66 209 80 236] "INLABE"] [cl-struct-plansys 149 51 3 4 7 36 16128 6293 [cl-struct-fastseedtype 34 149 244 61] "ISBIOR"] [cl-struct-plansys 29 217 1 3 9 41 20664 6173 [cl-struct-fastseedtype 154 29 64 253] "ISRAA"] [cl-struct-plansys 0 15 7 1 1 13 1560 3584 [cl-struct-fastseedtype 138 0 212 3] "GEBETI"] [cl-struct-plansys 47 23 7 2 4 26 3744 5423 [cl-struct-fastseedtype 18 47 80 90] "QULEARBI"] [cl-struct-plansys 38 42 2 2 8 37 14208 3110 [cl-struct-fastseedtype 18 38 212 241] "ATXEQU"] [cl-struct-plansys 142 90 2 5 10 48 27648 5774 [cl-struct-fastseedtype 170 142 128 43] "MARARERE"] [cl-struct-plansys 14 206 6 7 7 42 14784 3086 [cl-struct-fastseedtype 186 14 244 241] "ATAER"] [cl-struct-plansys 145 208 0 4 10 45 28800 2961 [cl-struct-fastseedtype 98 145 208 48] "ERENRA"] [cl-struct-plansys 23 84 6 0 4 23 2944 6423 [cl-struct-fastseedtype 130 23 52 254] "RIARONUS"] [cl-struct-plansys 232 127 7 7 4 31 8184 3560 [cl-struct-fastseedtype 58 232 64 210] "ENQURALE"] [cl-struct-plansys 1 244 4 5 7 38 16416 2817 [cl-struct-fastseedtype 106 1 148 48] "ERBIEDER"] [cl-struct-plansys 176 88 0 6 10 47 37600 6832 [cl-struct-fastseedtype 50 176 208 47] "AEDEDLE"] [cl-struct-plansys 129 24 0 6 11 51 40800 3713 [cl-struct-fastseedtype 114 129 20 211] "BEDILEXE"] [cl-struct-plansys 2 193 3 1 7 33 9240 3074 [cl-struct-fastseedtype 74 2 128 49] "ATLAESER"] [cl-struct-plansys 80 41 1 3 8 37 18648 6736 [cl-struct-fastseedtype 154 80 180 111] "AESARGE"] [cl-struct-plansys 3 170 2 0 8 35 8960 4611 [cl-struct-fastseedtype 130 3 80 151] "TIZAON"] [cl-struct-plansys 187 223 7 4 5 32 6144 3003 [cl-struct-fastseedtype 226 187 116 224] "TEED"] [cl-struct-plansys 116 24 0 3 9 40 22400 5236 [cl-struct-fastseedtype 218 116 64 9] "ESVERARA"] [cl-struct-plansys 180 20 6 1 2 16 2560 6836 [cl-struct-fastseedtype 74 180 84 95] "ONUSAN"] [cl-struct-plansys 195 59 3 2 8 38 12768 4803 [cl-struct-fastseedtype 82 195 80 39] "SOBIATRI"] [cl-struct-plansys 94 17 1 2 9 40 17280 4446 [cl-struct-fastseedtype 210 94 84 150] "VEBIAN"] [cl-struct-plansys 151 251 3 5 10 49 24696 5271 [cl-struct-fastseedtype 234 151 128 153] "ORINRIZA"] [cl-struct-plansys 36 94 6 7 5 34 11968 6692 [cl-struct-fastseedtype 122 36 116 175] "AENBI"] [cl-struct-plansys 231 6 6 4 6 35 8960 6887 [cl-struct-fastseedtype 162 231 208 31] "ONDIRA"] [cl-struct-plansys 66 20 6 0 3 19 2432 3906 [cl-struct-fastseedtype 66 66 180 100] "ZAANXEGE"] [cl-struct-plansys 131 98 2 7 12 58 40832 3459 [cl-struct-fastseedtype 122 131 64 162] "XEUSREOR"] [cl-struct-plansys 217 174 6 5 5 32 9216 3033 [cl-struct-fastseedtype 42 217 20 16] "ERTIUS"] [cl-struct-plansys 40 128 0 6 10 47 37600 2856 [cl-struct-fastseedtype 114 40 208 64] "ARBERI"] [cl-struct-plansys 126 83 3 6 9 46 25760 5758 [cl-struct-fastseedtype 50 126 148 187] "ANDIBIA"] [cl-struct-plansys 15 198 6 1 5 28 4480 3599 [cl-struct-fastseedtype 138 15 128 99] "GEORINAR"] [cl-struct-plansys 74 172 4 3 7 36 12096 3146 [cl-struct-fastseedtype 90 74 52 49] "ATUSZAIS"] [cl-struct-plansys 253 164 6 0 2 15 1920 5629 [cl-struct-fastseedtype 194 253 80 202] "ARINRA"] [cl-struct-plansys 106 52 4 4 7 37 14208 5482 [cl-struct-fastseedtype 162 106 244 10] "ARESON"] [cl-struct-plansys 212 31 7 3 2 19 3192 6356 [cl-struct-fastseedtype 26 212 64 157] "ISERXEER"] [cl-struct-plansys 48 2 2 1 6 28 8960 3376 [cl-struct-fastseedtype 10 48 212 194] "XERASO"] [cl-struct-plansys 159 231 7 2 4 26 3744 6047 [cl-struct-fastseedtype 146 159 80 124] "TEGEONON"] [cl-struct-plansys 159 31 7 2 4 26 3744 3487 [cl-struct-fastseedtype 146 159 212 194] "XELEZA"] [cl-struct-plansys 41 228 4 5 7 38 16416 6697 [cl-struct-fastseedtype 42 41 128 143] "AISLAXE"] [cl-struct-plansys 130 85 5 7 8 45 19800 3970 [cl-struct-fastseedtype 58 130 244 116] "RAINZA"] [cl-struct-plansys 5 68 4 4 6 33 12672 4357 [cl-struct-fastseedtype 226 5 208 150] "VEIS"] [cl-struct-plansys 245 125 7 0 1 12 1152 3829 [cl-struct-fastseedtype 2 245 52 83] "BECEDIBI"] [cl-struct-plansys 38 13 5 7 8 45 19800 5414 [cl-struct-fastseedtype 186 38 64 250] "QUREAED"] [cl-struct-plansys 120 79 7 5 3 25 5400 4728 [cl-struct-fastseedtype 234 120 148 247] "TIERUS"] [cl-struct-plansys 232 48 0 6 10 47 37600 5352 [cl-struct-fastseedtype 178 232 208 217] "ORBELAXE"] [cl-struct-plansys 130 181 5 6 7 40 16000 6018 [cl-struct-fastseedtype 242 130 20 44] "INZABEZA"] [cl-struct-plansys 164 19 3 1 5 25 7000 6308 [cl-struct-fastseedtype 202 164 128 29] "ISVEEDIN"] [cl-struct-plansys 140 152 0 3 9 40 22400 5516 [cl-struct-fastseedtype 26 140 180 250] "QUONBI"] [cl-struct-plansys 192 166 6 0 1 11 1408 4288 [cl-struct-fastseedtype 2 192 80 133] "CEMAER"] [cl-struct-plansys 161 48 0 4 10 45 28800 6305 [cl-struct-fastseedtype 98 161 116 189] "ISARE"] [cl-struct-plansys 59 238 6 3 6 34 7616 5179 [cl-struct-fastseedtype 90 59 64 185] "ORXETIBE"] [cl-struct-plansys 115 215 7 1 4 25 3000 6515 [cl-struct-fastseedtype 202 115 84 46] "REORAN"] [cl-struct-plansys 195 27 3 2 8 38 12768 5315 [cl-struct-fastseedtype 210 195 80 89] "OREDVEUS"] [cl-struct-plansys 232 86 6 2 2 17 3264 4840 [cl-struct-fastseedtype 82 232 84 119] "TILALA"] [cl-struct-plansys 66 21 5 5 7 39 14040 6210 [cl-struct-fastseedtype 106 66 128 13] "DICELASO"] [cl-struct-plansys 39 181 5 7 9 49 21560 3367 [cl-struct-fastseedtype 250 39 116 66] "XEONAR"] [cl-struct-plansys 236 138 2 4 7 35 17920 4332 [cl-struct-fastseedtype 34 236 208 149] "LAINDI"] [cl-struct-plansys 47 141 7 0 3 20 1920 5167 [cl-struct-fastseedtype 194 47 180 201] "ESSOAT"] [cl-struct-plansys 209 128 0 7 12 56 49280 5585 [cl-struct-fastseedtype 250 209 64 218] "QUDIOR"] [cl-struct-plansys 224 217 1 5 9 43 27864 4832 [cl-struct-fastseedtype 170 224 20 231] "SOLEUSER"] [cl-struct-plansys 240 104 0 6 10 47 37600 5616 [cl-struct-fastseedtype 242 240 208 250] "QURAA"] [cl-struct-plansys 143 64 0 6 13 59 47200 3983 [cl-struct-fastseedtype 178 143 148 36] "ZARAUSXE"] [cl-struct-plansys 194 168 2 1 8 36 11520 6850 [cl-struct-fastseedtype 10 194 128 95] "ONARMALA"] [cl-struct-plansys 21 235 3 3 7 35 13720 5909 [cl-struct-fastseedtype 218 21 52 204] "INREERAR"] [cl-struct-plansys 74 176 2 0 7 31 7936 4938 [cl-struct-fastseedtype 66 74 80 200] "USXELA"] [cl-struct-plansys 96 213 5 4 4 26 8320 4704 [cl-struct-fastseedtype 34 96 244 247] "TIDICE"] [cl-struct-plansys 170 133 5 3 6 33 9240 6314 [cl-struct-fastseedtype 154 170 64 93] "ISDILAON"] [cl-struct-plansys 127 149 7 1 4 25 3000 3199 [cl-struct-fastseedtype 138 127 212 161] "LEVETI"] [cl-struct-plansys 48 215 7 2 1 14 2016 6448 [cl-struct-fastseedtype 18 48 80 190] "RIBIRAOR"] [cl-struct-plansys 57 180 4 2 5 27 7776 3641 [cl-struct-fastseedtype 18 57 212 179] "BELERE"] [cl-struct-plansys 227 142 6 5 7 40 11520 3811 [cl-struct-fastseedtype 170 227 128 19] "BECETELA"] [cl-struct-plansys 21 124 4 7 8 44 23232 4629 [cl-struct-fastseedtype 186 21 244 23] "TIARED"] [cl-struct-plansys 154 216 0 4 11 49 31360 6042 [cl-struct-fastseedtype 98 154 208 28] "TEABI"] [cl-struct-plansys 242 70 6 0 3 19 2432 5106 [cl-struct-fastseedtype 130 242 52 200] "USLEANCE"] [cl-struct-plansys 133 187 3 7 9 47 28952 3461 [cl-struct-fastseedtype 58 133 64 66] "XEGEARER"] [cl-struct-plansys 16 74 2 5 8 40 23040 6416 [cl-struct-fastseedtype 106 16 148 222] "RIORUSLE"] [cl-struct-plansys 65 40 0 6 11 51 40800 3649 [cl-struct-fastseedtype 50 65 208 163] "GEDIENZA"] [cl-struct-plansys 164 242 2 6 8 41 26240 4260 [cl-struct-fastseedtype 114 164 20 165] "CETECESO"] [cl-struct-plansys 103 133 7 1 4 25 3000 5223 [cl-struct-fastseedtype 74 103 128 41] "ESVESOSO"] [cl-struct-plansys 231 167 7 3 5 31 5208 4327 [cl-struct-fastseedtype 154 231 180 165] "CERAXETE"] [cl-struct-plansys 156 194 2 0 5 23 5888 3740 [cl-struct-fastseedtype 130 156 80 147] "BEATLE"] [cl-struct-plansys 166 33 1 4 10 46 26496 5542 [cl-struct-fastseedtype 226 166 116 186] "QUGEZA"] [cl-struct-plansys 33 228 4 3 6 32 10752 5153 [cl-struct-fastseedtype 218 33 64 137] "ESAQUBE"] [cl-struct-plansys 83 58 2 1 9 40 12800 6227 [cl-struct-fastseedtype 74 83 84 29] "ISMAAN"] [cl-struct-plansys 228 27 3 2 5 26 8736 5860 [cl-struct-fastseedtype 82 228 80 171] "MAMAANAT"] [cl-struct-plansys 145 59 3 2 6 30 10080 5009 [cl-struct-fastseedtype 210 145 84 120] "EDCEA"] [cl-struct-plansys 12 79 7 5 3 25 5400 3084 [cl-struct-fastseedtype 234 12 128 161] "LEISINMA"] [cl-struct-plansys 75 172 4 7 10 52 27456 4171 [cl-struct-fastseedtype 122 75 116 245] "LADIRE"] [cl-struct-plansys 16 46 6 4 3 23 5888 5648 [cl-struct-fastseedtype 162 16 208 43] "MAARBI"] [cl-struct-plansys 61 166 6 0 2 15 1920 6461 [cl-struct-fastseedtype 66 61 180 78] "REENRI"] [cl-struct-plansys 64 190 6 7 5 34 11968 3392 [cl-struct-fastseedtype 122 64 64 50] "ENATZAUS"] [cl-struct-plansys 8 164 4 5 6 34 14688 6408 [cl-struct-fastseedtype 42 8 20 222] "RIAREROR"] [cl-struct-plansys 217 112 0 6 11 51 40800 4057 [cl-struct-fastseedtype 114 217 208 212] "RAONDILE"] [cl-struct-plansys 193 205 5 6 6 36 14400 6337 [cl-struct-fastseedtype 50 193 148 173] "DITEARRA"] [cl-struct-plansys 148 170 2 1 6 28 8960 5780 [cl-struct-fastseedtype 138 148 128 123] "ANQUARLE"] [cl-struct-plansys 1 202 2 3 8 38 17024 4609 [cl-struct-fastseedtype 90 1 52 135] "SOBETEVE"] [cl-struct-plansys 182 220 6 0 3 19 2432 4534 [cl-struct-fastseedtype 194 182 80 230] "BIORVE"] [cl-struct-plansys 117 22 6 4 4 27 6912 3957 [cl-struct-fastseedtype 162 117 244 4] "ZAERMA"] [cl-struct-plansys 161 11 3 3 7 35 13720 6305 [cl-struct-fastseedtype 26 161 64 61] "ISESUSA"] [cl-struct-plansys 239 200 2 1 9 40 12800 3055 [cl-struct-fastseedtype 10 239 212 160] "TISO"] [cl-struct-plansys 224 231 7 2 1 14 2016 3040 [cl-struct-fastseedtype 146 224 80 32] "USESEN"] [cl-struct-plansys 242 233 1 2 9 40 17280 4082 [cl-struct-fastseedtype 146 242 212 196] "ZAED"] [cl-struct-plansys 190 88 0 5 12 54 38880 4798 [cl-struct-fastseedtype 42 190 128 183] "TIREGEES"] [cl-struct-plansys 201 67 3 7 9 47 28952 5577 [cl-struct-fastseedtype 58 201 244 218] "QUSOIN"] [cl-struct-plansys 78 140 4 4 7 37 14208 3406 [cl-struct-fastseedtype 226 78 208 194] "XEISA"] [cl-struct-plansys 16 175 7 0 0 8 768 6160 [cl-struct-fastseedtype 2 16 52 93] "ISTEESGE"] [cl-struct-plansys 3 137 1 7 13 61 48312 5379 [cl-struct-fastseedtype 186 3 64 170] "ARTICESO"] [cl-struct-plansys 199 229 5 5 8 43 15480 4295 [cl-struct-fastseedtype 234 199 148 229] "CEBEOR"] [cl-struct-plansys 185 64 0 6 11 51 40800 6329 [cl-struct-fastseedtype 178 185 208 141] "DIUSACE"] [cl-struct-plansys 229 207 7 6 4 30 7200 6629 [cl-struct-fastseedtype 242 229 20 62] "RIBETIES"] [cl-struct-plansys 73 23 7 1 2 17 2040 4169 [cl-struct-fastseedtype 202 73 128 85] "LATIVEGE"] [cl-struct-plansys 99 86 6 3 6 34 7616 2915 [cl-struct-fastseedtype 26 99 180 112] "ERARRIOR"] [cl-struct-plansys 153 254 6 0 2 15 1920 3225 [cl-struct-fastseedtype 2 153 80 193] "LEEDEN"] [cl-struct-plansys 204 178 2 4 7 35 17920 4812 [cl-struct-fastseedtype 98 204 116 215] "TIVEQU"] [cl-struct-plansys 40 250 2 3 7 34 15232 5160 [cl-struct-fastseedtype 90 40 64 121] "ORANISEN"] [cl-struct-plansys 82 61 7 1 3 21 2520 5970 [cl-struct-fastseedtype 202 82 84 44] "INTEAN"] [cl-struct-plansys 36 59 3 2 5 26 8736 6180 [cl-struct-fastseedtype 210 36 80 29] "ISISAN"] [cl-struct-plansys 91 192 0 2 11 47 22560 5211 [cl-struct-fastseedtype 82 91 84 153] "ORRAES"] [cl-struct-plansys 247 169 1 5 12 55 35640 4343 [cl-struct-fastseedtype 106 247 128 85] "LAVEGERE"] [cl-struct-plansys 142 67 3 7 10 51 31416 5006 [cl-struct-fastseedtype 250 142 116 200] "USQUEN"] [cl-struct-plansys 85 242 2 4 8 39 19968 3157 [cl-struct-fastseedtype 34 85 208 225] "LEESON"] [cl-struct-plansys 106 95 7 0 2 16 1536 3690 [cl-struct-fastseedtype 194 106 180 243] "BERITERE"] [cl-struct-plansys 206 28 4 7 9 48 25344 5582 [cl-struct-fastseedtype 250 206 64 170] "ARVEAA"] [cl-struct-plansys 79 15 7 5 6 37 7992 4175 [cl-struct-fastseedtype 170 79 20 245] "LARAEDLE"] [cl-struct-plansys 225 152 0 6 11 51 40800 6625 [cl-struct-fastseedtype 242 225 208 206] "REESQUEN"] [cl-struct-plansys 18 250 2 6 10 49 31360 4370 [cl-struct-fastseedtype 178 18 148 86] "VEGEINSO"] [cl-struct-plansys 135 204 6 1 5 28 4480 4743 [cl-struct-fastseedtype 10 135 128 183] "TIMAESIN"] [cl-struct-plansys 12 73 1 3 8 37 18648 3340 [cl-struct-fastseedtype 218 12 52 98] "XEORUSGE"] [cl-struct-plansys 67 40 2 0 8 35 8960 3907 [cl-struct-fastseedtype 66 67 80 36] "ZAATI"] [cl-struct-plansys 171 247 7 4 5 32 6144 3243 [cl-struct-fastseedtype 34 171 244 49] "ATRAAT"] [cl-struct-plansys 183 177 1 3 11 49 24696 6327 [cl-struct-fastseedtype 154 183 64 61] "ISBIANRI"] [cl-struct-plansys 126 155 3 1 7 33 9240 6782 [cl-struct-fastseedtype 138 126 212 191] "ONORTI"] [cl-struct-plansys 177 23 7 2 2 18 2592 3505 [cl-struct-fastseedtype 18 177 80 162] "XEMARIIN"]] [[cl-struct-plansys 1 45 5 4 5 30 9600 5633 [cl-struct-fastseedtype 36 1 169 219] "ANESBI"] [cl-struct-plansys 204 230 6 3 3 22 4928 6348 [cl-struct-fastseedtype 90 204 11 189] "ISEROR"] [cl-struct-plansys 38 249 1 7 12 57 45144 2854 [cl-struct-fastseedtype 184 38 101 144] "ERERSO"] [cl-struct-plansys 168 103 7 1 1 13 1560 3240 [cl-struct-fastseedtype 206 168 55 193] "LEATANRE"] [cl-struct-plansys 206 59 3 1 7 33 9240 6094 [cl-struct-fastseedtype 204 206 97 220] "TEBIENLA"] [cl-struct-plansys 85 146 2 0 6 27 6912 6485 [cl-struct-fastseedtype 130 85 163 206] "REVEQU"] [cl-struct-plansys 95 218 2 4 10 47 24064 2911 [cl-struct-fastseedtype 96 95 29 240] "ERESORBE"] [cl-struct-plansys 147 121 1 6 12 56 40320 4499 [cl-struct-fastseedtype 118 147 207 54] "VEGEDIUS"] [cl-struct-plansys 136 13 5 6 5 32 12800 5512 [cl-struct-fastseedtype 116 136 25 218] "QUERBEIS"] [cl-struct-plansys 29 211 3 5 8 41 20664 3101 [cl-struct-fastseedtype 170 29 59 97] "LEATER"] [cl-struct-plansys 132 208 2 1 6 28 8960 4484 [cl-struct-fastseedtype 8 132 213 166] "BIANBIES"] [cl-struct-plansys 201 38 6 3 4 26 5824 5833 [cl-struct-fastseedtype 30 201 103 235] "MAANA"] [cl-struct-plansys 86 143 7 3 4 27 4536 2902 [cl-struct-fastseedtype 28 86 209 192] "BIIS"] [cl-struct-plansys 36 189 5 2 3 20 4800 4900 [cl-struct-fastseedtype 210 36 211 136] "USMAEDIN"] [cl-struct-plansys 12 24 0 6 10 47 37600 2828 [cl-struct-fastseedtype 176 12 141 144] "ERLABEOR"] [cl-struct-plansys 153 83 3 0 5 24 5376 3481 [cl-struct-fastseedtype 198 153 255 162] "XEERIN"] [cl-struct-plansys 255 204 6 0 4 23 2944 6143 [cl-struct-fastseedtype 196 255 137 92] "TEESCEAR"] [cl-struct-plansys 10 133 5 7 8 45 19800 5130 [cl-struct-fastseedtype 250 10 107 185] "ORESREAT"] [cl-struct-plansys 17 14 6 3 4 26 5824 5137 [cl-struct-fastseedtype 88 17 69 105] "ESUSGE"] [cl-struct-plansys 245 3 3 5 8 41 20664 3317 [cl-struct-fastseedtype 110 245 151 129] "LEQUATDI"] [cl-struct-plansys 237 241 1 5 10 47 30456 5357 [cl-struct-fastseedtype 108 237 65 89] "OREDRIER"] [cl-struct-plansys 16 126 6 4 3 23 5888 4624 [cl-struct-fastseedtype 34 16 3 199] "SOLADIAR"] [cl-struct-plansys 74 45 7 0 2 16 1536 5962 [cl-struct-fastseedtype 0 74 253 204] "INZAAN"] [cl-struct-plansys 108 91 3 2 5 26 8736 5740 [cl-struct-fastseedtype 22 108 47 11] "MAENES"] [cl-struct-plansys 39 73 1 2 10 44 19008 3367 [cl-struct-fastseedtype 20 39 249 66] "XEBEIS"] [cl-struct-plansys 20 29 7 1 1 13 1560 4116 [cl-struct-fastseedtype 74 20 155 229] "CETIAT"] [cl-struct-plansys 14 17 1 5 11 51 33048 4622 [cl-struct-fastseedtype 168 14 181 55] "TITIRIDI"] [cl-struct-plansys 45 158 6 7 6 38 13376 3629 [cl-struct-fastseedtype 190 45 199 35] "GEALE"] [cl-struct-plansys 84 65 1 7 10 49 38808 4180 [cl-struct-fastseedtype 188 84 177 133] "CEANVE"] [cl-struct-plansys 151 246 6 6 7 41 13120 5271 [cl-struct-fastseedtype 114 151 51 169] "ESVETI"] [cl-struct-plansys 87 119 7 2 4 26 3744 4183 [cl-struct-fastseedtype 80 87 109 5] "CERAERAR"] [cl-struct-plansys 10 48 0 4 11 49 31360 6666 [cl-struct-fastseedtype 102 10 95 15] "AARZARI"] [cl-struct-plansys 190 100 4 4 7 37 14208 6334 [cl-struct-fastseedtype 100 190 105 109] "DIERANOR"] [cl-struct-plansys 185 187 3 3 7 35 13720 4281 [cl-struct-fastseedtype 154 185 203 5] "CEANQUAN"] [cl-struct-plansys 187 58 2 7 12 58 40832 3515 [cl-struct-fastseedtype 248 187 37 114] "ENUSTIEN"] [cl-struct-plansys 114 151 7 1 3 21 2520 3186 [cl-struct-fastseedtype 14 114 247 113] "ATANON"] [cl-struct-plansys 76 95 7 1 1 13 1560 4428 [cl-struct-fastseedtype 12 76 33 38] "BIATGE"] [cl-struct-plansys 58 67 3 0 6 28 6272 6714 [cl-struct-fastseedtype 194 58 99 79] "ADITIRA"] [cl-struct-plansys 116 87 7 4 2 20 3840 5236 [cl-struct-fastseedtype 160 116 221 153] "ORBIRA"] [cl-struct-plansys 116 116 4 6 6 35 16800 6772 [cl-struct-fastseedtype 182 116 143 79] "AEDIS"] [cl-struct-plansys 133 253 5 6 6 36 14400 5765 [cl-struct-fastseedtype 180 133 217 187] "ANRI"] [cl-struct-plansys 122 128 0 5 12 54 38880 5242 [cl-struct-fastseedtype 234 122 251 57] "ORLAAROR"] [cl-struct-plansys 89 234 2 1 7 32 10240 4953 [cl-struct-fastseedtype 72 89 149 120] "EDANDI"] [cl-struct-plansys 194 141 5 3 6 33 9240 6082 [cl-struct-fastseedtype 94 194 39 12] "INTEARBE"] [cl-struct-plansys 147 42 2 3 10 46 20608 5523 [cl-struct-fastseedtype 92 147 145 26] "QUEDBIQU"] [cl-struct-plansys 122 134 6 2 4 25 4800 5242 [cl-struct-fastseedtype 18 122 147 217] "ORQURE"] [cl-struct-plansys 225 46 6 6 5 33 10560 5601 [cl-struct-fastseedtype 240 225 77 234] "ARQUBI"] [cl-struct-plansys 171 198 6 0 4 23 2944 5803 [cl-struct-fastseedtype 6 171 191 107] "MAISRAIS"] [cl-struct-plansys 61 244 6 0 2 15 1920 6461 [cl-struct-fastseedtype 4 61 73 14] "REONAR"] [cl-struct-plansys 216 138 2 7 9 46 32384 3544 [cl-struct-fastseedtype 58 216 43 162] "XECEON"] [cl-struct-plansys 38 127 7 3 4 27 4536 5670 [cl-struct-fastseedtype 152 38 5 171] "MAATXERI"] [cl-struct-plansys 30 34 2 5 10 48 27648 3358 [cl-struct-fastseedtype 174 30 87 146] "ENBEZA"] [cl-struct-plansys 234 132 4 5 8 42 18144 3818 [cl-struct-fastseedtype 172 234 1 67] "GEEN"] [cl-struct-plansys 213 224 0 4 10 45 28800 4821 [cl-struct-fastseedtype 98 213 195 103] "SOISQU"] [cl-struct-plansys 223 90 2 0 8 35 8960 4575 [cl-struct-fastseedtype 64 223 189 86] "VEERBIDI"] [cl-struct-plansys 173 198 6 2 3 21 4032 3757 [cl-struct-fastseedtype 86 173 239 3] "GETIUSZA"] [cl-struct-plansys 164 40 0 2 8 35 16800 4004 [cl-struct-fastseedtype 84 164 185 68] "ZAATQUAT"] [cl-struct-plansys 81 250 2 1 7 32 10240 6481 [cl-struct-fastseedtype 138 81 91 94] "RIARQUGE"] [cl-struct-plansys 99 90 2 5 11 52 29952 5219 [cl-struct-fastseedtype 232 99 117 105] "ESUSLA"] [cl-struct-plansys 134 245 5 7 8 45 19800 3974 [cl-struct-fastseedtype 254 134 135 164] "ZAINAT"] [cl-struct-plansys 17 76 4 7 8 44 23232 6673 [cl-struct-fastseedtype 252 17 113 127] "ONISAIN"] [cl-struct-plansys 204 111 7 6 3 26 6240 5324 [cl-struct-fastseedtype 178 204 243 25] "ORLATEIN"] [cl-struct-plansys 172 60 4 2 4 23 6624 6828 [cl-struct-fastseedtype 144 172 45 63] "ONUSRA"] [cl-struct-plansys 123 20 4 4 8 41 15744 4987 [cl-struct-fastseedtype 166 123 31 184] "EDSOAN"] [cl-struct-plansys 123 123 3 4 9 44 19712 6779 [cl-struct-fastseedtype 164 123 41 63] "ONVEAT"] [cl-struct-plansys 102 241 1 3 10 45 22680 6502 [cl-struct-fastseedtype 218 102 139 142] "REBITE"] [cl-struct-plansys 81 220 4 7 8 44 23232 3665 [cl-struct-fastseedtype 56 81 229 19] "BELEBI"] [cl-struct-plansys 251 166 6 1 5 28 4480 3579 [cl-struct-fastseedtype 78 251 183 226] "XEZAXEES"] [cl-struct-plansys 201 98 2 1 7 32 10240 6857 [cl-struct-fastseedtype 76 201 225 175] "AANRA"] [cl-struct-plansys 224 84 6 0 1 11 1408 3040 [cl-struct-fastseedtype 2 224 35 16] "ERZARA"] [cl-struct-plansys 137 52 4 4 6 33 12672 3721 [cl-struct-fastseedtype 224 137 157 3] "GEXEALA"] [cl-struct-plansys 21 79 7 6 4 30 7200 4885 [cl-struct-fastseedtype 246 21 79 40] "USREINXE"] [cl-struct-plansys 130 204 4 6 8 43 20640 6274 [cl-struct-fastseedtype 244 130 153 221] "ISDIDIED"] [cl-struct-plansys 152 141 5 5 5 31 11160 3480 [cl-struct-fastseedtype 42 152 187 82] "ENEDGE"] [cl-struct-plansys 46 99 3 1 7 33 9240 5422 [cl-struct-fastseedtype 136 46 85 10] "ARTELAAN"] [cl-struct-plansys 123 213 5 3 7 37 10360 6011 [cl-struct-fastseedtype 158 123 231 236] "INISBI"] [cl-struct-plansys 208 166 6 3 3 22 4928 4048 [cl-struct-fastseedtype 156 208 81 180] "RAMAA"] [cl-struct-plansys 143 175 7 2 4 26 3744 5519 [cl-struct-fastseedtype 82 143 83 106] "ARESGETI"] [cl-struct-plansys 183 163 3 6 10 50 28000 4023 [cl-struct-fastseedtype 48 183 13 4] "ZARIORTE"] [cl-struct-plansys 124 25 3 0 4 20 4480 3964 [cl-struct-fastseedtype 70 124 127 244] "RAARAN"] [cl-struct-plansys 122 251 3 0 6 28 6272 2938 [cl-struct-fastseedtype 68 122 9 0] "LAACE"] [cl-struct-plansys 101 239 7 7 5 35 9240 5477 [cl-struct-fastseedtype 122 101 235 202] "ARERAN"] [cl-struct-plansys 59 80 0 3 12 52 29120 5947 [cl-struct-fastseedtype 216 59 197 172] "INORLE"] [cl-struct-plansys 7 33 1 5 12 55 35640 3591 [cl-struct-fastseedtype 238 7 23 99] "GEINTISO"] [cl-struct-plansys 231 247 7 5 6 37 7992 6119 [cl-struct-fastseedtype 236 231 193 108] "ININLEQU"] [cl-struct-plansys 90 161 1 4 10 46 26496 4954 [cl-struct-fastseedtype 162 90 131 72] "USZASORA"] [cl-struct-plansys 116 231 7 0 0 8 768 2932 [cl-struct-fastseedtype 128 116 125 160] "TEAT"] [cl-struct-plansys 174 17 1 2 9 40 17280 6062 [cl-struct-fastseedtype 150 174 175 188] "TETEUS"] [cl-struct-plansys 33 232 0 2 9 39 18720 4385 [cl-struct-fastseedtype 148 33 121 134] "BIERED"] [cl-struct-plansys 78 56 2 1 8 36 11520 4686 [cl-struct-fastseedtype 202 78 27 23] "TIRIGE"] [cl-struct-plansys 185 3 3 5 8 41 20664 5817 [cl-struct-fastseedtype 40 185 53 91] "ANEDIN"] [cl-struct-plansys 160 44 4 7 7 40 21120 4256 [cl-struct-fastseedtype 62 160 71 229] "CEENTI"] [cl-struct-plansys 207 55 7 7 7 43 11352 5327 [cl-struct-fastseedtype 60 207 49 185] "ORUS"] [cl-struct-plansys 193 72 0 6 11 51 40800 5569 [cl-struct-fastseedtype 242 193 179 202] "ARLALE"] [cl-struct-plansys 1 97 1 2 8 36 15552 4865 [cl-struct-fastseedtype 208 1 237 56] "EDTETIIN"] [cl-struct-plansys 172 215 7 4 2 20 3840 2988 [cl-struct-fastseedtype 230 172 223 32] "CEENED"] [cl-struct-plansys 56 114 2 4 7 35 17920 2872 [cl-struct-fastseedtype 228 56 233 80] "ERTEBIRA"] [cl-struct-plansys 212 134 6 3 3 22 4928 4820 [cl-struct-fastseedtype 26 212 75 87] "TIATISBI"] [cl-struct-plansys 230 221 5 7 8 45 19800 4326 [cl-struct-fastseedtype 120 230 165 117] "LAORLAZA"] [cl-struct-plansys 68 149 7 1 1 13 1560 3652 [cl-struct-fastseedtype 142 68 119 19] "BEDICE"] [cl-struct-plansys 70 69 7 1 3 21 2520 5190 [cl-struct-fastseedtype 140 70 161 121] "ORCECE"] [cl-struct-plansys 69 197 7 0 1 12 1152 2885 [cl-struct-fastseedtype 66 69 227 16] "ERTEENON"] [cl-struct-plansys 159 114 2 4 10 47 24064 6303 [cl-struct-fastseedtype 32 159 93 45] "DIRIMA"] [cl-struct-plansys 119 11 3 6 10 50 28000 3191 [cl-struct-fastseedtype 54 119 15 193] "LEGEAN"] [cl-struct-plansys 128 123 3 6 7 38 21280 6784 [cl-struct-fastseedtype 52 128 89 63] "ONANQU"] [cl-struct-plansys 117 250 2 5 9 44 25344 5749 [cl-struct-fastseedtype 106 117 123 171] "MAANTEGE"] [cl-struct-plansys 3 60 6 1 5 28 4480 5891 [cl-struct-fastseedtype 200 3 21 92] "TETETE"] [cl-struct-plansys 244 252 4 3 5 28 9408 6388 [cl-struct-fastseedtype 222 244 167 141] "DIRILEDI"] [cl-struct-plansys 13 1 1 3 9 41 20664 6413 [cl-struct-fastseedtype 220 13 17 142] "REISORZA"] [cl-struct-plansys 100 57 1 2 7 32 13824 5732 [cl-struct-fastseedtype 146 100 19 59] "ANEDED"] [cl-struct-plansys 140 120 0 6 10 47 37600 6284 [cl-struct-fastseedtype 112 140 205 221] "ISXEIN"] [cl-struct-plansys 13 76 6 0 2 15 1920 6157 [cl-struct-fastseedtype 134 13 63 61] "ISTIXETI"] [cl-struct-plansys 183 226 2 0 8 35 8960 3255 [cl-struct-fastseedtype 132 183 201 49] "ATMALA"] [cl-struct-plansys 178 181 5 7 8 45 19800 3762 [cl-struct-fastseedtype 186 178 171 51] "BEANLE"] [cl-struct-plansys 81 129 1 3 9 41 20664 6481 [cl-struct-fastseedtype 24 81 133 110] "RELELEAT"] [cl-struct-plansys 177 1 1 5 10 47 30456 3761 [cl-struct-fastseedtype 46 177 215 243] "BEBIMA"] [cl-struct-plansys 229 75 3 5 8 41 20664 4581 [cl-struct-fastseedtype 44 229 129 214] "VEBIXE"] [cl-struct-plansys 159 194 2 4 10 47 24064 5279 [cl-struct-fastseedtype 226 159 67 105] "ESMARA"] [cl-struct-plansys 9 212 6 0 2 15 1920 5385 [cl-struct-fastseedtype 192 9 61 170] "ARUSTEA"] [cl-struct-plansys 111 60 4 2 7 35 10080 4207 [cl-struct-fastseedtype 214 111 111 53] "LAXESORI"] [cl-struct-plansys 158 135 7 2 3 22 3168 5022 [cl-struct-fastseedtype 212 158 57 8] "USRELAIN"] [cl-struct-plansys 12 213 7 1 1 13 1560 6668 [cl-struct-fastseedtype 10 12 219 15] "AATDIRE"] [cl-struct-plansys 14 13 5 5 7 39 14040 5902 [cl-struct-fastseedtype 104 14 245 12] "INUSGE"] [cl-struct-plansys 121 67 3 7 9 47 28952 4473 [cl-struct-fastseedtype 126 121 7 230] "BIGEXEIN"] [cl-struct-plansys 140 2 2 7 9 46 32384 3468 [cl-struct-fastseedtype 124 140 241 50] "ENXELETI"] [cl-struct-plansys 119 129 1 6 12 56 40320 5751 [cl-struct-fastseedtype 50 119 115 187] "ANRASOTI"] [cl-struct-plansys 87 231 7 2 4 26 3744 3415 [cl-struct-fastseedtype 16 87 173 242] "ENERQU"] [cl-struct-plansys 158 122 2 4 9 43 22016 5278 [cl-struct-fastseedtype 38 158 159 73] "ESXEAR"] [cl-struct-plansys 246 74 2 4 9 43 22016 3574 [cl-struct-fastseedtype 36 246 169 162] "XEXEAN"] [cl-struct-plansys 1 123 3 3 7 35 13720 2817 [cl-struct-fastseedtype 90 1 11 96] "ISRI"] [cl-struct-plansys 123 62 6 7 8 46 16192 4731 [cl-struct-fastseedtype 184 123 101 151] "TIATZA"] [cl-struct-plansys 77 100 6 1 3 20 3200 3917 [cl-struct-fastseedtype 206 77 55 4] "ZAVEUSGE"] [cl-struct-plansys 195 8 2 1 9 40 12800 3779 [cl-struct-fastseedtype 204 195 97 131] "GEATIAR"] [cl-struct-plansys 106 151 7 0 2 16 1536 3178 [cl-struct-fastseedtype 130 106 163 81] "ATBEA"] [cl-struct-plansys 180 15 7 4 2 20 3840 4788 [cl-struct-fastseedtype 96 180 29 23] "TIQUBIED"] [cl-struct-plansys 152 166 6 6 4 29 9280 5272 [cl-struct-fastseedtype 118 152 207 25] "OREDARIS"] [cl-struct-plansys 125 10 2 6 9 45 28800 3197 [cl-struct-fastseedtype 116 125 25 225] "LEESUSEN"] [cl-struct-plansys 18 200 0 5 12 54 38880 3858 [cl-struct-fastseedtype 170 18 59 68] "ZARILA"] [cl-struct-plansys 217 117 7 1 2 17 2040 6361 [cl-struct-fastseedtype 8 217 213 109] "DITEGERE"] [cl-struct-plansys 46 3 3 3 8 39 15288 6446 [cl-struct-fastseedtype 30 46 103 238] "RETE"] [cl-struct-plansys 75 60 4 3 8 40 13440 4683 [cl-struct-fastseedtype 28 75 209 167] "SOAXE"] [cl-struct-plansys 249 34 2 2 7 33 12672 5881 [cl-struct-fastseedtype 210 249 211 75] "MAUSDILE"] [cl-struct-plansys 97 173 5 6 6 36 14400 4705 [cl-struct-fastseedtype 176 97 141 119] "TIBIRI"] [cl-struct-plansys 94 96 2 0 7 31 7936 4190 [cl-struct-fastseedtype 198 94 255 69] "CECEES"] [cl-struct-plansys 244 169 3 0 4 20 4480 3828 [cl-struct-fastseedtype 196 244 137 163] "GEXEQUON"] [cl-struct-plansys 191 218 2 7 12 58 40832 6079 [cl-struct-fastseedtype 250 191 107 220] "TEVEBEBI"] [cl-struct-plansys 102 19 3 3 8 39 15288 2918 [cl-struct-fastseedtype 88 102 69 240] "ERES"] [cl-struct-plansys 26 192 0 5 12 54 38880 3866 [cl-struct-fastseedtype 110 26 151 68] "ZAONRIXE"] [cl-struct-plansys 226 126 6 5 6 36 10368 3042 [cl-struct-fastseedtype 108 226 65 128] "LEGECE"] [cl-struct-plansys 165 67 3 4 7 36 16128 5541 [cl-struct-fastseedtype 34 165 3 202] "ARENXEON"] [cl-struct-plansys 159 34 2 0 8 35 8960 3743 [cl-struct-fastseedtype 0 159 253 115] "BELAUS"] [cl-struct-plansys 241 72 0 2 9 39 18720 6641 [cl-struct-fastseedtype 22 241 47 110] "RESOBI"] [cl-struct-plansys 28 6 6 2 2 17 3264 5148 [cl-struct-fastseedtype 20 28 249 201] "ESINEN"] [cl-struct-plansys 137 210 2 1 7 32 10240 5001 [cl-struct-fastseedtype 74 137 155 72] "USZAVE"] [cl-struct-plansys 99 118 6 5 7 40 11520 6499 [cl-struct-fastseedtype 168 99 181 126] "RIEDANEN"] [cl-struct-plansys 18 59 3 7 10 51 31416 4370 [cl-struct-fastseedtype 190 18 199 166] "BIRARE"] [cl-struct-plansys 73 174 6 7 6 38 13376 5961 [cl-struct-fastseedtype 188 73 177 236] "INZAAN"] [cl-struct-plansys 236 27 3 6 7 38 21280 6124 [cl-struct-fastseedtype 114 236 51 236] "INBEIN"] [cl-struct-plansys 172 204 4 2 4 23 6624 6060 [cl-struct-fastseedtype 80 172 109 108] "INCEISA"] [cl-struct-plansys 79 253 5 4 7 38 12160 3407 [cl-struct-fastseedtype 102 79 95 50] "ENONLEBE"] [cl-struct-plansys 179 1 1 4 11 50 28800 4019 [cl-struct-fastseedtype 100 179 105 52] "RAESERRE"] [cl-struct-plansys 238 208 0 3 11 48 26880 5102 [cl-struct-fastseedtype 154 238 203 168] "USUSONER"] [cl-struct-plansys 16 255 7 7 4 31 8184 5136 [cl-struct-fastseedtype 248 16 37 121] "ORESRATI"] [cl-struct-plansys 23 20 6 1 5 28 4480 3863 [cl-struct-fastseedtype 14 23 247 180] "RAIN"] [cl-struct-plansys 65 172 6 1 3 20 3200 6209 [cl-struct-fastseedtype 12 65 33 205] "DIQUUS"] [cl-struct-plansys 79 200 2 0 8 35 8960 3407 [cl-struct-fastseedtype 194 79 99 210] "ENARINES"] [cl-struct-plansys 201 12 4 4 6 33 12672 3017 [cl-struct-fastseedtype 160 201 221 192] "TILE"] [cl-struct-plansys 121 33 1 6 10 48 34560 3449 [cl-struct-fastseedtype 182 121 143 50] "ENDIQU"] [cl-struct-plansys 122 122 2 6 10 49 31360 3450 [cl-struct-fastseedtype 180 122 217 194] "XETILA"] [cl-struct-plansys 111 245 5 5 8 43 15480 5999 [cl-struct-fastseedtype 234 111 251 28] "TEXEARE"] [cl-struct-plansys 174 15 7 1 3 21 2520 6830 [cl-struct-fastseedtype 72 174 149 63] "ONTEAR"] [cl-struct-plansys 39 234 2 3 10 46 20608 6695 [cl-struct-fastseedtype 94 39 39 15] "ALETIUS"] [cl-struct-plansys 136 87 7 3 2 19 3192 3208 [cl-struct-fastseedtype 92 136 145 1] "LELEMAA"] [cl-struct-plansys 79 107 3 2 8 38 12768 5967 [cl-struct-fastseedtype 18 79 147 156] "TETIGE"] [cl-struct-plansys 54 67 3 6 9 46 25760 3126 [cl-struct-fastseedtype 240 54 77 209] "ATMABE"] [cl-struct-plansys 112 83 3 0 4 20 4480 6512 [cl-struct-fastseedtype 6 112 191 14] "REENATEN"] [cl-struct-plansys 50 81 3 0 6 28 6272 4146 [cl-struct-fastseedtype 4 50 73 85] "LAEDON"] [cl-struct-plansys 141 95 7 7 5 35 9240 4237 [cl-struct-fastseedtype 58 141 43 197] "CEENZA"] [cl-struct-plansys 123 4 4 3 8 40 13440 3451 [cl-struct-fastseedtype 152 123 5 50] "ENENONGE"] [cl-struct-plansys 67 95 7 5 6 37 7992 4163 [cl-struct-fastseedtype 174 67 87 85] "LAEDAT"] [cl-struct-plansys 223 145 1 5 12 55 35640 5599 [cl-struct-fastseedtype 172 223 1 106] "ARANCE"] [cl-struct-plansys 106 37 5 4 6 34 10880 5482 [cl-struct-fastseedtype 98 106 195 106] "ARQUA"] [cl-struct-plansys 52 207 7 0 0 8 768 6196 [cl-struct-fastseedtype 64 52 189 253] "ISLEBEEN"] [cl-struct-plansys 50 51 3 2 7 34 11424 4402 [cl-struct-fastseedtype 86 50 239 102] "BIINCEOR"] [cl-struct-plansys 153 101 5 2 4 24 5760 5785 [cl-struct-fastseedtype 84 153 185 203] "MAARABI"] [cl-struct-plansys 198 47 7 1 3 21 2520 3270 [cl-struct-fastseedtype 138 198 91 193] "LETIONED"] [cl-struct-plansys 184 63 7 5 3 25 5400 3000 [cl-struct-fastseedtype 232 184 117 176] "ERESEN"] [cl-struct-plansys 107 18 2 7 12 58 40832 4715 [cl-struct-fastseedtype 254 107 135 39] "SOCEORBI"] [cl-struct-plansys 6 57 1 7 12 57 45144 4358 [cl-struct-fastseedtype 252 6 113 230] "BIBIRALE"] [cl-struct-plansys 33 20 4 6 7 39 18720 5921 [cl-struct-fastseedtype 178 33 243 92] "TEENATLE"] [cl-struct-plansys 1 17 1 2 8 36 15552 4353 [cl-struct-fastseedtype 144 1 45 166] "BIORLE"] [cl-struct-plansys 192 97 1 4 8 38 21888 5824 [cl-struct-fastseedtype 166 192 31 219] "ANTEED"] [cl-struct-plansys 112 152 0 4 9 41 26240 4464 [cl-struct-fastseedtype 164 112 41 6] "BIABI"] [cl-struct-plansys 155 134 6 3 6 34 7616 3227 [cl-struct-fastseedtype 218 155 139 49] "ATBELE"] [cl-struct-plansys 166 33 1 7 12 57 45144 5542 [cl-struct-fastseedtype 56 166 229 26] "QUXEGE"] [cl-struct-plansys 160 163 3 1 5 25 7000 4256 [cl-struct-fastseedtype 78 160 183 37] "CEESARI"] [cl-struct-plansys 190 47 7 1 3 21 2520 4542 [cl-struct-fastseedtype 76 190 225 86] "VEZAORLA"] [cl-struct-plansys 245 89 3 0 5 24 5376 3829 [cl-struct-fastseedtype 2 245 35 147] "BELEES"] [cl-struct-plansys 222 105 1 4 10 46 26496 5598 [cl-struct-fastseedtype 224 222 157 42] "ARBETEQU"] [cl-struct-plansys 26 124 4 6 8 43 20640 5658 [cl-struct-fastseedtype 246 26 79 11] "MAGEESTI"] [cl-struct-plansys 119 201 1 6 12 56 40320 3959 [cl-struct-fastseedtype 244 119 153 228] "ZABIXEDI"] [cl-struct-plansys 141 130 2 5 9 44 25344 4237 [cl-struct-fastseedtype 42 141 187 53] "LACEUS"] [cl-struct-plansys 131 8 2 1 9 40 12800 3203 [cl-struct-fastseedtype 136 131 85 209] "ATISEN"] [cl-struct-plansys 224 178 2 3 7 34 15232 6880 [cl-struct-fastseedtype 158 224 231 239] "AXEBE"] [cl-struct-plansys 197 83 3 3 7 35 13720 5829 [cl-struct-fastseedtype 156 197 81 155] "ANRARA"] [cl-struct-plansys 100 20 4 2 4 23 6624 6244 [cl-struct-fastseedtype 82 100 83 45] "DIBIEDIN"] [cl-struct-plansys 12 56 0 6 10 47 37600 5644 [cl-struct-fastseedtype 48 12 13 235] "MAABILE"] [cl-struct-plansys 65 38 6 0 2 15 1920 4673 [cl-struct-fastseedtype 70 65 127 151] "TIONED"] [cl-struct-plansys 111 216 2 0 8 35 8960 4719 [cl-struct-fastseedtype 68 111 9 71] "SOREZAQU"] [cl-struct-plansys 26 68 4 7 9 48 25344 6170 [cl-struct-fastseedtype 122 26 235 237] "DIDILAER"] [cl-struct-plansys 144 85 5 3 4 25 7000 3728 [cl-struct-fastseedtype 216 144 197 51] "BEQURI"] [cl-struct-plansys 44 222 6 5 4 28 8064 4396 [cl-struct-fastseedtype 238 44 23 38] "BIATZATE"] [cl-struct-plansys 220 132 4 5 6 34 14688 3804 [cl-struct-fastseedtype 236 220 193 147] "BELABIA"] [cl-struct-plansys 239 102 6 4 6 35 8960 5871 [cl-struct-fastseedtype 162 239 131 75] "MALETEES"] [cl-struct-plansys 201 220 6 0 2 15 1920 4809 [cl-struct-fastseedtype 128 201 125 71] "SODIRI"] [cl-struct-plansys 51 254 6 2 5 29 5568 6707 [cl-struct-fastseedtype 150 51 175 31] "ONATCE"] [cl-struct-plansys 22 165 5 2 5 28 6720 6166 [cl-struct-fastseedtype 148 22 121 13] "DIESDI"] [cl-struct-plansys 195 237 7 1 4 25 3000 5571 [cl-struct-fastseedtype 202 195 27 122] "QUMAUS"] [cl-struct-plansys 14 104 0 5 12 54 38880 3342 [cl-struct-fastseedtype 40 14 53 162] "XEORESCE"] [cl-struct-plansys 133 201 1 7 11 53 41976 4997 [cl-struct-fastseedtype 62 133 71 104] "USTIZA"] [cl-struct-plansys 196 164 4 7 7 40 21120 3012 [cl-struct-fastseedtype 60 196 49 32] "ESDI"] [cl-struct-plansys 22 109 5 6 7 40 16000 6166 [cl-struct-fastseedtype 242 22 179 13] "DIENVE"] [cl-struct-plansys 86 182 6 2 4 25 4800 6742 [cl-struct-fastseedtype 208 86 237 159] "ONDIZAAT"] [cl-struct-plansys 241 164 4 4 6 33 12672 3825 [cl-struct-fastseedtype 230 241 223 67] "GEQUADI"] [cl-struct-plansys 45 15 7 4 3 24 4608 4653 [cl-struct-fastseedtype 228 45 233 23] "TILAANES"] [cl-struct-plansys 9 155 3 3 7 35 13720 5385 [cl-struct-fastseedtype 26 9 75 250] "QURIXEAN"] [cl-struct-plansys 59 162 2 7 12 58 40832 5947 [cl-struct-fastseedtype 120 59 165 124] "TEQUENES"] [cl-struct-plansys 233 18 2 1 7 32 10240 4585 [cl-struct-fastseedtype 142 233 119 86] "VEENEN"] [cl-struct-plansys 59 146 2 1 9 40 12800 2875 [cl-struct-fastseedtype 140 59 161 32] "REAR"] [cl-struct-plansys 90 74 2 0 7 31 7936 3674 [cl-struct-fastseedtype 66 90 227 147] "BEORSORA"] [cl-struct-plansys 244 39 7 4 2 20 3840 4084 [cl-struct-fastseedtype 32 244 93 84] "RAAED"] [cl-struct-plansys 124 184 0 6 10 47 37600 3964 [cl-struct-fastseedtype 54 124 15 164] "ZAEDED"] [cl-struct-plansys 117 248 0 6 11 51 40800 4469 [cl-struct-fastseedtype 52 117 89 70] "BIRAA"] [cl-struct-plansys 106 111 7 5 5 33 7128 6506 [cl-struct-fastseedtype 106 106 123 142] "REUSLEED"] [cl-struct-plansys 88 97 3 1 5 25 7000 3672 [cl-struct-fastseedtype 200 88 21 35] "GEISOR"] [cl-struct-plansys 89 89 1 3 9 41 20664 2905 [cl-struct-fastseedtype 222 89 167 144] "ERGEREXE"] [cl-struct-plansys 2 46 6 3 5 30 6720 4098 [cl-struct-fastseedtype 220 2 17 117] "LABIRIOR"] [cl-struct-plansys 57 30 6 2 3 21 4032 6457 [cl-struct-fastseedtype 146 57 19 254] "RILADI"] [cl-struct-plansys 225 141 5 6 6 36 14400 4065 [cl-struct-fastseedtype 112 225 205 196] "ZABEOR"] [cl-struct-plansys 210 217 3 0 6 28 6272 3026 [cl-struct-fastseedtype 134 210 63 224] "INONIN"] [cl-struct-plansys 172 63 7 0 0 8 768 5036 [cl-struct-fastseedtype 132 172 201 120] "EDZAAR"] [cl-struct-plansys 103 138 2 7 12 58 40832 4455 [cl-struct-fastseedtype 186 103 171 86] "VEUSBI"] [cl-struct-plansys 166 6 6 3 5 30 6720 4262 [cl-struct-fastseedtype 24 166 133 245] "LAXERIVE"] [cl-struct-plansys 214 62 6 5 6 36 10368 4566 [cl-struct-fastseedtype 46 214 215 182] "VEMAED"] [cl-struct-plansys 218 88 0 5 12 54 38880 6362 [cl-struct-fastseedtype 44 218 129 253] "ISASO"] [cl-struct-plansys 52 7 7 4 2 20 3840 5940 [cl-struct-fastseedtype 226 52 67 108] "INUSES"] [cl-struct-plansys 94 73 3 0 6 28 6272 3166 [cl-struct-fastseedtype 192 94 61 81] "ATORESRA"] [cl-struct-plansys 244 169 1 2 7 32 13824 5108 [cl-struct-fastseedtype 214 244 111 152] "EDTIZABE"] [cl-struct-plansys 147 196 4 2 7 35 10080 6803 [cl-struct-fastseedtype 212 147 57 143] "ASOARLE"] [cl-struct-plansys 129 10 2 1 7 32 10240 3457 [cl-struct-fastseedtype 10 129 219 114] "ENRIENGE"] [cl-struct-plansys 99 242 2 5 11 52 29952 3683 [cl-struct-fastseedtype 104 99 245 83] "BEES"] [cl-struct-plansys 94 96 0 7 13 60 52800 5214 [cl-struct-fastseedtype 126 94 7 105] "ESUSALE"] [cl-struct-plansys 129 239 7 7 5 35 9240 5249 [cl-struct-fastseedtype 124 129 241 153] "ORMABIIN"] [cl-struct-plansys 204 38 6 6 4 29 9280 6604 [cl-struct-fastseedtype 50 204 115 254] "RIATTEIN"] [cl-struct-plansys 172 188 4 2 4 23 6624 5292 [cl-struct-fastseedtype 16 172 173 89] "ORLESO"] [cl-struct-plansys 227 199 7 4 5 32 6144 6115 [cl-struct-fastseedtype 38 227 159 108] "INTISO"]]])

(defvar elite-for-emacs-logo 
"               /\\         *** *   * *** ***         /\\
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
                                  ^")

(defvar  elite-for-emacs-logo-no-text 
"               /\\                                   /\\
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
________________\\     /   /__|-       -|__\\   \\    /________________
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
                                  ^")
