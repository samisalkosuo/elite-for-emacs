;;; elite-description-functions.el -  Functions for planet descriptions in Emacs-Elite

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
  

;variables for planer descriptions
(defvar desc_list
  ;vector of vectors
  (vector 
   (vector "fabled" "notable" "well known" "famous" "noted")
   (vector "very" "mildly" "most" "reasonably" "")
   (vector "ancient"  "?\x95"  "great"  "vast"  "pink")
   (vector "?\x9E ?\x9D plantations"  "mountains"  "?\x9C"  "?\x94 forests"  "oceans")
   (vector "shyness"  "silliness"  "mating traditions"  "loathing of ?\x86"  "love for ?\x86")
   (vector "food blenders"  "tourists"  "poetry"  "discos"  "?\x8E")
   (vector "talking tree"  "crab"  "bat"  "lobst"  "?\xB2")
   (vector "beset"  "plagued"  "ravaged"  "cursed"  "scourged")
   (vector "?\x96 civil war"  "?\x9B ?\x98 ?\x99s"  "a ?\x9B disease"  "?\x96 earthquakes"  "?\x96 solar activity")
   (vector "its ?\x83 ?\x84"  "the ?\xB1 ?\x98 ?\x99" "its inhabitants' ?\x9A ?\x85"  "?\xA1"  "its ?\x8D ?\x8E")
   (vector "juice"  "brandy"  "water"  "brew"  "gargle blasters")
   (vector "?\xB2"  "?\xB1 ?\x99"  "?\xB1 ?\xB2"  "?\xB1 ?\x9B"  "?\x9B ?\xB2")
   (vector "fabulous"  "exotic"  "hoopy"  "unusual"  "exciting")
   (vector "cuisine"  "night life"  "casinos"  "sit coms"  " ?\xA1 ")
   (vector "?\xB0"  "The planet ?\xB0"  "The world ?\xB0"  "This planet"  "This world")
   (vector "n unremarkable"  " boring"  " dull"  " tedious"  " revolting")
   (vector "planet"  "world"  "place"  "little planet"  "dump")
   (vector "wasp"  "moth"  "grub"  "ant"  "?\xB2")
   (vector "poet"  "arts graduate"  "yak"  "snail"  "slug")
   (vector "tropical"  "dense"  "rain"  "impenetrable"  "exuberant")
   (vector "funny"  "wierd"  "unusual"  "strange"  "peculiar")
   (vector "frequent"  "occasional"  "unpredictable"  "dreadful"  "deadly")
   (vector "?\x82 ?\x81 for ?\x8A"  "?\x82 ?\x81 for ?\x8A and ?\x8A"  "?\x88 by ?\x89"  "?\x82 ?\x81 for ?\x8A but ?\x88 by ?\x89" "a ?\x90 ?\x91")
   (vector "?\x9B"  "mountain"  "edible"  "tree"  "spotted")
   (vector "?\x9F"  "?\xA0"  "?\x87oid"  "?\x93"  "?\x92")
   (vector "ancient"  "exceptional"  "eccentric"  "ingrained"  "?\x95")
   (vector "killer"  "deadly"  "evil"  "lethal"  "vicious")
   (vector "parking meters"  "dust clouds"  "ice bergs"  "rock formations"  "volcanoes")
   (vector "plant"  "tulip"  "banana"  "corn"  "?\xB2weed")
   (vector "?\xB2"  "?\xB1 ?\xB2"  "?\xB1 ?\x9B"  "inhabitant"  "?\xB1 ?\xB2")
   (vector "shrew"  "beast"  "bison"  "snake"  "wolf")
   (vector "leopard"  "cat"  "monkey"  "goat"  "fish")
   (vector "?\x8C ?\x8B"  "?\xB1 ?\x9F ?\xA2" "its <12> ?\xA0 ?\xA2"  "?\xA3 ?\xA6"  "?\x8C ?\x8B")
   (vector "meat"  "cutlet"  "steak"  "burgers"  "soup")
   (vector "ice"  "mud"  "Zero-G"  "vacuum"  "?\xB1 ultra")
   (vector "hockey"  "cricket"  "karate"  "polo"  "tennis")
   ))

(defvar rnd_seed 0
  "Seed variable used when generating planet description.")


;; (defun elite-planet-description (plsy)
;;   "Return description of planet."
;;   (let (
;; 
;; 	)
;;     (setq rnd_seed (plansys-goatsoupseed))
;;     
;; 
;; ))
;; 
;; (defun gen_rnd_number ()
;;   
;;   (let
;;       (
;;        (a)
;;        (x)
;;        )
;;     (setq x (logand (* 2 (fastseedtype-a rnd_seed)) 255 ))
;;     (setq a (+ x (fastseedtype-c)))
;;     (if (> (fastseedtype-a rnd_seed) 127)
;; 	(setq a (1+ a)))
;;     (setf (fastseedtype-a rnd_seed) (logand a 255))
;;     (setf (fastseedtype-c rnd_seed) x)
;;     
;;     (setq a (/ a 256));a = any carry left from above
;;     (setq x (fastseedtype-b rnd_seed))
;;     (setq a (logand (+ a x (fastseedtype-d rnd_seed)) 255))
;;     (setf (fastseedtype-b rnd_seed) a)
;;     (setf (fastseedtype-d rnd_seed) x)
;;     (max a)
;;     ))
;; 
;; ;		rnd_seed = plsy.goatsoupseed;
;; ;		printf("\n");goat_soup("?\x8F is ?\x97.",&plsy);
;; 
;; (defun goat_soup (source psy)
;;   "Generates description."
;;   (let (
;; 	(c)
;; 	(tmp)
;; 	)
;;     (setq tmp (car (split-string source " ")))
;;     (if (tmp
;;     
;; ))
;; 
;; 
;; void goat_soup(const char *source,plansys * psy)
;; {	for(;;)
;; 	{	int c=*(source++);
;; 		if(c=='\0')	break;
;; 		if(c<0x80) printf("%c",c);
;; 		else
;; 		{	if (c <=0xA4)
;; 			{	int rnd = gen_rnd_number();
;; 				goat_soup(desc_list[c-0x81].option[(rnd >= 0x33)+(rnd >= 0x66)+(rnd >= 0x99)+(rnd >= 0xCC)],psy);
;; 			}
;; 			else switch(c)
;; 			{ case 0xB0: /* planet name */
;; 		 		{ int i=1;
;; 					printf("%c",psy->name[0]);
;; 					while(psy->name[i]!='\0') printf("%c",tolower(psy->name[i++]));
;; 				}	break;
;; 				case 0xB1: /* <planet name>ian */
;; 				{ int i=1;
;; 					printf("%c",psy->name[0]);
;; 					while(psy->name[i]!='\0')
;; 					{	if((psy->name[i+1]!='\0') || ((psy->name[i]!='E')	&& (psy->name[i]!='I')))
;; 						printf("%c",tolower(psy->name[i]));
;; 						i++;
;; 					}
;; 					printf("ian");
;; 				}	break;
;; 				case 0xB2: /* random name */
;; 				{	int i;
;; 					int len = gen_rnd_number() & 3;
;; 					for(i=0;i<=len;i++)
;; 					{	int x = gen_rnd_number() & 0x3e;
;; 						if(pairs0[x]!='.') printf("%c",pairs0[x]);
;; 						if(i && (pairs0[x+1]!='.')) printf("%c",pairs0[x+1]);
;; 					}
;; 				}	break;
;; 				default: printf("<bad char in data [%X]>",c); return;
;; 			}	/* endswitch */
;; 		}	/* endelse */
;; 	}	/* endwhile */
;; }	/* endfunc */
;; 
;; /**+end **/
;; 
;; /* B0 = <planet name>
;; 	 B1 = <planet name>ian
;; 	 B2 = <random name>
;; */
;; 
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
