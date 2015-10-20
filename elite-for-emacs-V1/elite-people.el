;;; elite-people.el -  People in Elite universe

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
;Variables and functions for people in Elite universe

;names (not including AI commanders) are
;generated from these names

(defvar elite-total-people-in-galaxy 300
  "Number of people in galaxy.")
;(setq elite-total-people-in-galaxy 300)

(defconst PERSON_CITIZEN 0);person is normal citizen
(defconst PERSON_PETTY_CRIMINAL 1);person is small time crook, pick pockets etc
(defconst PERSON_CRIMINAL 2);person is criminal, bank robberies, drug dealer etc
(defconst PERSON_SPY 3);person is a spy, spies for other governments or big corporations
(defconst PERSON_NAVY_OFFICER 4);person is office of Galactic Navy
(defconst PERSON_NAVY_TRAITOR 5);person is traitor of Galactic Navy
(defconst PERSON_MAFIA_MEMBER 6);person is part of organized crime
(defconst PERSON_RELIGIOUS 7);person is part of some religious faction
(defconst PERSON_ROYALTY 8);person is royal person
(defconst PERSON_ALIEN 9);person is in reality an alien outside The Eight Galaxies

(defvar elite-non-citizen-professions
;(setq elite-non-citizen-professions
  (vector 
   PERSON_PETTY_CRIMINAL
   PERSON_CRIMINAL
   PERSON_SPY
   PERSON_NAVY_OFFICER
   PERSON_NAVY_TRAITOR
   PERSON_MAFIA_MEMBER
   PERSON_RELIGIOUS
   PERSON_ROYALTY
   PERSON_ALIEN
   )
  )
 

;elite-person attributes
(defstruct elite-person
  first-name
  last-name
  gender
  name;full name
  profession
  galaxy
  home-planet
  current-planet 
  )

(defvar elite-people-names (list)
  "All elite people in universe"
  )

(defvar elite-people-in-universe (make-vector 8 (list))
  "Vector of people in galaxy 1-8."
  )
;(setq elite-people-in-universe nil)

(defvar elite-people-out-of-loop (list)
;(setq elite-people-out-of-loop nil)
  "list of people that are no longer in normal
   interaction. for example, people that are in prison, captured or dead")


(defun elite-list-people-in-system (system &optional delimiter)
  "return string of people in system."
  (let (
	(people)
	(person)
	(names)
	(delim)
	)
    (if delimiter
	(setq delim delimiter)
      (setq delim "\n")
      )
    (setq names nil)
    ;(setq system currentplanet)
    (setq people (elite-get-people-in-system system))
    (while people
      (setq person (car people))
      (setq names (concat names (elite-person-name person)
			  delim))
      
      (setq people (cdr people))
      )
    names
    )
  )

(defun elite-get-people-in-system (system)
  "Return list of people in system."
;(elite-get-people-in-system 0)
  (let (
	(persons-in-galaxy)
	(i)
	(len)
	(persons-in-system)
	(person)
	)
    (setq persons-in-system nil)
    (setq persons-in-galaxy (aref elite-people-in-universe (1- galaxynum)))
    (setq len (length persons-in-galaxy))
    (setq i 0)
    (while (< i len)
      (setq person (aref persons-in-galaxy i))
      (if (= (elite-person-current-planet person) system)
	  (setq persons-in-system (append persons-in-system (vector person) nil))
	  )

      (setq i (1+ i))
      )
    persons-in-system    
    )
  )

(defun elite-find-person (name)
  "Find system where person is currently"
  ;;for debug
  (let (
	(i)
	(j)
	(person)
	(people-in-galaxy)
	(system-name)
	)
    ;(setq profession PERSON_NAVY_OFFICER)
    ;(setq people-by-profession nil)
     ;(setq name "aleksis paasikivi")
    ;(setq people-in-galaxy (aref elite-people-in-universe 4))
      (setq people-in-galaxy (aref elite-people-in-universe (1- galaxynum)))
      (setq i 0)
      (while (< i elite-total-people-in-galaxy)
	;loop through people
	(setq person (aref people-in-galaxy i))
	
	(if (string= (downcase name) (downcase (elite-person-name person)))
	    (setq system-name  (elite-get-system-name (elite-person-current-planet person)))
	    )
	(setq i (1+ i))
      )
    system-name

    )

)

(defun elite-get-people-by-profession (profession)
  "Returns list of people that have specified profession."
  (let (
	(i)
	(j)
	(person)
	(people-in-galaxy)
	(people-by-profession)
	)
    ;(setq profession PERSON_NAVY_OFFICER)
    ;(setq people-by-profession nil)
    (setq j 0)
    (while (< j 8)
      ;loop through galaxies
      (setq people-in-galaxy (aref elite-people-in-universe j))
      (setq i 0)
      (while (< i elite-total-people-in-galaxy)
	;loop through people
	(setq person (aref people-in-galaxy i))
	
	(if (= (elite-person-profession person) profession)
	    (setq people-by-profession (append people-by-profession (vector person) nil))
	    )

	(setq i (1+ i))
      )
      (setq j (1+ j))
      )
    people-by-profession

    )
  )

(defun elite-person-out-of-loop (person)
  "return true person is out of loop"
  (let (
	(out)
	(tmp)
	(out-of-loop)
	)
    (setq out-of-loop nil)
    ;if person is out of loop return t
    (setq out elite-people-out-of-loop)
    (while out
      (setq tmp (car out))
      (if (string= (elite-person-name person) (elite-person-name tmp))
	  (setq out-of-loop t)
	  )
      (setq out (cdr out))
      )
    out-of-loop
    )
  )

(defun elite-update-person-current-planet (name)
  "updates persons current system"
  (let (
	(persons-in-galaxy)
	(i)
	(len)
	(persons-in-system)
	(person)
	(current-person)
	)
    (setq persons-in-system nil)
    (setq persons-in-galaxy (aref elite-people-in-universe (1- galaxynum)))
    (setq len (length persons-in-galaxy))
    (setq i 0)
    (while (< i len)
      (setq person (aref persons-in-galaxy i))
      (if (string= (elite-person-name person) name)
	  (progn
	    (setf (elite-person-current-planet person) currentplanet)
	    (setq current-person person)
	    )
	  (aset persons-in-galaxy i person)
	  )
      (setq i (1+ i))
      )
    (aset elite-people-in-universe galaxynum persons-in-galaxy)
    (= (elite-person-current-planet current-person) (elite-person-home-planet current-person))
    )
  )

(defun elite-initialize-people ()
  "Initializes people in elite universe."
  (let (
	(i)
	(j)
	(k)
	(fname-index)
	(first-name)
	(last-name)
	(name)
	(gender)
	(persons-in-galaxy)
	(progress)
	(percent)
	(prog-text)
	(one-percent)
	(profession)
	(debug-citizens);for debuggin people professions in message buffer
	(citizens)
	(non-citizens)
	(msge)
	)
    (setq progress 0)
    (setq prog-text "Initializing universe")
    (setq one-percent (/ (* 8 elite-total-people-in-galaxy) 100.0))
    
    (setq debug-citizens nil)
    (setq citizens 0)
    (setq non-citizens 0)
    (setq msge "")


    (setq j 0)
    (while (< j 8);eight galaxies
      (setq persons-in-galaxy (make-vector elite-total-people-in-galaxy nil))
      (setq i 0)
      (while (< i elite-total-people-in-galaxy)
	(setq fname-index (random (length elite-first-names)))
	(setq first-name (nth  fname-index elite-first-names))
	(setq last-name (nth (random (length elite-last-names)) elite-last-names))
	(setq gender (nth fname-index elite-first-name-gender))
	(setq name (concat  first-name " " last-name))
	;(setq elite-people-names nil)
	(setq k 0)
	;not so much same names..
	(while (and (member name elite-people-names) (< k 20))
	  (setq fname-index (random (length elite-first-names)))
	  (setq first-name (nth  fname-index elite-first-names))
	  (setq last-name (nth (random (length elite-last-names)) elite-last-names))
	  (setq gender (nth fname-index elite-first-name-gender))
	  (setq name (concat  first-name " " last-name))
	  (setq k (1+ k))
	  )
	(setq elite-people-names (append elite-people-names (list name)))


	(if (> (elite-rand1 t) 235)
	;(if (> (elite-rand1 t) 255);testing
	    (progn
	      
	      ;person is other than ordinary citizen	      
	      (setq profession (aref elite-non-citizen-professions (random (length elite-non-citizen-professions))))
	      (if debug-citizens
		  (progn
		    (setq msge (concat msge name ": " profession " "))
		    (setq non-citizens (1+ non-citizens))
		    )
		)
	      )
	  (progn
	    ;person is citizen
	    (setq profession PERSON_CITIZEN)
	    ;(setq profession PERSON_NAVY_OFFICER);testing special mission 1
	    (setq citizens (1+ citizens))
	    )
	  )


	(aset persons-in-galaxy i (make-elite-person
				   :first-name first-name
				   :last-name last-name
				   :gender gender
				   :name name
				   :profession profession
				   :galaxy j
				   :home-planet (random galsize)
				   :current-planet (random galsize)
				   )
	      )       
	(setq i (1+ i))

	;progress
	(setq progress (1+ progress))
	(if (= (% progress 1) 0)
	    (progn 
	      (setq percent (format "%.1f" (/ progress one-percent)))
	     ;(setq prog-text (concat prog-text "."))
	      (message (concat prog-text " " percent "%%"))
	      ;(message (concat prog-text " " percent "%%" "(2/2)"))
	      )
	  )
	)
      (aset elite-people-in-universe j persons-in-galaxy)
      (setq j (1+ j))
      )
    (if debug-citizens
	(message (concat "citizens: " citizens " non-citizens: " non-citizens "\n" msge))
      )    
    )
  )

;first name genders
;0=female
;1=male
(defvar elite-first-name-gender
  (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 0 0 0 0 1 1 0 0))

;first and last names in elite universe
(defvar elite-first-names
;(setq elite-first-names
  (list 
   "Odin"
   "Jean"
   "Lauri"
   "Pehr"
   "Kyosti"
   "Risto"
   "Carl"
   "Juho"
   "Urho"
   "Mauno"
   "Martti"
   "Kaarlo"
   "Elias"
   "Edvard"
   "Eino"
   "Georg"
   "Paavo"
   "Alvar"
   "Jalmari"
   "Erik"
   "Sven"
   "Anders"
   "Akseli"
   "Johan"
   "Helene"
   "Albert"
   "Ellen"
   "Magnus"
   "Minna"
    "Uno"
   "Hugo"
   "Fredrik"
   "Eino"
   "Zacharias"
   "Aleksis"
  "Maria"
   "Elin"
   "Juhana"
   "Pentti"
   "Adolf"
   "Tarja"
   "Johannes"
   "Veikko"
   "Juhani"
   "Onni"
   "Heikki"
   "Oskari"
   "Helena"
   "Myrna"
   "Anne"
   "Kaija"
   "A.W."
   "Karl-August"
   "Elisabeth"
   "Amelie"
   )
  )

(defvar elite-last-names
;(setq elite-last-names
  (list 
   "Kekkonen"
   "Koivisto"
   "Ahtisaari"
   "Relander"
   "Svinhufvud"
   "Kallio"
   "Ryti"
   "Stahlberg"
   "Mannerheim"
   "Paasikivi"
   "Sibelius"
   "Nurmi"
   "Linna"
   "Chydenius"
   "Aalto"
   "Lonnrot"
   "von Wright"
   "Westermarck"
   "Kaili"
   "Salomaa"
   "Ahlman"
   "Krohn"
   "Gallen-Kallela"
   "Runeberg"
   "Schjerfbeck"
   "Edelfelt"
   "Thesleff"
   "Enckell"
   "Canth"
   "Cygnaeus"
   "Simberg"
   "Pacius"
   "Leino"
   "Topelius"
   "Kivi"
   "Tshetshulin"
   "Danielson"
   "Snellman"
   "Linkola"
   "Ehnrooth"
   "Halonen"
   "Virolainen"
   "Vennamo"
   "Aho"
   "Talas"
   "Renvall"
   "Louhivuori"
   "Anhava"
   "Bjork"
   "Hanninen"
   "Saariaho"
   "Yrjana"
   "Fagerholm"
   "Blomqvist"
   "Lundahl"
   )
  )

;(message (concat (nth (random (length elite-first-names)) elite-first-names) " " (nth (random (length elite-last-names)) elite-last-names)))

(provide 'elite-people)