;;; elite-for-emacs-commander.el - Elite for EMACS commander functions

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

;; Player and AI commander functions. Idea is that player and AI commander have
;; exactly the same attributes
;; All game variables should be in commander struct


;;state constants are internal indicators of commander state
;;different situations may have different state, for example
;;commander can enter casino while docked and then state would be in casino etc
(defconst STATE_DOCKED 0)
(defconst STATE_IN_SPACE 1)
(defconst STATE_WITCH_SPACE 2)
(defconst STATE_COMBAT 3)
(defconst STATE_BAZAAR 4)

(defstruct elite-for-emacs-commander
  ;;additional fields MUST be included at the end of struct
  ;;when deployed: do NOT make new fields so that they have to be available from
  ;;the start of game
  id
  name
  gender
  elite-score;one kill, one point
  legal-status
  reputation;reputation, more the better
  credits
  fuel
  missiles
  current-planet
  hyperspace-system
  condition;0=docked,1=green,2=yellow,3=red
  current-state;docked,in space, witchspace, combat etc
  current-galaxy  
  cargo-capacity
  max-cargo-capacity
  current-cargo
  local-market
  lastrand
  fluct
  current-day ;day in elite unverse
  auto-refuel
  equipment-list
  script-list;script list, for good trade routes etc
  temp
  home-system
  home-galaxy
  ;;last message sent is for displaying last known system of commander
  ;;messages are public so we know it...
  last-message-sent-galaxy;;for online mode, galaxy index where user sent message
  last-message-sent-planet;;for online mode, planet index where user sent message
  distance-to-planet;;when in space, distance to planet, location 0 is space station and commander can dock,
  character-dd;;dungeons and dragons character description
  trade-history-current;;current trade items in cargo hold, update when buying
  trade-history;;overall cargo history, update when selling
  ;;todo mates;list of mates
)

(defvar elite-for-emacs-commander-list nil
  "Elite for EMACS commanders. First in list is player")

(defvar elite-for-emacs-saved-commander-list nil
  "Saved commanders");;used to easily upgrade commander struct

(defun elite-for-emacs-generate-commander (name id gender current-planet current-galaxy day)

  (let (

	)

    (setq elite-for-emacs-commander-list 
	  (append 
	   elite-for-emacs-commander-list 
	   (list (make-elite-for-emacs-commander
	    :id id
	    :name name
	    :gender gender
	    :elite-score 0;one kill, one point
	    :legal-status 0
	    :condition CONDITION_DOCKED
	    :current-state STATE_DOCKED
	    :reputation 0;reputation, more the better
	    :credits 1000
	    ;;:credits 100000
	    :current-planet current-planet
	    :current-galaxy current-galaxy
	    :hyperspace-system current-planet
;;(aref (elite-for-emacs-get-galaxy 0) 7)
	    :fuel 70
	    :cargo-capacity 20
	    :max-cargo-capacity 20
	    :current-cargo (make-vector (+ lasttrade 1) 0)
	    :lastrand (mysrand 12345)
	    :current-day day
	    :auto-refuel t;;set nil to switch autorefuel off
	    :home-system current-planet
	    :home-galaxy current-galaxy
	    )
		 )
	   )
	  )
    )
  )

(defun elite-for-emacs-get-commander (index)
  (nth index elite-for-emacs-commander-list)
)