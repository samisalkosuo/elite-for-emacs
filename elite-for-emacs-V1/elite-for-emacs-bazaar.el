;;; elite-for-emacs-bazaar.el - Elite for EMACS bazaar

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

;; Bazaar is place in Elite for EMACS stations where can be and 
;; happen anything ..

(defun elite-for-emacs-bazaar ()
  "Station bazaar."
  (let (

	)
    (insert "Entering bazaar.")
    (setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_BAZAAR)
    

    )
  
  )

(defun elite-for-emacs-leave-bazaar ()
  "Leave bazaar."
  (let (

	)
    (insert "Leaving bazaar.")
    (setf (elite-for-emacs-commander-current-state (car elite-for-emacs-commander-list)) STATE_DOCKED)
    

    )
  
  )

(defun elite-for-emacs-bazaar-prompt ()
  ""
  (let (
	(cmdr)
	)
    (setq cmdr (car elite-for-emacs-commander-list))
    (concat
     (elite-for-emacs-get-system-name (elite-for-emacs-commander-current-galaxy cmdr) (elite-for-emacs-commander-current-planet cmdr))
     " Bazaar "
     (format "%.1f" (/ (elite-for-emacs-commander-credits cmdr) 10.0))
     ">"
     )
    )
  )
  
