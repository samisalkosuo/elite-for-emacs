;;; elite-ships.el -  Elite ship data

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

(defconst SHIP_PIRATE 0)
(defconst SHIP_TRADER 1)
(defconst SHIP_ASTEROID 2)
(defconst SHIP_FEDERAL_MILITARY 3)
(defconst SHIP_IMPERIAL_MILITARY 4)

(defstruct elite-ship
  ;combination from ffe and original elite
  class;name
  hull-mass;from ffe
  energy;from elite,10*hull-mass
  maneuverability;ffe,main thruster accelerator
  missiles;from ffe
)


(defvar elite-ships (vector)
  "Elite ships")

(defvar elite-ship-silhouette nil
  "Ship silhouette")

(defvar elite-ship-profession nil
  "Ship profession."
  ;Profession: pirate,trader,military,etc
)

(defun elite-make-ship (class hull-mass maneuverability missiles profession)
  "Makes elite ship struct"
  (elite-set-ship-profession class profession)
  (elite-set-ship-silhouette class)
  (make-elite-ship
   :class class
   :hull-mass hull-mass
   :energy (* 10 hull-mass)
   :maneuverability maneuverability
   :missiles missiles
   )
)

(defun elite-set-ship-profession (ship-class profession)
  "Sets ships main profession."
  ;(setq elite-ship-profession nil)
  (setq elite-ship-profession (plist-put elite-ship-profession ship-class profession))
)

(defun elite-get-ship-profession (ship-class)
  "Returns ship class (main) profession"
  
  (plist-get elite-ship-profession ship-class)
)

(defun elite-initialize-ships ()
  "initialise ship data"
  (setq elite-ships 
	(vector
	 ;(elite-make-ship "Cobra MK III" 20 201 4 SHIP_PIRATE);COBRA MKIII are used by Elite Federation pilots
	 (elite-make-ship "Falcon" 5 302 2 SHIP_PIRATE)
	 ;(elite-make-ship "Asteroid" 1 0 0 SHIP_ASTEROID)	 
	 (elite-make-ship "Hawk" 11 272 2 SHIP_PIRATE)
	 (elite-make-ship "Kestrel" 11 252 2 SHIP_PIRATE)
	 (elite-make-ship "Eagle MK I" 5 252 2 SHIP_PIRATE)
	 (elite-make-ship "Eagle MK II" 6 282 2 SHIP_PIRATE)
	 (elite-make-ship "Eagle MK III" 8 282 2 SHIP_PIRATE)
	 (elite-make-ship "Saker MK III" 5 211 2 SHIP_PIRATE)
	 (elite-make-ship "Sidewinder" 8 201 4 SHIP_PIRATE)
	 (elite-make-ship "Merlin" 8 222 2 SHIP_PIRATE)
	 (elite-make-ship "Osprey" 6 222 4 SHIP_PIRATE)
	 (elite-make-ship "Gecko" 11 161 1 SHIP_PIRATE)
	 (elite-make-ship "Spar" 12 171 2 SHIP_PIRATE)
	 (elite-make-ship "Adder" 15 181 0 SHIP_PIRATE)
	 (elite-make-ship "Krait" 8 201 4 SHIP_PIRATE)
	 (elite-make-ship "Viper" 15 242 4 SHIP_PIRATE)
	 (elite-make-ship "Cobra MK I" 15 161 2 SHIP_PIRATE)
	 (elite-make-ship "Moray" 17 141 4 SHIP_PIRATE)
	 (elite-make-ship "Harris" 28 211 2 SHIP_PIRATE)
	 (elite-make-ship "Constrictor" 30 222 2 SHIP_PIRATE)
	 (elite-make-ship "Harrier" 25 211 4 SHIP_PIRATE)
	 (elite-make-ship "Asp Explorer" 30 222 1 SHIP_PIRATE)
	 (elite-make-ship "Wyvern" 35 131 2 SHIP_PIRATE)
	 (elite-make-ship "Lanner" 53 80 2 SHIP_TRADER)
	 (elite-make-ship "Lion" 65 50 8 SHIP_TRADER)
	 (elite-make-ship "Anaconda" 175 90 8 SHIP_TRADER)
	 (elite-make-ship "Imperial Courier" 80 121 1 SHIP_PIRATE)
	 (elite-make-ship "Imperial Trader" 100 100 4 SHIP_PIRATE)

	 )
	)
  )

(defun elite-set-ship-silhouette (ship-class)
  "Set ship silhouette"
;   " _________________ " 
;   "|    |\     /|    |" "      ___|___      "
;   "|    | \   / |    |" "     /\     /\     "
;   " \   |  \ /  |   / " "    /| \   / |\    "
;   "  \  |   ^   |  /  " "   / |  \ /  | \   "
;   "   \ |  / \  | /   " "  /  |  /|\  |  \  "
;   "    \| /   \ |/    " " /\  | / | \ |  /\ "
;   "     \/_____\/     " "|  \ |/  |  \| /  |" 
;   "         |         " "|___\|___|___|/___|"
;   (if (string= ship-class "Cobra MK III")
;       (setq elite-ship-silhouette (plist-put elite-ship-silhouette ship-class 
; 					     (list 
; 					      "      ___|___      "
; 					      "     /\\     /\\     "
; 					      "    /| \\   / |\\    "
; 					      "   / |  \\ /  | \\   "
; 					      "  /  |  /|\\  |  \\  "
; 					      " /\\  | / | \\ |  /\\ "
; 					      "|  \\ |/  |  \\| /  |" 
; 					      "|___\\|___|___|/___|"
; 					      )
; 					     )
; 	    )
;  |  _  |   
;  |=(_)=|   
;  |     |   
;            
;   /  _  \  
;  |-=(_)=-| 
;   \     /  
;            
;   \_____/  
; __   |   __
;   \-(_)-/  
;    \   /   
;    /   \   
;            
;  / _  _ \  
; |=(_)(_)=| 
;  \   '  /


; 					    "    warning    "
; 					    "  |    _    |  "
; 					    "  |   / \\   |  "
; 					    "  |--- + ---|  "
; 					    "  |   \\_/   |  "
; 					    "  |         |  "
; 					    "sensor reading "
; 					    "inaccurate     " 


  
  (setq elite-ship-silhouette (plist-put elite-ship-silhouette ship-class 
				(nth (random (length elite-ship-silhouettes)) elite-ship-silhouettes)
				)
    )
  )

(defun elite-get-ship-silhouette (ship-class)
  "Returns ship class (main) profession"
  (plist-get elite-ship-silhouette ship-class)
)


(defvar elite-ship-silhouettes 
;(setq elite-ship-silhouettes
  (list
   (list 
    "         \"..-..pf.                             "
    "        -L   ..#'                              "
    "      .+_L  .\"]#                               "
    "      ,'j' .+.j`                 -'.__..,.,p.  "
    "     _~ #..<..0.                 .J-.``..._f.  "
    "    .7..#_.. _f.                .....-..,`4'   "
    "    ;` ,#j.  T'      ..         ..J....,'.j`   "
    "   .` ..\"^.,-0.,,,,yMMMMM,.    ,-.J...+`.j@    "
    "  .'.`...' .yMMMMM0M@^=`\"\"g.. .'..J..\".'.jH    "
    "  j' .'1`  q'^)@@#\"^\".`\"='BNg_...,]_)'...0-    "
    " .T ...I. j\"    .'..+,_.'3#MMM0MggCBf....F.    "
    " j/.+'.{..+       `^~'-^~~\"\"\"\"'\"\"\"?'\"``'1`     "
    " .... .y.}                  `.._-:`_...jf      "
    " g-.  .Lg'                 ..,..'-....,'.      "
    ".'.   .Y^                  .....',].._f        "
    "......-f.                 .-,,.,.-:--&`        "
    "                          .`...'..`_J`         "
    "                          .~......'#'          "
    "                          '..,,.,_]`           "
    
    )
   (list
"         ,,o                            __.o+.       " 
"       od8^                         oo888888P^b      " 
"     ,\".o'                          `b^'\"\"`b -`b     "
"   ,'.'o'                            t. = -`b -`t.   " 
"  ; d o'          ___          _.--.. 8  -  `b  =`b  " 
" dooo8<       .o:':__;o.     ,;;o88%%8bb - = `b  =`b."
"|^88^88=. .,x88/::/ | \\`;;;;;;d%%%%%88%88888/%x88888"
":-88=88%%L8`%`|::|_>-<_||%;;%;8%%=;:::=%8;;\%%%%\8888"
"|=88 88%%|HHHH|::| >-< |||;%;;8%%=;:::=%8;;;%%%%+|]88"
"| 88-88%%LL.%.%b::Y_|_Y/%|;;;;`%8%%oo88%:o%.;;;;+|]88"
"Yx88o88^^'\"`^^%8boooood..-\\H_Hd%P%%88%P^%%^'\;;;/%%88"
" `\"\\^\\          ~\"\"\"\"\"'      d%P \"\"\"^\" ;   = `+' - P "
"  `.`.b                    :<%%>      :  -   d' - P  "
"    .`.b                   `788      ,'-  = d' =.'   "
"     ``.b.                           :..-  :'  P     "
"      `q.>b                         `^^^:::::,'      "
"        \"\"^^                                         "
    )
   (list
" ::'                                    `:: "
":: :.                                  .: ::"
" `:. `:.             .             .:'  .:' "
"  `::. `::           !           ::' .::'   "
"      `::.`::.    .' ! `.    .::'.::'       "
"        `:.  `::::'':!:``::::'   ::'        "
"        :'*:::.  .:' ! `:.  .:::*`:         "
"       :: HHH::.   ` ! '   .::HHH ::        "
"      ::: `H TH::.  `!'  .::HT H' :::       "
"      ::..  `THHH:`:   :':HHHT'  ..::       "
"      `::      `T: `. .' :T'      ::'       "
"        `:. .   :         :   . .:'         "
"          `::'               `::'           "
"            :'  .`.  .  .'.  `:             "
"            :' ::.       .:: `:             "
"            :' `:::     :::' `:             "
"             `.  ``     ''  .'              "
"              :`...........':               "
"              ` :`.     .': '               "
    )
   (list
"               ...                            "
"             ;::::;                           "
"           ;::::; :;                          "
"         ;:::::'   :;                         "
"        ;:::::;     ;.                        "
"       ,:::::'       ;           OOO\         "
"       ::::::;       ;          OOOOO\        "
"       ;:::::;       ;         OOOOOOOO       "
"      ,;::::::;     ;'         / OOOOOOO      "
"    ;:::::::::`. ,,,;.        /  / DOOOOOO    "
"  .';:::::::::::::::::;,     /  /     DOOOO   "
" ,::::::;::::::;;;;::::;,   /  /        DOOO  "
";`::::::`'::::::;;;::::: ,#/  /          DOOO "
":`:::::::`;::::::;;::: ;::#  /            DOOO"
"::`:::::::`;:::::::: ;::::# /              DOO"
"`:`:::::::`;:::::: ;::::::#/               DOO"
" :::`:::::::`;; ;:::::::::##                OO"
" ::::`:::::::`;::::::::;:::#                OO"
" `:::::`::::::::::::;'`:;::#                O "
    )

   )
  "Ship shilhouettes")

(provide 'elite-ships)

;              ._,.
;            "..-..pf.
;           -L   ..#'
;         .+_L  ."]#
;         ,'j' .+.j`                 -'.__..,.,p.
;        _~ #..<..0.                 .J-.``..._f.
;       .7..#_.. _f.                .....-..,`4'
;       ;` ,#j.  T'      ..         ..J....,'.j`
;      .` .."^.,-0.,,,,yMMMMM,.    ,-.J...+`.j@
;     .'.`...' .yMMMMM0M@^=`""g.. .'..J..".'.jH
;     j' .'1`  q'^)@@#"^".`"='BNg_...,]_)'...0-
;    .T ...I. j"    .'..+,_.'3#MMM0MggCBf....F.
;    j/.+'.{..+       `^~'-^~~""""'"""?'"``'1`
;    .... .y.}                  `.._-:`_...jf
;    g-.  .Lg'                 ..,..'-....,'.
;   .'.   .Y^                  .....',].._f
;   ......-f.                 .-,,.,.-:--&`
;                             .`...'..`_J`
;                             .~......'#'
;                             '..,,.,_]`     
;                             .L..`..``.           

;  .     .            +         .         .                 .  .
;       .                 .                   .               .
;               .    ,,o         .                  __.o+.
;     .            od8^                  .      oo888888P^b           .
;        .       ,".o'      .     .             `b^'""`b -`b   .
;              ,'.'o'             .   .          t. = -`b -`t.    .
;             ; d o' .        ___          _.--.. 8  -  `b  =`b
;         .  dooo8<       .o:':__;o.     ,;;o88%%8bb - = `b  =`b.    .
;     .     |^88^88=. .,x88/::/ | \\`;;;;;;d%%%%%88%88888/%x88888
;           :-88=88%%L8`%`|::|_>-<_||%;;%;8%%=;:::=%8;;\%%%%\8888
;       .   |=88 88%%|HHHH|::| >-< |||;%;;8%%=;:::=%8;;;%%%%+|]88        .
;           | 88-88%%LL.%.%b::Y_|_Y/%|;;;;`%8%%oo88%:o%.;;;;+|]88  .
;           Yx88o88^^'"`^^%8boooood..-\H_Hd%P%%88%P^%%^'\;;;/%%88
;          . `"\^\          ~"""""'      d%P """^" ;   = `+' - P
;    .        `.`.b   .                :<%%>  .   :  -   d' - P      . .
;               .`.b     .        .    `788      ,'-  = d' =.'
;        .       ``.b.                           :..-  :'  P
;             .   `q.>b         .               `^^^:::::,'       .
;                   ""^^               .                     .    

;    (list
; "    ,-```'.        ,-```````>  |\          "
; "   ;       `.      /  ' ,--,\,-\ \         "
; "    .        `.  /`    ,   )/   \ \        "
; "     `.        ' `/  ,'   /|     \ \       " 
; "       `.      _\/_  (  ,- /   ,' \ \      "
; "         >-''``  \ | /  \ |. ,'    \ \     "
; "    ,.-'`      _ | |/`-./\| `.      \ \    "
; "    |   ,.--<``|/  `       \  \      \ \,= "
; "    \  |,.-'``'(           /,  \     .'' '."
; "    \  \       |`-..-'  _,'  \  '----''\.,'"
; "   ,'\  (    ,'|        /:    \____..--`   "
; "  ,  ,>  \,,'  /       / `,                "
; " ,   '\\\\||`'''---..._|_ ;                "
; ":      ```||/'''---...___```'-,            "
; "`.     ,'  /            |`;```             "
; "  `--''    |      ,     | ;                "
; "           |/\  /`\     /`                 "
; "           |  `V  |\,/\/                   "
; "           |      |   /                    "
;     )
;; 
;;   |::::::::'   .':     o`:::::::::::|
;;   |:::::::' oo | |o  o    ::::::::::|
;;   |::::::: 8  .'.'    8 o  :::::::::|
;;   |::::::: 8  | |     8    :::::::::|
;;   |::::::: _._| |_,...8    :::::::::|
;;   |::::::'~--.   .--. `.   `::::::::|
;;   |:::::'     =8     ~  \ o ::::::::|
;;   |::::'       8._ 88.   \ o::::::::|
;;   |:::'   __. ,.ooo~~.    \ o`::::::|
;;   |:::   . -. 88`78o/:     \  `:::::|
;;   |::'     /. o o \ ::      \88`::::|   "He will join us or die."
;;   |:;     o|| 8 8 |d.        `8 `:::|
;;   |:.       - ^ ^ -'           `-`::|
;;   |::.                          .:::|
;;   |:::::.....           ::'     ``::|
;;   |::::::::-'`-        88          `|
;;   |:::::-'.          -       ::     |
;;   |:-~. . .                   :     |
;;   | .. .   ..:   o:8      88o       |
;;   |. .     :::   8:P     d888. . .  |
;;   |.   .   :88   88      888'  . .  |
;;   |   o8  d88P . 88   ' d88P   ..   |
;;   |  88P  888   d8P   ' 888         |
;;   |   8  d88P.'d:8  .- dP~ o8       |   Darth Vader (1)
;;   |      888   888    d~ o888    LS |
;;   |_________________________________|