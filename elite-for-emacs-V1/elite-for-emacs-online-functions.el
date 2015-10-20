;;; elite-for-emacs-online-functions.el - Elite for EMACS online functions

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
;;
;; Functions in this package communicate with server

;;(defconst  
(setq elite-for-emacs-server-url "www.elite-for-emacs.org/servlet/elite-for-emacs?command=")

;(defconst
;;  (setq elite-for-emacs-server-url "127.0.0.1:8080/elite-for-emacs/server?command=")

(defun elite-for-emacs-online-check-name (commander)
  ""
  (let (
	)
    (elite-for-emacs-online-get-content
     (elite-for-emacs-online-generate-command "new" commander))
	  

    )
  )

(defun elite-for-emacs-online-save-commander (commander)
  ""
  (let (

	)
    (elite-for-emacs-online-get-content
     (elite-for-emacs-online-generate-command "save" commander))

    
    )
)

(defun elite-for-emacs-online-load-commander (commander-name)
  ""
  (let (
	(tmp)
	)
    (setq tmp (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "load" nil  commander-name)))
    (if (string= tmp "NOT_OK")
	nil
      (read-from-string tmp)
      )
    )
)

(defun elite-for-emacs-online-local-market (current-galaxy current-planet)
  ""
  (let (
	(localmarket)
	(fluct)
	)
    (setq localmarket (elite-for-emacs-online-get-content
		       (elite-for-emacs-online-generate-command "localmarket" nil nil current-galaxy current-planet)))
    ;;(setq localmarket (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "localmarket" nil nil 0 7)))

    (if (string-match "RANDBYTE" localmarket)
	(progn
	  ;;generate market
	   (elite-for-emacs-online-genmarket localmarket current-galaxy current-planet)
	  )
      (car (read-from-string localmarket))
      )
    ;;(insert localmarket "\n")

    ;;
    )
)

(defun elite-for-emacs-online-update-local-market-buy (current-galaxy current-planet item-index amount)
  (let( (tmp)
    )
    (setq tmp (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "updatelocalmarket" nil nil current-galaxy current-planet nil "buy" item-index amount)))
    (if (string-match "RANDBYTE" tmp)
	(progn
	  (elite-for-emacs-online-genmarket tmp current-galaxy current-planet)
	  (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "updatelocalmarket" nil nil current-galaxy current-planet nil "buy" item-index amount))
	  )
      tmp
      )
	
)
)

(defun elite-for-emacs-online-update-local-market-sell (current-galaxy current-planet item-index amount)
  (let( (tmp)
    )
    (setq tmp (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "updatelocalmarket" nil nil current-galaxy current-planet nil "sell" item-index amount)))
    (if (string-match "RANDBYTE" tmp)
	(progn
	  (elite-for-emacs-online-genmarket tmp current-galaxy current-planet)
	  (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "updatelocalmarket" nil nil current-galaxy current-planet nil "sell" item-index amount))
	  )
      tmp
      )
	
)
)

(defun elite-for-emacs-online-genmarket (randbyte current-galaxy current-planet)
  (let (
	(localmarket)
	(fluct)
	)
    (setq fluct (string-to-number (nth 1 (split-string randbyte " "))))
    (setq localmarket (genmarket fluct (aref (aref elite-for-emacs-galaxies-in-universe current-galaxy) current-planet )))
	  ;;(genmarket 0 (aref (aref elite-for-emacs-galaxies-in-universe 0) 7))
    (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "savelocalmarket" nil nil current-galaxy current-planet localmarket))
    localmarket
    
    )  
  )

(defun elite-for-emacs-online-fluct ()
  ""
  (let (

	)
    (string-to-number (elite-for-emacs-online-get-content
		       (elite-for-emacs-online-generate-command "fluct")))
    
    )
)

(defun elite-for-emacs-online-message-board (galaxy system)
  ""
  (let (
	(messages)
	)
    ;;todo galaxy/universum messages
    ;;(setq messages (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "messageboard" nil nil 0 7)))
    (setq messages (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "messageboard" nil nil galaxy system)))
    (if (string= messages "NO_MESSAGES")
	nil
      (progn
	(car (read-from-string messages))

	)
      )

    )
  )

(defun elite-for-emacs-online-send-message (galaxy system commander-name message)
  ""
  (let (
	(res)
	)
    (setq res (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "sendmessage" nil commander-name galaxy system nil nil nil nil message)))
    
    )
  )

(defun elite-for-emacs-online-commanders ()
  ""
  (let (
	(res)
	)
    (setq res (elite-for-emacs-online-get-content (elite-for-emacs-online-generate-command "commanders")))
    (car (read-from-string res))
    
    )
  )


(defun elite-for-emacs-online-get-content (command-xml)
  ""
  (let (
	(content)
	(tmp)
	(tmp2)
	)

    (if (and elite-for-emacs-online-proxy-host elite-for-emacs-online-proxy-port)
	(setq content (elite-for-emacs-online-get-url-content (concat 
					elite-for-emacs-server-url
					command-xml
					)
				       elite-for-emacs-online-proxy-host
				       (string-to-number elite-for-emacs-online-proxy-port)
				       )
	      )      
      (setq content (elite-for-emacs-online-get-url-content (concat 
				      elite-for-emacs-server-url
				      command-xml
				      )
				     )
	  )
      )
    (switch-to-buffer elite-for-emacs-buffer-name)
    (condition-case error
	(progn
	  (setq tmp (split-string content))
	  (setq tmp2 (nth 1 tmp))	  
	  (if (string= tmp2 "200")
	      (car (split-string (substring content (+ 2 (string-match "\n\n" content))) "\n"))
	    (progn
	      (message (concat  (nth 0 tmp) " " (nth 1 tmp) " " (nth 2 tmp)))
	      "NOT_OK"
	      )
	    )	    
	  )
      (error
       "NOT_OK"       
       )
      )
    )
  )


(defun elite-for-emacs-online-generate-command (command 
						&optional commander 
						commander-name 
						current-galaxy current-planet
						localmarket
						transaction-type;;'buy' or 'sell'
						item-index;;item index that was bought/sold
						amount;;amount of buy/sell
						message;;message to message board
						)
  (let (
	(tmp)
	)

  ;;(setq commander (car elite-for-emacs-commander-list))
;;    (base64-encode-string   
    (elite-for-emacs-online-url-encode
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<elite-for-emacs>"
      "<command>"
      command
      "</command>"

      ;;(length (prin1-to-string (genmarket 0 (aref (aref elite-for-emacs-galaxies-in-universe 0) 7))))
      (if localmarket
	  (progn
	    (concat
	     "<localmarket>"
	     (prin1-to-string localmarket)
	     "</localmarket>"
	     )
	    )
	  )
      (if message
	  (progn
	    (concat
	     "<message>"
	     message
	     "</message>"
	     )
	    )
	)

      (if (and item-index amount transaction-type)
	  (progn
	    (concat
	     "<item>"
	     (number-to-string item-index)
	     "</item>"
	     "<amount>"
	     (number-to-string amount)
	     "</amount>"
	     "<type>"
	     transaction-type
	     "</type>"
	    )
	  )
	)

      (if (and current-galaxy current-planet)
	  (progn
	    (concat
	     "<galaxy>"
	     (number-to-string current-galaxy)
	     "</galaxy>"
	     "<planet>"
	     (number-to-string current-planet)
	     "</planet>"
	    )
	  )
	)
      (if commander-name
	  (progn
	    (concat 
	     "<commander-name>"
	     commander-name
	     "</commander-name>"	    
	     )
	    )
	)
      (if commander
	  (progn
	    (concat
	     "<commander>"
	     (prin1-to-string commander)
	     "</commander>"
	     )
	    )
	)
      "</elite-for-emacs>"    
      )
     ;;t
     )
    )
  )

(defun elite-for-emacs-online-url-encode (str &optional coding-system)
  ;;code taken from somewhere... thanks to whoever you are
  (let (
	(urlencode-default-coding-system 'iso-8859-1)
	(urlencode-exceptional-chars "[a-zA-Z0-9]")
	)
  (mapconcat
   (lambda (c)
     (format (if (string-match urlencode-exceptional-chars (char-to-string c))
		 "%c" "%%%02X") c))
   (encode-coding-string str
			 (or coding-system urlencode-default-coding-system))
   "")
  )
)


(defun elite-for-emacs-online-get-url-content (url &optional proxy-host proxy-port)
  "Functions that gets content from specified URL. Returns content of
the URL
as a string. Supports only HTTP protocol.
Arguments:
url: get content from this URL. MUST be in format
'some.host[:port]/somefile?some=parameter'
proxy-host and proxy-port: HTTP proxy, host as string and port as integer"
  (let(
       (host)
       (port 80)
       (file)
       (buffer)
       (tcp-connection)
       (request)
       (tmp)
       (tmp2)
       (content)
       )
    (if proxy-host
	(progn
	  (setq host proxy-host)
	  (setq port proxy-port)
	  (setq file (concat "http://" url))
	  )
      (progn
	(setq tmp (string-match ":" url))
	(if (not tmp)
	    (setq tmp2 (string-match "/" url))
	  (setq tmp2 (string-match "/" url tmp))
	  )

	(if (not tmp)
	    (progn
	      (setq port 80)
	      (if (not tmp2)
		  (progn
		    (setq host url)
		    (setq file "/")
		    )
		(progn
		  (setq host (substring url 0 tmp2))
		  (setq file (substring url tmp2))
		  )
		)
	      )
	  (progn
	    (setq host (substring url 0 tmp))
	    (if (not tmp2)
		(setq port (string-to-number (substring url (1+ tmp))))
	      (setq port (string-to-number (substring url (1+ tmp) tmp2)))
	      )
	    (setq file (substring url tmp2))

	    )
	  )
	)
      )

    (setq buffer (get-buffer-create "*Elite for EMACS server response*"))
    (set-buffer buffer)
    (erase-buffer)
    (goto-char 0)

    (setq tcp-connection
	  (open-network-stream
	   "Elite for EMACS server connection"
	   buffer
	   host
	   port
	   ))

    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-sentinel tcp-connection 'elite-for-emacs-online-get-url-content-sentinel)

    (setq request (concat "GET " file " HTTP/1.1\n"
			  "Host: " host "\n"
			  "Accept: */*\n"
			  (concat "User-Agent: elite-for-emacs/" elite-for-emacs-version  "\n")
			  "Connection: close\n"
			  "\n"
			  ))
    (process-send-string tcp-connection request)
    (elite-for-emacs-online-get-url-content-parse tcp-connection)
    (delete-process tcp-connection)

    (setq content (buffer-string))
    (kill-buffer buffer)
    content
    )
  )

(defun elite-for-emacs-online-get-url-content-sentinel (process string)
  "Process the results from the efine network connection.
process - The process object that is being notified.
string - The string that describes the notification."
  )

(defun elite-for-emacs-online-get-url-content-parse (process)
  ""
  (let (
	(buffer)
	(header-end)
	(msg)
	(i)
	)
    (setq i 0)
    (while (eq (process-status process) 'open)
      (sit-for 0 200)
      )
    )
  )
