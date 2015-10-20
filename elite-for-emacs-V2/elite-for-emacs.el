;;; elite-for-emacs.el - Elite for EMACS

(defconst elite-for-emacs-version "0.1"
  "Version number of Elite for EMACS")

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
;; Uses shell-like user interface. elite.el is obsolete and no further
;; development is planned. Future releases will include elite.el but new
;; development is done for elite-for-emacs.el. Some code from old elite.el is
;; used in the new version.
;;
;;
;; TAB is used for command completion. Shift-right arrow browses all commands.
;; Shift-up arrow browses command history.
;;
;; Installation:
;;
;; Add elite-for-emacs.el to your load path and add
;; (require 'elite-for-emacs)
;; to .emacs
;;
;; Start Elite for EMACS M-x elite-for-emacs
;;

(require 'cl)

;;Load Elite for EMACS functions
(load "elite-for-emacs-commands")
(load "elite-for-emacs-functions")
(load "elite-for-emacs-commander")
(load "elite-for-emacs-engine")
(load "elite-for-emacs-online-functions")
(load "elite-for-emacs-bazaar")

(defvar elite-for-emacs-current-command nil)

(defvar elite-for-emacs-original-frame-title-format nil)

(defvar elite-for-emacs-command-history nil)

(defvar elite-for-emacs-command "")

(defvar elite-for-emacs-buffer-name "*Elite for EMACS*")
(defvar elite-for-emacs-buffer-name-offline "*Elite for EMACS*")
(defvar elite-for-emacs-buffer-name-online "*Elite for EMACS (online)*")
;;Functions for prompt, modeline title.
(defvar elite-for-emacs-prompt-function 'elite-for-emacs-prompt)
(defvar elite-for-emacs-mode-line-function 'elite-for-emacs-mode-line)
(defvar elite-for-emacs-frame-title-function 'ignore);elite-for-emacs-frame-title)
(defvar elite-for-emacs-custom-post-command-function 'elite-for-emacs-post-command)
(defvar elite-for-emacs-custom-pre-command-function 'elite-for-emacs-post-command)
(defvar elite-for-emacs-kill-buffer-function 'elite-for-emacs-kill-buffer)
(defvar elite-for-emacs-command-list nil)
(defvar elite-for-emacs-suppress-message t)
(defvar elite-for-emacs-suppress-default-newline-command nil)
(defvar elite-for-emacs-online nil)


(defvar elite-for-emacs-base-command-list
  ;;(setq elite-for-emacs-base-command-list
  (list
   (list "version" 'elite-for-emacs-version-info)
   (list "display-logo" 'elite-for-emacs-logo)
   (list "display-logo-no-text" 'elite-for-emacs-logo-no-text)
   ;;(list "changes" 'elite-for-emacs-changes)
   (list "cls" 'elite-for-emacs-clear)
   (list "help" 'elite-for-emacs-help)
   (list "exit" 'elite-for-emacs-exit)
   (list "quit" 'elite-for-emacs-exit)
   (list "script" 'elite-for-emacs-script-execute)
   )
  )


;;todo: save history variable

(add-hook 'kill-buffer-hook 'elite-for-emacs-kill-buffer-hook)


(defun elite-for-emacs ()
  "Elite for EMACS."
  (interactive)
  (let (
	(buffer)
	)
    (if (get-buffer elite-for-emacs-buffer-name)
	(progn
	  (switch-to-buffer elite-for-emacs-buffer-name)
	  )
      (progn
	(random t)
	(setq elite-for-emacs-command "")
	(setq elite-for-emacs-tab-command nil)
	(setq elite-for-emacs-tab-index 0)

	;;buffer local variable, shell prompt
	;;enter: executes command, adds new line, inserts prompt
	(setq buffer (get-buffer-create elite-for-emacs-buffer-name))
	(switch-to-buffer buffer)

	(setq elite-for-emacs-command-loop-index 0)
	(setq elite-for-emacs-history-index 0)

	(insert (elite-for-emacs-set-prompt))
	(local-set-key [tab] 'elite-for-emacs-tab)

	(setq elite-for-emacs-original-frame-title-format frame-title-format)
	(elite-for-emacs-set-mode-line)
	(elite-for-emacs-frame-title)

	;;Set commands before game
	(setq elite-for-emacs-command-list
	       (append 
		       (list
			(list "new" 'elite-for-emacs-new-commander)
			(list "load" 'elite-for-emacs-load-commander)
			;;(list "start-script" 'elite-for-emacs-start-script)

			)
		       elite-for-emacs-base-command-list
		       )
	       )

	;;todo: save history in kill hook, load here
	(setq elite-for-emacs-command-history (list))
	;;add handler to self-insert-command, post-command hook
	(add-hook 'pre-command-hook 'elite-for-emacs-pre-command-hook nil t)
	(add-hook 'post-command-hook 'elite-for-emacs-post-command-hook nil t)


	)
      )

    )
  )

(defun elite-for-emacs-online ()
  "Elite for EMACS online."
  (interactive)
  (setq elite-for-emacs-online t)
  (setq elite-for-emacs-buffer-name elite-for-emacs-buffer-name-online)
  (elite-for-emacs)
  )


(defvar elite-for-emacs-command-loop-index 0)
(defvar elite-for-emacs-history-index 0)


(defun elite-for-emacs-post-command-hook ()
  "Added to post-command-hook"
  (let (
	(cmd)
	(completion)
	(event)
	(modifiers)
	(command-list)
	(temp)
	)
    ;;special functions: tab, enter, up, down, left, right
    (condition-case error
	(progn


	  ;;(message elite-for-emacs-command-output-string)
	  (setq event last-command-event)


    (if (eq this-command 'self-insert-command)
	(progn
	  ;;
	  (setq elite-for-emacs-command (concat elite-for-emacs-command (list event)))

	  )
	)

    (if (eq this-command 'delete-backward-char)
	(progn
	  (if (> (length elite-for-emacs-command) 0)
	      (setq elite-for-emacs-command (substring elite-for-emacs-command 0 (- (length elite-for-emacs-command) 1)))
	    )
	  )
      )

    (if (eq this-command 'forward-char)
	(progn
	  (setq modifiers (event-modifiers last-input-event))
	  (if (eq (car modifiers) 'shift)
	      (progn
		(goto-char (point-max))
		(beginning-of-line)
		(kill-line)
		(insert (concat (elite-for-emacs-set-prompt)))
		(setq cmd (car (nth elite-for-emacs-command-loop-index elite-for-emacs-command-list)))
		(setq elite-for-emacs-command cmd)
		(insert cmd)
		(setq elite-for-emacs-command-loop-index (1+ elite-for-emacs-command-loop-index))
		(if (= elite-for-emacs-command-loop-index (length elite-for-emacs-command-list))
		    (setq elite-for-emacs-command-loop-index 0)
		  )
		)
	    )
	  )
      )

    (if (eq this-command 'backward-char)
	(progn
	  (setq modifiers (event-modifiers last-input-event))
	  (if (eq (car modifiers) 'shift)
	      (progn
		(goto-char (point-max))
		(beginning-of-line)
		(kill-line)
		(insert (concat (elite-for-emacs-set-prompt)))
		(setq cmd (car (nth elite-for-emacs-command-loop-index elite-for-emacs-command-list)))
		(setq elite-for-emacs-command cmd)
		(insert cmd)
		(setq elite-for-emacs-command-loop-index (1- elite-for-emacs-command-loop-index))
		(if (< elite-for-emacs-command-loop-index 0)
		    (setq elite-for-emacs-command-loop-index (1- (length elite-for-emacs-command-list)))
		  )
		)
	    )
	  )
      )

    (if (eq this-command 'previous-line)
	(progn
	  (setq modifiers (event-modifiers last-input-event))
	  (if (eq (car modifiers) 'shift)
	      (progn
		(if (> (length elite-for-emacs-command-history) 0)
		    (progn
		      (goto-char (point-max))
		      (beginning-of-line)
		      (kill-line)
		      (insert (concat (elite-for-emacs-set-prompt)))
		      (setq cmd (nth elite-for-emacs-history-index (reverse elite-for-emacs-command-history)))
		      (setq elite-for-emacs-command cmd)
		      (insert cmd)
		      (setq elite-for-emacs-history-index (1+ elite-for-emacs-history-index))
		      (if (= elite-for-emacs-history-index (length elite-for-emacs-command-history))
			  (setq elite-for-emacs-history-index 0)
			)
		      )
		  )
		)
	    )
	  )
      (progn
	;;set history to 0 if not previous line command
	(setq elite-for-emacs-history-index 0)

	)
      )


    (if (eq this-command 'newline)
	(progn
	  (if (< (point) (point-max))
	      (progn
		;;for some reason pre-command-hook newline (goto-char (point-max))
		;;does not work if we are not at the end of buffer
		(delete-backward-char 1)
		(goto-char (point-max))
		(insert "\n")
		)
	    )
	  (if (not elite-for-emacs-suppress-default-newline-command)
	      (progn
		;;todo (documentation 'forward-char)

		    ;;do command
		    ;;(setq cmd (assoc elite-for-emacs-current-command elite-for-emacs-command-list))
		    ;;(setq cmd (cadr (assoc (car (split-string elite-for-emacs-command)) elite-for-emacs-command-list)))
		    ;;(setq cmd (cadr (assoc  elite-for-emacs-command-list)))
;; 		    (setq temp (car (split-string elite-for-emacs-command)))
;; 		    ;;(setq temp "versi")
;; 		    (if temp
;; 			(progn
;; 			  (setq cmd (cadr (assoc (try-completion temp elite-for-emacs-command-list) elite-for-emacs-command-list)))
;; 			  (if (not cmd)
;; 			      (setq cmd (cadr (assoc (car (split-string elite-for-emacs-command)) elite-for-emacs-command-list)))
;; 			    )
;; 		    )
;; 		      (progn
;; 			(setq cmd "")
;; 			)
;; 		      )

		    ;;get first match
		    (setq cmd (elite-for-emacs-get-first-command-match (car (split-string elite-for-emacs-command))))
		    (setq cmd (cadr (assoc cmd elite-for-emacs-command-list)))

		    ;;act on command
		    (if  (or (commandp cmd) (functionp cmd))
			(progn

			  (if (commandp cmd)
			      (command-execute cmd)
			    (funcall cmd)
			    )

			  ;;(insert (concat elite-for-emacs-current-command " executed."))
			  (if (current-message)
			      (progn
				(insert (current-message))
				(if elite-for-emacs-suppress-message
				    (message nil)
				  )
				)
			    )
			  ;;(setq elite-for-emacs-command-history (remove elite-for-emacs-command elite-for-emacs-command-history))
			  ;;(setq elite-for-emacs-command-history (append elite-for-emacs-command-history (list elite-for-emacs-command)))

			  ;;(setq elite-for-emacs-current-command nil)
			  (if (not (eq cmd 'elite-for-emacs-clear))
			      (insert "\n")
			    )
			  )
		      (progn
			;;(funcall cmd)
			(if (not (string= elite-for-emacs-command ""))
			    (insert "Bad command (" elite-for-emacs-command ").\n")
			)
		      )
		      )
		    
		    (if (and elite-for-emacs-command (not (string= elite-for-emacs-command "")))
			(setq elite-for-emacs-command-history (append elite-for-emacs-command-history (list elite-for-emacs-command)))
		      )
		    (setq elite-for-emacs-command "")
		    (insert (concat (elite-for-emacs-set-prompt)))
		    
		    (setq elite-for-emacs-tab-index 0)
		    (setq elite-for-emacs-tab-ring nil)
		    (setq elite-for-emacs-tab-command nil)
		    
		    (elite-for-emacs-set-mode-line)
		    (elite-for-emacs-frame-title)
		    )
	    )
	  )
      )
    
    (if (and elite-for-emacs-suppress-message (current-message))
	(progn
	  (message nil)
	  )
      )
    
    (if (functionp elite-for-emacs-custom-post-command-function)
	(funcall elite-for-emacs-custom-post-command-function)
      )


    )
      (error
       (insert (error-message-string error) "\n")
       (setq elite-for-emacs-command "")
       (insert (concat (elite-for-emacs-set-prompt)))
       
       (setq elite-for-emacs-tab-index 0)
       (setq elite-for-emacs-tab-ring nil)
       (setq elite-for-emacs-tab-command nil)

       (elite-for-emacs-set-mode-line)
       (elite-for-emacs-frame-title)

       )
      )
    )
)

(defun elite-for-emacs-get-first-command-match (cmd)
  ""
  (let (
	(command-list)
	(temp)
	(index)
	(command)
	)
    (setq command "")
    ;;(setq cmd "")
    (if cmd
	(progn
	  (setq command-list elite-for-emacs-command-list)
	  (while command-list
	    (setq temp (caar command-list))
	    (setq index (string-match cmd temp))
	    (if (and index (= index 0))
		(progn
		  (setq command-list nil)
		  (setq command temp)
		  )
	      )
	    (setq command-list (cdr command-list))
	    )
	  )
      )
    command
    )
  )

(defvar elite-for-emacs-tab-index 0)
(defvar elite-for-emacs-tab-command nil)
(defvar elite-for-emacs-tab-ring nil)

(defun elite-for-emacs-tab ()
  (interactive)
  (let (
	(completion)
	(cmd)
	)
    ;;(setq completion nil)
    ;;(setq elite-for-emacs-command "3")
    (if elite-for-emacs-tab-command
	(setq completion (all-completions elite-for-emacs-tab-command elite-for-emacs-command-list))
      (progn
	(if (string= elite-for-emacs-command "")
	    (setq elite-for-emacs-tab-ring t)
	  )
	;;(all-completions "r" '(("list" 'l) ("re" 'l) ("reeee" 'l)))
	(if elite-for-emacs-tab-ring
	    (setq completion (all-completions "" elite-for-emacs-command-list))
	  (setq completion (all-completions elite-for-emacs-command elite-for-emacs-command-list))
	  )
	)
      )

    (if completion
	(progn
	  (if (and (not elite-for-emacs-tab-ring) (> (length completion) 1) (not elite-for-emacs-tab-command))
	      (progn
		(setq elite-for-emacs-tab-command elite-for-emacs-command)
		)
	    )

	  (if (>= elite-for-emacs-tab-index (length completion))
	      (setq elite-for-emacs-tab-index 0)
	    )

	  (goto-char (point-max))
	  (beginning-of-line)
	  (kill-line)
	  (insert (elite-for-emacs-set-prompt))
	  (setq cmd (nth elite-for-emacs-tab-index completion))
	  (setq elite-for-emacs-command cmd)
	  (insert cmd)
	  (setq elite-for-emacs-tab-index (1+ elite-for-emacs-tab-index))
	  )
      )

    )
  )


(defun elite-for-emacs-pre-command-hook ()
  "Added to pre-command-hook"
  (let (
	(cmd)
	(completion)
	)    

    (setq event last-command-event)
    ;;(message (number-to-string event))
    (if (/= event 9);;TAB
  	(progn
	  (setq elite-for-emacs-tab-index 0)
	  (setq elite-for-emacs-tab-ring nil)
	  (setq elite-for-emacs-tab-command nil)
  	  )
      )

    (if (eq this-command 'self-insert-command)
	(progn

	  )
	)

    (if (eq this-command 'newline)
	(progn
	  ;;(message "pre command hook newline")
	  (goto-char (point-max))
	  )
	)

    (if (functionp elite-for-emacs-custom-pre-command-function)
	(funcall elite-for-emacs-custom-pre-command-function)
      )

    )
)

(defun elite-for-emacs-kill-buffer-hook ()
  (let (
	)
    (if (string= (buffer-name) elite-for-emacs-buffer-name)
	(progn
	  ;;clean up
	  (remove-hook 'post-command-hook 'elite-for-emacs-post-command-hook t)
	  (remove-hook 'pre-command-hook 'elite-for-emacs-pre-command-hook t)
	  (setq frame-title-format elite-for-emacs-original-frame-title-format)
	  (if (functionp elite-for-emacs-kill-buffer-function)
	      (funcall elite-for-emacs-kill-buffer-function)
	    )

	  )
	)
    )
  )

(defun elite-for-emacs-set-prompt ()
  "Function returns prompt"
  (let (
	)
    (if (and (functionp elite-for-emacs-prompt-function) (not (string= (symbol-name elite-for-emacs-prompt-function) "ignore")))
	;;(if (functionp elite-for-emacs-prompt-function)
	(funcall elite-for-emacs-prompt-function)
      ""
      )
    )
  )

(defun elite-for-emacs-set-mode-line ()
  "Sets mode line for Simple Shell buffer"
  (let (

	)
    (if (and (functionp elite-for-emacs-mode-line-function) (not (string= (symbol-name elite-for-emacs-mode-line-function) "ignore")))
	;;(if (functionp elite-for-emacs-mode-line-function)
	(progn
	  (setq mode-line-format
		(funcall elite-for-emacs-mode-line-function)
		)

	  ;;remember to use force at the end of mode-line function
	  (force-mode-line-update)
	  )
      )
    )
  )

(defun elite-for-emacs-frame-title ()
  "Sets frame title for Simple Shell buffer"
  (let (

	)
    (if (and (functionp elite-for-emacs-frame-title-function) (not (string= (symbol-name elite-for-emacs-frame-title-function) "ignore")))
	(setq frame-title-format (funcall elite-for-emacs-frame-title-function))
      )
    )
  )


(defun elite-for-emacs-default-frame-title ()
  "Returns default frame title"
  (let (

	)
    "Simple Shell Title"
    )
  )

(defun elite-for-emacs-default-mode-line ()
  "Sets  mode line"
  (let (
	)
    ;;custome modeline format
    (list "---"
	  'elite-for-emacs-command
	  "-%-"
	  )
    )
  )

(defun elite-for-emacs-default-prompt ()
  "Function returns prompt"
  (let (
	)
    "SimpleShell>"
    )
  )


(defun elite-for-emacs-test-function ()
  (let (
	)
    (if (eq this-command 'newline)
	(insert "hello world")
      )
    )
  )

(defun elite-for-emacs-clear ()
  "Clear screen."
  (let (

	)
    (erase-buffer)
    )
  )

(defun elite-for-emacs-exit ()
  "Exit Elite for EMACS."
  (let (

	)
    (insert "Use kill-buffer (\\C-x k) to exit.")
    ;;(call-interactively 'kill-buffer)
    ;;(call-interactively 'emacs-version)
    )
  )

(defun elite-for-emacs-help ()
  "Help."
  (let (
	(temp)
	)
    (setq temp (split-string elite-for-emacs-command))
    ;;(insert  "Commands:\nhelp This help\nhelp <command> Command help\n" )
    (insert  "Commands:\n" )
    (setq command-list elite-for-emacs-command-list)
    (while command-list
      (setq cmd (car command-list))
      (insert (car cmd))
      (if (>= (length (car cmd)) 16)
	  (insert  "\t")
	(progn
	  (if (>= (length (car cmd)) 8)
	      (insert  "\t\t")
	    (progn
	      (insert  "\t\t\t")
	      )
	    )
	  )
	)	
      (setq temp (documentation (cadr cmd)))
      (if temp
	  (insert (documentation (cadr cmd)) "\n")
	(insert "\n")	
	)
      (setq command-list (cdr command-list))
      )
    ;;(setq elite-for-emacs-command-history (remove elite-for-emacs-command elite-for-emacs-command-history))
    ;;(setq elite-for-emacs-command-history (append elite-for-emacs-command-history (list elite-for-emacs-command)))

    )
  )


 (defgroup elite-for-emacs nil
   "Extensible simple shell."
  :tag "Elite for EMACS"
  :prefix "elite-for-emacs-"
  :version "21.2.1"
  :group 'games)

;;custom variables: colors (online/offline)?


 (defcustom elite-for-emacs-save-confirmation-when-exit t
   "*If non-nil user is asked to save commander when killing Elite for EMACS buffer."
   :type 'boolean
   :group 'elite-for-emacs)


;;Online customization
(defgroup elite-for-emacs-online nil
  "Elite for EMACS."
  :tag "Elite for EMACS Online"
  :prefix "elite-for-emacs-online"
  :version "21.2.1"
  :group 'elite-for-emacs)


 (defcustom elite-for-emacs-online-autosave-dock nil
   "*Non-nil saves commander when docking station."
   :type 'boolean
   :group 'elite-for-emacs-online)

 (defcustom elite-for-emacs-online-autosave-undock nil
   "*Non-nil saves commander when undocking station."
   :type 'boolean
   :group 'elite-for-emacs-online)

 (defcustom elite-for-emacs-online-proxy-host nil
   "*HTTP proxy host."
   :type 'string
   :group 'elite-for-emacs-online)

 (defcustom elite-for-emacs-online-proxy-port nil
   "*HTTP proxy port."
   :type 'string
   :group 'elite-for-emacs-online)

;; (defcustom elite-for-emacs-buffer-name "*Elite for EMACS*"
;;   "*Simple Shell buffer name."
;;   :type 'string
;;   :group 'elite-for-emacs)
;;
;;
;; (defcustom elite-for-emacs-prompt-function 'elite-for-emacs-default-prompt
;;   "*Simple Shell prompt function name. Function must return a string that will be used
;; as shell prompt. Prompt function is called after calling function 'newline (normally after pressing
;; ENTER). Use 'ignore'-function to disable feature."
;;   :type 'function
;;   :group 'elite-for-emacs)
;;
;; (defcustom elite-for-emacs-mode-line-function 'elite-for-emacs-default-mode-line
;;   "*Simple Shell mode line function name. Function must return a list of mode line options, see
;; GNU Emacs (21.2) Lisp Reference Manual section 23.3 Mode Line Format.
;; Mode line function is called after calling function 'newline (normally after pressing
;; ENTER). Use 'ignore'-function to disable feature."
;;   :type 'function
;;   :group 'elite-for-emacs)
;;
;; (defcustom elite-for-emacs-frame-title-function 'elite-for-emacs-default-frame-title
;;   "*Simple Shell frame title function name. Function must return a string that will be used as
;; a new frame title.
;; Frame title function is called after calling function 'newline (normally after pressing
;; ENTER). Use 'ignore'-function to disable feature."
;;   :type 'function
;;   :group 'elite-for-emacs)
;;
;; (defcustom elite-for-emacs-custom-post-command-function 'ignore
;;   "*Function is added to post-command-hook and is executed after executing simple shell command. Use 'ignore'-function to disable feature."
;;   :type 'function
;;   :group 'elite-for-emacs)
;;
;; (defcustom elite-for-emacs-custom-pre-command-function 'ignore
;;   "*Function is added to pre-command-hook and is executed before executing simple shell command. Use 'ignore'-function to disable feature."
;;   :type 'function
;;   :group 'elite-for-emacs)
;;
;;
;; (defcustom elite-for-emacs-command-list '(("version" emacs-version))
;;   "*Simple Shell command list. Key is shell command and value is function. If function displays something
;; in the mini buffer it is inserted in the mini buffer and message is not show in the mini buffer if
;; elite-for-emacs-suppress-message is t. Functions should use insert-function to insert messages in the shell
;; buffer. "
;;
;;   :type 'alist;(alist :key-type string :value-type symbol)
;;   :group 'elite-for-emacs)
;;
;; (defcustom elite-for-emacs-suppress-message t
;;   "*Suppress message in mini buffer."
;;   :type 'boolean
;;   :group 'elite-for-emacs)

(provide 'elite-for-emacs)
