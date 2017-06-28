(defun full-screen ()
      (interactive)
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(add-to-list 'load-path (expand-file-name "/headless/elite-for-emacs-master/elite-for-emacs-V1"))
(require 'elite)

(full-screen)
