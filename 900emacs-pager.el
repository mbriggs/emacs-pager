;;; 900emacs-pager.el --- set up autoloads etc for emacs-pager

(autoload 'emacs-pager-mode "emacs-pager" nil t)

(add-to-list 'auto-mode-alist '("\\.emacs-pager\\'" . emacs-pager-mode))

(eval-after-load "shell"
  `(progn (require 'server)
	  (unless (server-running-p)
	    (server-start) )) )

;;; 900emacs-pager.el ends here
