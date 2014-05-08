;;; incredibly simple mode for showing data paged by emacs-pager

(defvar emacs-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'server-edit)

    map)
  "Keymap for emacs pager mode.")

;;;###autoload
(define-derived-mode emacs-pager-mode fundamental-mode "Pager"
  "Mode for viewing data paged by emacs-pager"
  (ansi-color-apply-on-region (goto-char (point-min)) (goto-char (point-max)))
  (setq buffer-name "*pager*")
  (read-only-mode))

(provide 'emacs-pager)
