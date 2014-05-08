;;; emacs-pager.el --- incredibly simple mode for showing data paged by emacs-pager

;; Copyright (C) 2014 Matt Briggs <http://mattbriggs.net>

;; Author: Matt Briggs
;; URL: http://github.com/mbriggs/emacs-pager
;; Version: 0.0.1
;; Keywords: pager shell

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;; See readme (http://mattbriggs.net/emacs-pager/) for installation / usage

;;; Code:

(defvar emacs-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'emacs-pager-kill-pager)

    map)
  "Keymap for emacs pager mode.")

(defcustom emacs-pager-max-line-coloring 500
  "Maximum number of lines to ansi-color. If performance is bad when
   loading data, reduce this number"
  :group 'emacs-pager)

(defun emacs-pager-kill-pager ()
  "Kill pager buffer immediately"
  (interactive)
  (set-buffer-modified-p nil)
  (server-edit))

;;;###autoload
(define-derived-mode emacs-pager-mode fundamental-mode "Pager"
  "Mode for viewing data paged by emacs-pager"
  (setq-local make-backup-files nil)
  (ansi-color-apply-on-region (goto-char (point-min))
                              (save-excursion
                                (forward-line emacs-pager-max-line-coloring)
                                (point)))

  (setq buffer-name "*pager*")
  (read-only-mode))

(provide 'emacs-pager)

;;; emacs-pager.el ends here
