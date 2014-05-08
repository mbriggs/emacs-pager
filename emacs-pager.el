;;;; emacs-pager.el --- incredibly simple mode for showing data paged by emacs-pager

;; Copyright (C) 2014 Matt Briggs <http://mattbriggs.net>
;; Author: Matt Briggs
;; URL: http://github.com/mbriggs/emacs-pager
;; Created: 2014
;; Version: 0.1
;; Keywords: pager shell
;; Package-Requires: ()

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
;; See readme for installation / usage

;;; Code:

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
