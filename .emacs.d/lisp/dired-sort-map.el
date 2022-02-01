;;; dired-sort-map.el --- in Dired: press s then s, x, t or n to sort by Size, eXtension, Time or Name

;;; Commentary:
;; The original file is distributed in https://www.emacswiki.org/emacs/dired-sort-map.el

;; Copyright (C) 2002 -> Free Software Foundation, Inc.

;; Inspired by Francis J. Wright's dired-sort-menu.el
;; Authors: Patrick Anderson, Santiago Mejia, Andy Stewart,
;;  Prof.Jayanth R Varma

;; Versions:
;; don't remember
;; 2.2a bundled in NoteMacs
;; 2.2 Add help message suggested by Santiago Mejia
;; 2.3 Precede each switch with " -" as found by Prof.Jayanth R Varma

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Install:
;; Copy this file to a directory in your load path.
;; Execute: M-x eval-buffer :so you don't have to restart.
;; Add the line: (require 'dired-sort-map) : to your .emacs

;;; Todo:
;; (add-hook
;;  'dired-load-hook
;;  '(lambda ()
;;     (progn

;;; Code:
(require 'dired)
(require 'bind-key)

(defvar dired-sort-map (make-sparse-keymap))

(bind-key "s" dired-sort-map dired-mode-map)

(defun dired-sort-map-by-size ()
  "Sort current `dired-mode' buffer by Size."
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -S")))
(bind-key "s" 'dired-sort-map-by-size dired-sort-map)

(defun dired-sort-map-by-extension ()
  "Sort current `dired-mode' buffer by eXtension."
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -X")))
(bind-key "x" 'dired-sort-map-by-extension dired-sort-map)

(defun dired-sort-map-by-time ()
  "Sort current `dired-mode' buffer by Time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches " -t")))
(bind-key "t" 'dired-sort-map-by-time dired-sort-map)

(defun dired-sort-map-by-name ()
  "Sort current `dired-mode' buffer by Name."
  (interactive)
  (dired-sort-other dired-listing-switches))
(bind-key "n" 'dired-sort-map-by-name dired-sort-map)

(defun dired-sort-map-help ()
  "Print help of `dired-sort-map' binding."
  (interactive)
  (message "s Size; x eXtension; t Time; n Name"))
(bind-key "h" 'dired-sort-map-help dired-sort-map)

(provide 'dired-sort-map)
;;; dired-sort-map.el ends here
