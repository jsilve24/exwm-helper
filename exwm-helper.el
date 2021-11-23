;;; exwm-helper.el --- utility functions for exwm -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: 2021-11-23
;; Modified: 2021-11-23
;; Version: 0.0.1
;; Keywords: 
;; Homepage: https://github.com/jsilve24/exwm-helper
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Some utility functions for exwm.
;;
;;   + eh-current-window-to-frame :: move current window to a new
;;     frame selected using completing-read
;;
;;; Code:

;; functions stolen from nameframe
(defun eh--get-frame-name (frame)
  "Helper function to extract the name of a FRAME."
  (cdr (assq 'name (frame-parameters frame))))

(defun eh--build-frames-alist-from-frame-list (frame-list)
  "Return an alist of name-frame pairs from a FRAME-LIST (i.e. returned value of the `frame-list' function)."
  (mapcar (lambda (f) `(,(nameframe--get-frame-name f) . ,f)) frame-list))

(defun eh--my-presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))


(defun eh--build-fancy-alist-from-frame-list (frame-list)
  (let ((fl (build-frames-alist-from-frame-list frame-list)))
    (dotimes (i (length fl))
      (let ((elt (nth i fl)))
	(setcar (nth i fl)
		(concat (number-to-string i)
			" : "
			(car (nth i fl))))))
    fl))

(defun eh--select-workspace ()
  "Select an exwm workspace using completing-read."
  (let* ((fl (build-fancy-alist-from-frame-list exwm-workspace--list))
	 (selected (completing-read "foo: " fl)))
    (cdr (assoc selected fl))))

(defun eh-current-window-to-frame ()
  "Delete current window and move it to a selected frame."
  (interactive)
  (let* ((window (selected-window))
	 (buf (window-buffer window))
	 (sel-frame (eh--select-workspace)))
    (select-frame sel-frame)
    (if (string= major-mode "splash-mode")
	nil
      (progn
	(split-window-right)
	(call-interactively #'other-window)))
    (switch-to-buffer buf)
    (delete-window window)))

(provide 'exwm-helper)
