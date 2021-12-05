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
;;   + eh-current-window-to-workspace-and-follow-completing-read :: move current window to a new
;;     frame selected using completing-read (and delete original window)
;;
;;   + eh-current-window-to-workspace-and-follow-by-index :: move current window to a new
;;     frame selected by passing numerical index argument (and delete original window)
;;
;;; Code:

;; functions stolen from nameframe
(defun eh--get-frame-name (frame)
  "Helper function to extract the name of a FRAME."
  (cdr (assq 'name (frame-parameters frame))))

(defun eh--build-frames-alist-from-frame-list (frame-list)
  "Return an alist of name-frame pairs from a FRAME-LIST (i.e. returned value of the `frame-list' function)."
  (mapcar (lambda (f) `(,(eh--get-frame-name f) . ,f)) frame-list))

(defun eh--my-presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))


(defun eh--build-fancy-alist-from-frame-list (frame-list)
  (let ((fl (eh--build-frames-alist-from-frame-list frame-list)))
    (dotimes (i (length fl))
      (let ((elt (nth i fl)))
	(setcar (nth i fl)
		(concat (number-to-string i)
			" : "
			(car (nth i fl))))))
    fl))

(defun eh--select-workspace ()
  "Select an exwm workspace using completing-read."
  (let* ((fl (eh--build-fancy-alist-from-frame-list exwm-workspace--list))
	 (selected (completing-read "Select Workspace: " fl)))
    (cdr (assoc selected fl))))

(defvar eh-split-window-function 'split-window-right
  "Function to be called when moving window onto new frame.")

(defun eh--switch-to-other-buffer ()
  (switch-to-buffer (other-buffer)))

(defvar eh-last-window-function 'eh--switch-to-other-buffer
  "Function to be called if removing last window from frame.")

(defvar eh-fallback-buffer "*splash*"
  "Look for this buffer and if present try to place things there.")

(defun eh--eligible-fallback-buffer-in-frame-p ()
  "Is there a visible fallback buffer in frame"
  (if 
      (-filter (lambda (x) (string= eh-fallback-buffer x))
	       (-map 'buffer-name (-map 'window-buffer (window-list))))
      (get-buffer-window eh-fallback-buffer)
    nil))

(defun eh--current-window-to-workspace-by-frame-and-follow (frm)
  "Delete current window and move it to the frame `FRM'."
  (let* ((cur-frame (selected-frame))
	 (window (selected-window))
	 (buf (window-buffer window)))
    ;; if (not (eq cur-frame frm))
    (if (one-window-p t)
	(funcall eh-last-window-function)
      (delete-window window))
    (select-frame frm)
    (let ((fallback (eh--eligible-fallback-buffer-in-frame-p)))
      (if fallback
	  (select-window fallback)
	(progn  (funcall eh-split-window-function)
		(other-window 1))))
    (switch-to-buffer buf)))

(defun eh-current-window-to-workspace-and-follow-completing-read ()
  "Delete current window and move it to a selected workspace.
Select workspace by completing-read."
  (interactive)
  (let ((sel-frame (eh--select-workspace)))
    (if (not  (eq (selected-frame) sel-frame))
     (eh--current-window-to-workspace-by-frame-and-follow sel-frame))))


(defun eh-current-window-to-workspace-and-follow-by-index (idx)
  "Delete current window and move it to workspace `IDX' (e.g.,
numerical index)."
  (interactive)
  (let ((sel-frame (exwm-workspace--workspace-from-frame-or-index idx)))
    (if (not  (eq (selected-frame) sel-frame))
	(eh--current-window-to-workspace-by-frame-and-follow sel-frame))))

(provide 'exwm-helper)
