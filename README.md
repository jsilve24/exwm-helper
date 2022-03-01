# exwm-helper
Some helper utilities for exwm

At the moment just some functions for moving windows between workspaces. 


# Functions

* `eh-current-window-to-workspace-and-follow-completing-read`  move current window to a new
  frame selected using completing-read (and delete original window)
* `eh-current-window-to-workspace-and-follow-by-index` move current window to a new
  frame selected by passing numerical index argument (and delete original window)


# My configuration

My installation setup:

```
(use-package exwm-helper
  :commands eh-current-window-to-workspace-and-follow-by-index eh-current-window-to-workspace-and-follow-by-index
  :straight (exwm-helper :type git :host github :repo "jsilve24/exwm-helper"))
```

I have the following in my exwm config which uses exwm-helper for i3 like window movements between workspaces. 

```
(setq exwm-input-prefix-keys 
	  ([?\s-!] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 1)))
      ([?\s-@] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 2)))
      ([?\s-#] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 3)))
      ([?\s-$] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 4)))
      ([?\s-%] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 5)))
      ([?\s-^] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 6)))
      ([?\s-&] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 7)))
      ([?\s-*] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 8)))
      ([?\s-\(] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 9)))
       [?\s-\)] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 0)))
```

There is also a completing read interface for this functionality provided by `eh-current-window-to-workspace-and-follow-completing-read`. 


# More of My Opinionated Configuration

## Nicer window splitting behavior than using built-in functions

You will need the following two functions to be loaded. 

```
;;;###autoload
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
	(and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
	(and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
	 (not (window-minibuffer-p window))
	 (let ((split-width-threshold 0))
	   (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

;;;###autoload
(defun jds~new-frame-or-new-window ()
  "New Frame and Focus unless using EXWM then new window."
  (if (frame-parameter (selected-frame) 'exwm-active)
      (progn
	(let ((split-width-threshold 150)
	      (split-height-threshold 20))
	  (if (not (split-window-sensibly-prefer-horizontal))
	      (split-window-right)))
	(other-window 1))
    (select-frame (make-frame))))

```

Then set the following in your exwm-helper configuration:

```
(setq eh-split-window-function 'jds~new-frame-or-new-window)
```

## Integration with KISSES
[KISSES](https://github.com/jsilve24/kisses) is a package I wrote which creates a minimalistic splash screen for emacs. 

Set the following in your exwm-helper config to make the KISSES splash screen act as a fallback buffer when moving the last window off of a workspace. 

```
(setq eh-last-window-function  '(lambda () (progn  (switch-to-buffer "*splash*") (kisses-recenter))))
```


