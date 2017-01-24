;;; mao-window.el --- Defhydra for switching and shrinking window -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: 猫 猫 猫 <mao@mao.mao>
;; Version: 1.0
;; Package-Requires: ((hydra "0.13"))
;; Keywords: tools
;; URL: https://github.com/pvmart/mao-window

(require 'hydra)

(defun mao--window< (wnd1 wnd2)
  "Returns true if WND1 is less than WND2.
This is determined by their respective window coordinates.
Windows are numbered top down, left to right."
  (let ((e1 (window-edges wnd1))
        (e2 (window-edges wnd2)))
    (cond ((< (car e1) (car e2)) t)
          ((> (car e1) (car e2)) nil)
          ((< (cadr e1) (cadr e2)) t))))

(defun mao--window-message (w msg)
  "Shows message `msg' in the center of window `w'. Returns a
list of lists of the window, an overlay hiding text under the
message, an overlay with the message, and a point in the window"
  (with-current-buffer (window-buffer w)
    (let* ((h (window-hscroll w))
           (a (window-width w))
           (b (window-height w))
           (s (window-start w))
           (e (window-end w))
           (l    (- (/ b 2) 3))
           (c (+ (- (/ a 2) 1) h))
           (m (compute-motion s '(0 . 0)
                              e (cons 0 l)
                              a (cons h 0) w))
           (n (car m))
           (r (- (- l (nth 2 m))))
           (k (compute-motion n '(0 . 0)
                              e (cons 0 4)
                              a (cons h 0) w))
           (o (make-overlay (1- n) (1- n)))
           (z (make-overlay n (car k)))
           (p (window-point w))
           (background (if (eq w (selected-window)) "tomato" "skyblue"))
           (style `(:height 2.0
                            :background ,background
                            :foreground "Black"
                            :box (:line-width 2 :color ,background))))
      (set-window-point w n)
      (overlay-put z 'window w)
      (overlay-put o 'window w)
      (overlay-put z 'invisible t)
      (overlay-put o 'after-string
                   (concat (propertize " " 'display `(raise ,r)) "\n\n"
                           (propertize " " 'display `(space :align-to
                                                            (+ left-fringe, c)))
                           (propertize msg 'face style)
                           "\n"))
      (set-window-point w p)
      (list w o z p))))

(defun mao--window-list ()
  "Returns a list of all interesting windows"
  (seq-filter
   (lambda (w)
     (let ((n (buffer-name (window-buffer w))))
       (not (or (string-match "^ " n)
                (string-match "^*helm" n)))))
   (window-list)))

(defun mao--window-enumerate ()
  (let ((i 0))
    (mapcar
     (lambda (w)
       (setq i (1+ i))
       (mao--window-message w (format "%s" i)))
     (sort (mao--window-list) 'mao--window<))))

(defvar mao--window-overlays nil)

(defvar mao--window-overlays-active nil)

(defun mao--window-select ()
  (interactive)
  (let* ((n (if (= last-command-event 48)
                10 (- last-command-event 49)))
         (w (car (nth n mao--window-overlays))))
    (when (window-live-p w) (select-window w))))

(defun mao--window-mark ()
  "Saves what `mao--window-message' returns into `mao--window-overlays'"
  (unless mao--window-overlays
    (setq mao--window-overlays (mao--window-enumerate))))

(defun mao--window-unmark ()
  "Removes overlays and restores points"
  (mapc
   (lambda (woop)
     (delete-overlay (cadr woop))
     (delete-overlay (nth 2 woop))
     (set-window-point (car woop) (nth 3 woop)))
   mao--window-overlays)
  (setq mao--window-overlays nil))



(defhydra mao-window-hydra
  (:color amaranth
          :post (setq mao--window-overlays-active nil)
          :body-pre (progn
                      (setq mao--window-overlays-active t)
                      (mao--window-mark))
          :after-exit (progn
                        (if mao--window-overlays-active
                            (progn
                              (mao--window-unmark)
                              (mao--window-mark))
                          (mao--window-unmark))))
  "
[_0_-_9_]: switch, [_s_/_v_]: split, [←_h_ ↑_j_ ↓_k_ →_l_]: shrink/enlarge, "
  ("1" mao--window-select nil :color blue)
  ("2" mao--window-select nil :color blue)
  ("3" mao--window-select nil :color blue)
  ("4" mao--window-select nil :color blue)
  ("5" mao--window-select nil :color blue)
  ("6" mao--window-select nil :color blue)
  ("7" mao--window-select nil :color blue)
  ("8" mao--window-select nil :color blue)
  ("9" mao--window-select nil :color blue)
  ("0" mao--window-select nil :color blue)
  ("h" shrink-window-horizontally nil)
  ("j" shrink-window nil)
  ("k" enlarge-window nil)
  ("l" enlarge-window-horizontally nil)
  ("s" split-window-below nil :color blue)
  ("v" split-window-right nil :color blue)
  ("q" nil "quit")
  ("C-g" nil))

(provide 'mao-window)

;;; mao-window.el ends here
