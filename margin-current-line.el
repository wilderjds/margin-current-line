;;; margin-current-line.el --- show current line on the margin.
;;
;; Copyright: (C) 2014 Kouhei Yanagita
;;
;; Author: Kouhei Yanagita <yanagi@shakenbu.org>
;; Author: Jacopo De Simoi <wilderkde@gmail.com>

;; Github: http://github.com/wilderjds/margin-current-line

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;; margin-current-line is a package to indicate current line on the margin.
;; You can use by the following steps.
;;
;; 1. Place this file on your load-path.
;;
;; 2. Add the following code into your init file:
;;
;;   (require 'margin-current-line)
;;
;; 3. Activate the mode.
;;
;; * To enable it only in certain buffer, run `M-x margin-current-line-mode'.
;;   Run again and you can disable it.
;;
;; * To enable it globally, add the following into your init file:
;;
;;     (global-margin-current-line-mode 1)
;;
;;   You can toggle it by running `M-x global-margin-current-line-mode'.
;;
;;; Code:

;;; internal variable
(defvar mcl/margin-overlay nil
  "Hold an overlay for the margin placeholder.")
(make-variable-buffer-local 'mcl/margin-overlay)

(defun mcl/display-margin-marker-at-current-line ()
  (let ((s "x")
        (point (point)))
    (mcl/clear-margin-marker)
    (setq mcl/margin-overlay (make-overlay point (1+ point)))
    (overlay-put mcl/margin-overlay 'priority 137)
    (overlay-put mcl/margin-overlay 'evaporate t)
    (put-text-property 0 1 'display `((margin left-margin)
                                      ,(propertize "▶" 'face 'default)) s)
    (overlay-put mcl/margin-overlay 'before-string s)))


(defun mcl/clear-margin-marker ()
  (when mcl/margin-overlay
    (delete-overlay mcl/margin-overlay)
    (setq mcl/margin-overlay nil)))

(defun margin-current-line-mode-on ()
  (add-hook 'pre-command-hook 'mcl/clear-margin-marker)
  (add-hook 'post-command-hook 'mcl/display-margin-marker-at-current-line nil t)
  )

(defun margin-current-line-mode-off ()
  (mcl/clear-margin-marker)
  (remove-hook 'pre-command-hook 'mcl/clear-margin-marker t)
  (remove-hook 'post-command-hook 'mcl/display-margin-marker-at-current-line t))

(define-minor-mode margin-current-line-mode
  "Indicate current line on the margin."
  :global nil
  (if margin-current-line-mode
      (margin-current-line-mode-on)
    (margin-current-line-mode-off)))

(define-global-minor-mode global-margin-current-line-mode
  margin-current-line-mode
  (lambda ()
    (unless (minibufferp)
      (margin-current-line-mode 1))))

(provide 'margin-current-line)

;;; margin-current-line.el ends here
