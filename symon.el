;;; symon.el --- tiny graphical system monitor

;; Copyright (C) 2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0beta

;;; Commentary:

;; Load either `symon-linux' or `symon-windows'
;;
;;   (require 'symon-linux)
;;
;; and turn on `symon-mode'.
;;
;;   (symon-mode)
;;
;; then a tiny system monitor is displayed in minibuffer, during idle.

;;; Change Log:

;;; Code:

(require 'ring)

(defconst symon-version "1.0.0beta")

(defgroup symon nil
  "tiny graphical system monitor"
  :group 'emacs)

;; + customs

(defcustom symon-refresh-rate 3
  "refresh rate of symon display. *set this option BEFORE
  enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-delay 2
  "delay in seconds until symon is displayed. *set this option
BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-monitors nil
  "list of monitors used to read system statuses. *set this
  option BEFORE enabling `symon-mode'.*")

(defcustom symon-sparkline-size '(80 . 11)
  "(WIDTH . HEIGHT) of sparkline."
  :group 'symon)

;; + utilities

(defun symon--make-sparkline (list &optional minimum maximum)
  "make sparkline image from LIST."
  (let ((image-data
         (make-bool-vector
          (* (car symon-sparkline-size) (cdr symon-sparkline-size)) nil))
        (maximum (if maximum (float maximum) 100.0))
        (minimum (if minimum (float minimum) 0.0))
        (num-samples (length list))
        (samples (apply 'vector list))
        (width (car symon-sparkline-size))
        (height (cdr symon-sparkline-size)))
    (unless (zerop num-samples)
      (let ((height-per-point (/ height (1+ (- maximum minimum))))
            (width-per-sample (/ width (float num-samples)))
            sample y)
        (dotimes (x width)
          (setq sample (aref samples (floor (/ x width-per-sample))))
          (when (numberp sample)
            (setq y (floor (* (- sample minimum) height-per-point)))
            (when (and (<= 0 y) (< y height))
              (aset image-data (+ (* (- height y 1) width) x) t))))))
    `(image :type xbm :data ,image-data :height ,height :width ,width :ascent 100)))

(defun symon--make-ring (size)
  "like `(make-ring symon-history-size)' but filled with `nil'."
  (cons 0 (cons size (make-vector size nil))))

;; + symon fetcher

(defconst symon--default-update-interval 3)

;; a symon monitor is a vector of 3 functions: [SETUP-FN CLEANUP-FN
;; DISPLAY-FN]. SETUP-FN is called on activation of `symon-mode', and
;; expected to setup Emacs to fetch status values in a specific
;; interval. CLEANUP-FN is called on deactivation and expected to tell
;; Emacs to stop fetching. DISPLAY-FN is called just before displaying
;; monitor, and must return display string for the monitor.

(defmacro define-symon-monitor (name &rest plist)
  (let* ((cell (make-vector 2 nil))
         (history (plist-get plist :history))
         (interval (or (plist-get plist :interval) symon--default-update-interval))
         (display (plist-get plist :display))
         (update-fn
          `(lambda ()
             (ring-insert (aref ,cell 0) ,(plist-get plist :fetch))))
         (setup-fn
          `(lambda ()
             (aset ,cell 0 (symon--make-ring (1+ (or ,history 0))))
             (aset ,cell 1 (run-with-timer 0 ,interval ,update-fn))
             ,(plist-get plist :setup)
             (funcall ,update-fn)))
         (cleanup-fn
          `(lambda ()
             (cancel-timer (aref ,cell 1))
             ,(plist-get plist :cleanup)))
         (display-fn
          (if display `(lambda () (concat ,display " "))
            `(lambda ()
               (or ,(plist-get plist :display)
                   (let* ((lst (ring-elements (aref ,cell 0)))
                          (val (car lst)))
                     (concat ,(plist-get plist :index)
                             (if (not (numberp val)) "N/A "
                               (concat (number-to-string val)
                                       ,(plist-get plist :unit) " "
                                       (let ((annot ,(plist-get plist :annotation)))
                                         (when annot (concat "(" annot ") ")))))
                             ,(when history
                                `(when (window-system)
                                   (concat (propertize " " 'display
                                                       (symon--make-sparkline
                                                        lst
                                                        ,(plist-get plist :lower-bound)
                                                        ,(plist-get plist :upper-bound)))
                                           " "))))))))))
    `(put ',name 'symon-monitor (vector ,setup-fn ,cleanup-fn ,display-fn))))

;; + symon core

(defvar symon--cleanup-fns    nil)
(defvar symon--display-fns    nil)
(defvar symon--display-active nil)
(defvar symon--timer-objects  nil)

;;;###autoload
(define-minor-mode symon-mode
  "tiny graphical system monitor"
  :init-value nil
  :global t
  (cond (symon-mode
         (unless symon-monitors
           (message "Warning: `symon-monitors' is empty."))
         (let ((monitors (mapcar (lambda (s) (get s 'symon-monitor)) symon-monitors)))
           (mapc (lambda (m) (funcall (aref m 0))) monitors)
           (setq symon--cleanup-fns    (mapcar (lambda (m) (aref m 1)) monitors)
                 symon--display-fns    (mapcar (lambda (m) (aref m 2)) monitors)
                 symon--display-active nil
                 symon--timer-objects
                 (list (run-with-timer 0 symon-refresh-rate 'symon--redisplay)
                       (run-with-idle-timer symon-delay t 'symon-display)))
           (add-hook 'pre-command-hook 'symon--display-end)))
        (t
         (remove-hook 'pre-command-hook 'symon--display-end)
         (mapc 'cancel-timer symon--timer-objects)
         (mapc 'funcall symon--cleanup-fns))))

(defun symon-display ()
  "activate symon display."
  (interactive)
  (unless (active-minibuffer-window)
    (let* ((message-log-max nil))   ; do not insert to *Messages* buffer
      (message "%s" (apply 'concat (mapcar 'funcall symon--display-fns))))
    (setq symon--display-active t)))

(defun symon--redisplay ()
  "update symon display."
  (when symon--display-active (symon-display)))

(defun symon--display-end ()
  "deactivate symon display."
  (setq symon--display-active nil))

;; + backward compatibility

(defvar symon-fetcher)
(make-obsolete-variable 'symon-fetcher 'symon-monitors "2015/02/05")

;; + provide

(provide 'symon)

;;; symon.el ends here
