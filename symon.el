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
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:

;; Load this script
;;
;;   (require 'symon)
;;
;; and activate.
;;
;;   (symon-mode)
;;
;; then a tiny system monitor is displayed in minibuffer, during idle.

;;; Change Log:

;;; Code:

(require 'ring)
(require 'cl-lib)
(require 'battery)

(defconst symon-version "1.0.0beta")

(defgroup symon nil
  "tiny graphical system monitor"
  :group 'emacs)

;; + customs

(defcustom symon-history-size 50
  "number of old values to keep. sparklines grow faster when set
smaller. *set this option BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-refresh-rate 3
  "refresh rate of status values in seconds. *set this option
  BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-delay 2
  "delay in seconds until symon is displayed. *set this option
BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-fetcher
  (cl-case system-type
    ((windows-nt) 'symon-default-windows-fetcher)
    ((gnu/linux cygwin) 'symon-default-linux-fetcher))
  "fetcher to read system statuses. you can use preconfigured
  fetcher `symon-default-linux-fetcher' or
  `symon-default-windows-fetcher', or implement your own with
  `define-symon-fetcher'. *set this option BEFORE enabling
  `symon-mode'.*"
  :group 'symon)

(defcustom symon-sparkline-size '(80 . 11)
  "(WIDTH . HEIGHT) of sparkline."
  :group 'symon)

;; + status rings

;; system statuses are stored in `symon--statuses', as a list of
;; rings.

(defvar symon--statuses nil)

(defun symon--make-ring ()
  "like `(make-ring symon-history-size)' but filled with `nil'."
  (cons 0 (cons symon-history-size (make-vector symon-history-size nil))))

(defun symon-commit-status (status value)
  "push a status value VALUE for STATUS."
  (let ((ring (cdr (assq status symon--statuses))))
    (unless ring
      (setq ring (symon--make-ring))
      (push (cons status ring) symon--statuses))
    (ring-insert ring value)))

(defun symon--status-history (status)
  "get a list of recent status values for STATUS."
  (let ((ring (cdr (assq status symon--statuses))))
    (when ring (ring-elements ring))))

;; + symon fetcher

;; a symon fetcher is a vector of [SETUP-FN CLEANUP-FN UPDATE-FN].
;; each FN is a function which takes no arguments. SETUP-FN is called
;; on activation, and CLEANUP-FN is called on deactivation of
;; symon-mode. UPDATE-FN is called every `symon-refresh-rate' seconds
;; and expected to commit status values via `symon-commit-status'.

(defmacro define-symon-fetcher (name &rest plist)
  `(put ',name 'symon-fetcher
        (vector (lambda () ,(plist-get plist :setup))
                (lambda () ,(plist-get plist :cleanup))
                (lambda () ,(plist-get plist :update)))))

;; linux fetcher
(defvar symon-default-linux-fetcher--last-cpu-ticks nil)
(define-symon-fetcher symon-default-linux-fetcher
  :setup (setq symon-default-linux-fetcher--last-cpu-ticks '(0 . 0))
  :update (progn
            ;; CPU
            (if (file-exists-p "/proc/stat")
                (let* ((str (shell-command-to-string "cat /proc/stat"))
                       (_ (string-match "^cpu\\_>\\(.*\\)$" str))
                       (lst (mapcar 'read (split-string (match-string 1 str) nil t)))
                       (total (apply '+ lst))
                       (idle (nth 3 lst))
                       (total-diff (- total (car symon-default-linux-fetcher--last-cpu-ticks)))
                       (idle-diff (- idle (cdr symon-default-linux-fetcher--last-cpu-ticks))))
                  (setq symon-default-linux-fetcher--last-cpu-ticks (cons total idle))
                  (symon-commit-status 'cpu (/ (* (- total-diff idle-diff) 100) total-diff)))
              (symon-commit-status 'cpu nil))
            ;; Memory / Swap
            (if (executable-find "free")
                (let* ((str (shell-command-to-string "free -m")))
                  (string-match "^Mem:[\s\t]*\\([0-9]+\\)[\s\t]*\\([0-9]+\\)\\>" str)
                  (symon-commit-status
                   'memory
                   (/ (* (read (match-string 2 str)) 100) (read (match-string 1 str))))
                  (string-match "^Swap:[\s\t]*[0-9]+[\s\t]*\\([0-9]+\\)\\>" str)
                  (symon-commit-status 'swap (read (match-string 1 str))))
              (symon-commit-status 'memory nil)
              (symon-commit-status 'swap   nil))
            ;; Battery
            (symon-commit-status 'battery
                                 (when battery-status-function
                                   (read (cdr (assoc ?p (funcall battery-status-function))))))))

;; windows fetcher
(define-symon-fetcher symon-default-windows-fetcher
  :setup (set-process-query-on-exit-flag
          (start-process-shell-command
           "symon-typeperf" " *symon-typeperf*"
           (format "typeperf -si %d \"\\Processor(_Total)\\%% Processor Time\""
                   symon-refresh-rate)) nil)
  :cleanup (kill-buffer " *symon-typeperf*")
  :update (progn
            ;; CPU
            (if (buffer-live-p (get-buffer " *symon-typeperf*"))
                (with-current-buffer " *symon-typeperf*"
                  (save-excursion
                    (if (search-backward-regexp "\",\"\\(.*\\)\"" nil t)
                        (progn
                          (symon-commit-status 'cpu (round (read (match-string 1))))
                          (delete-region (point-min) (point)))
                      (symon-commit-status 'cpu nil))))
              (symon-commit-status 'cpu nil))
            ;; Memory
            (cl-destructuring-bind (_ total free . __) (cadr (w32-memory-info))
              (symon-commit-status 'memory (round (/ (* (- total free) 100) total))))
            ;; Swap (is this correct ?)
            (if (executable-find "wmic")
                (let ((str (shell-command-to-string "wmic path Win32_PageFileUsage get CurrentUsage")))
                  (string-match "^[0-9]+\\>" str)
                  (symon-commit-status 'swap (read (match-string 0 str))))
              (symon-commit-status 'swap nil))
            ;; Battery
            (symon-commit-status 'battery (read (cdr (assoc ?p (w32-battery-status)))))))

;; + symon core

(defvar symon--cleanup-function nil)
(defvar symon--status-update-function nil)
(defvar symon--display-active nil)
(defvar symon--timer-objects nil)

;;;###autoload
(define-minor-mode symon-mode
  "tiny graphical system monitor"
  :init-value nil
  :global t
  (cond (symon-mode
         (setq symon--statuses nil)
         (if (null symon-fetcher)
             (error "`symon-fetcher' is not set.")
           (let ((vec (get symon-fetcher 'symon-fetcher)))
             (funcall (aref vec 0))
             (setq symon--cleanup-function       (aref vec 1)
                   symon--status-update-function (aref vec 2))))
         (setq symon--timer-objects
               (list (run-with-timer 0 symon-refresh-rate 'symon--update)
                     (run-with-idle-timer symon-delay t 'symon--display)))
         (add-hook 'pre-command-hook 'symon-display-end))
        (t
         (remove-hook 'pre-command-hook 'symon-display-end)
         (mapc 'cancel-timer symon--timer-objects)
         (funcall symon--cleanup-function))))

(defun symon--make-sparkline (list &optional upper-bound)
  "make sparkline image from LIST."
  (let ((image-data
         (make-bool-vector (* (car symon-sparkline-size) (cdr symon-sparkline-size)) nil))
        (num-samples (length list))
        (samples (apply 'vector list))
        (width (car symon-sparkline-size))
        (height (cdr symon-sparkline-size)))
    (unless (zerop num-samples)
      (let ((height-per-point (/ height (if upper-bound (float upper-bound) 100.0)))
            (width-per-sample (/ width (float num-samples)))
            sample y)
        (dotimes (x width)
          (setq sample (aref samples (floor (/ x width-per-sample))))
          (when (numberp sample)
            (setq y (floor (* (if (= sample 100) 99 sample) height-per-point)))
            (when (< y height)
              (aset image-data (+ (* (- height y 1) width) x) t))))))
    `(image :type xbm :data ,image-data :height ,height :width ,width :ascent 100)))

(defun symon--display ()
  "activate symon display."
  (interactive)
  (when (not (active-minibuffer-window))
    (let* ((memory-lst (symon--status-history 'memory))
           (memory (car memory-lst))
           (swap-lst (symon--status-history 'swap))
           (swap (car swap-lst))
           (cpu-lst (symon--status-history 'cpu))
           (cpu (car cpu-lst))
           (battery-lst (symon--status-history 'battery))
           (battery (car battery-lst))
           (message-log-max nil))   ; do not insert to *Messages* buffer
      (message
       (concat
        "MEM:" (if (not (integerp memory)) "N/A " (format "%2d%%%% " memory))
        (when (and (integerp swap) (> swap 0)) (format "(%dMB Swapped) " swap))
        (propertize " " 'display (symon--make-sparkline memory-lst)) " "
        "CPU:" (if (not (integerp cpu)) "N/A " (format "%2d%%%% " cpu))
        (propertize " " 'display (symon--make-sparkline cpu-lst)) " "
        "BAT:" (if (not (integerp battery)) "N/A " (format "%d%%%% " battery))
        (propertize " " 'display (symon--make-sparkline battery-lst)))))
    (setq symon--display-active t)))

(defun symon--update ()
  "update symon display."
  (funcall symon--status-update-function)
  (when symon--display-active (symon--display)))

(defun symon-display-end ()
  "deactivate symon display."
  (setq symon--display-active nil))

(define-obsolete-function-alias 'symon-initialize 'symon-mode "1/30/2015")

;; + provide

(provide 'symon)

;;; symon.el ends here
