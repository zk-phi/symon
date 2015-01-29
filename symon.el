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

;; Load this script
;;
;;   (require 'symon)
;;
;; and initialize.
;;
;;   (symon-initialize)
;;
;; then a tiny system monitor is displayed in minibuffer during idle.

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

(defcustom symon-history-size 100
  "number of old values to keep. sparklines grow faster when set
smaller. *set this option BEFORE calling `symon-initialize'.*"
  :group 'symon)

(defcustom symon-refresh-rate 3
  "refresh rate of status values in seconds. *set this option
  BEFORE calling `symon-initialize'.*"
  :group 'symon)

(defcustom symon-delay 2
  "delay in seconds until symon is displayed. *set this option
BEFORE calling `symon-initialize'.*"
  :group 'symon)

(defcustom symon-fetcher
  (cl-case system-type
    ((gnu/linux cygwin) 'symon-default-linux-fetcher)
    ((ms-dos windows-nt) 'symon-default-windows-fetcher))
  "fetcher function to read system statuses. you can use
  preconfigured fetcher `symon-default-linux-fetcher' or
  `symon-default-windows-fetcher', or implement your own. *set
  this option BEFORE calling `symon-initialize'.*"
  :group 'symon)

(defcustom symon-sparkline-size '(80 . 11)
  "(WIDTH . HEIGHT) of sparkline."
  :group 'symon)

;; + symon core

(defvar symon--memory-status nil)
(defvar symon--swap-status nil)
(defvar symon--cpu-status nil)
(defvar symon--battery-status nil)
(defvar symon--active nil)

(defun symon--make-ring (size init)
  "like `make-ring' but INIT can be specified."
  (cons 0 (cons size (make-vector size init))))

(defun symon-initialize ()
  "setup symon system monitor."
  (interactive)
  (unless symon-fetcher
    (error "`symon-fetcher' is not set."))
  (dolist (var '(symon--memory-status symon--swap-status
                                      symon--cpu-status symon--battery-status))
    (set var (symon--make-ring symon-history-size nil)))
  (funcall symon-fetcher)
  (run-with-timer 0 symon-refresh-rate 'symon--redisplay)
  (run-with-idle-timer symon-delay t 'symon-display)
  (add-hook 'pre-command-hook 'symon-display-end))

(defun symon--make-sparkline (ring)
  "make sparkline image from RING."
  (let ((image-data
         (make-bool-vector (* (car symon-sparkline-size) (cdr symon-sparkline-size)) nil))
        (num-samples (ring-size ring))
        (samples (apply 'vector (ring-elements ring)))
        (width (car symon-sparkline-size))
        (height (cdr symon-sparkline-size)))
    (let ((height-per-point (/ height 100.0))
          (width-per-sample (/ width (float num-samples)))
          sample y)
      (dotimes (x width)
        (setq sample (aref samples (floor (/ x width-per-sample))))
        (when (numberp sample)
          (setq y (floor (* (if (= sample 100) 99 sample) height-per-point)))
          (aset image-data (+ (* (- height y 1) width) x) t)))
      `(image :type xbm :data ,image-data :height ,height :width ,width :ascent 100))))

(defun symon-display ()
  "activate symon display."
  (interactive)
  (let ((memory (ring-ref symon--memory-status 0))
        (swap (ring-ref symon--swap-status 0))
        (cpu (ring-ref symon--cpu-status 0))
        (battery (ring-ref symon--battery-status 0))
        (message-log-max nil))
    (message
     (concat "MEM:" (if (not (integerp memory)) "N/A "
                      (concat (number-to-string memory) "%% "
                              (when (and (integerp swap) (> swap 0))
                                (concat "(" (number-to-string swap) "MB Swapped) "))))
             (propertize " " 'display (symon--make-sparkline symon--memory-status)) " "
             "CPU:" (if (not (integerp cpu)) "N/A "
                      (concat (number-to-string cpu) "%% "))
             (propertize " " 'display (symon--make-sparkline symon--cpu-status)) " "
             "BAT:" (if (not (integerp battery)) "N/A "
                      (concat (number-to-string battery) "%% "))
             (propertize " " 'display (symon--make-sparkline symon--battery-status)))))
  (setq symon--active t))

(defun symon--redisplay ()
  "update symon display."
  (when symon--active (symon-display)))

(defun symon-display-end ()
  "deactivate symon display."
  (setq symon--active nil))

;; + default linux fetcher

(defun symon-default-linux-fetcher ()
  "symon fetcher for Linux systems. use `/proc/stat' for cpu,
`free' for memory, and `battery-status-function' for battery
informations."
  (run-with-timer 0 symon-refresh-rate 'symon--default-linux/update-statuses))

(defvar symon--default-linux/last-cpu-ticks '(0 . 0))
(defun symon--default-linux/update-statuses ()
  ;; CPU
  (if (file-exists-p "/proc/stat")
      (let* ((str (shell-command-to-string "cat /proc/stat"))
             (_ (string-match "^cpu\\_>\\(.*\\)$" str))
             (lst (mapcar 'read (split-string (match-string 1 str) nil t)))
             (total (apply '+ lst))
             (idle (nth 3 lst))
             (total-diff (- total (car symon--default-linux/last-cpu-ticks)))
             (idle-diff (- idle (cdr symon--default-linux/last-cpu-ticks))))
        (setq symon--default-linux/last-cpu-ticks (cons total idle))
        (ring-insert symon--cpu-status (/ (* (- total-diff idle-diff) 100) total-diff)))
    (ring-insert symon--cpu-status nil))
  ;; Memory / Swap
  (if (executable-find "free")
      (cl-destructuring-bind (_ mem swap . __)
          (split-string (shell-command-to-string "free -o -m") "\n")
        (setq mem  (cdr (split-string mem))
              swap (cdr (split-string swap)))
        (ring-insert symon--memory-status (/ (* (read (cadr mem)) 100) (read (car mem))))
        (ring-insert symon--swap-status   (read (cadr swap))))
    (ring-insert symon--memory-status nil)
    (ring-insert symon--swap-status nil))
  ;; Battery
  (ring-insert symon--battery-status
               (when battery-status-function
                 (read (cdr (assoc ?p (funcall battery-status-function)))))))

;; + default windows fetcher

(defun symon-default-windows-fetcher ()
  "symon fetcher for Windows systems. use `typeperf' for cpu,
`w32-memory-info' for physical memory, `wmic' for page file,
`w32-battery-status' for battery informations."
  (set-process-query-on-exit-flag
   (start-process-shell-command
    "symon-typeperf" " *symon-typeperf*"
    (format "typeperf -si %d \"\\Processor(_Total)\\%% Processor Time\""
            symon-refresh-rate)) nil)
  (run-with-timer 0 symon-refresh-rate 'symon--default-windows/update-statuses))

(defun symon--default-windows/update-statuses ()
  ;; CPU
  (with-current-buffer " *symon-typeperf*"
    (save-excursion
      (if (search-backward-regexp "\",\"\\(.*\\)\"" nil t)
          (progn
            (ring-insert symon--cpu-status (round (read (match-string 1))))
            (delete-region (point-min) (point)))
        (ring-insert symon--cpu-status nil))))
  ;; Memory
  (let* ((info (cadr (w32-memory-info)))
         (total (cadr info))
         (free (cl-caddr info)))
    (ring-insert symon--memory-status (round (/ (* (- total free) 100) total))))
  ;; Swap (is this correct ?)
  (if (executable-find "wmic")
      (let ((str (shell-command-to-string "wmic path Win32_PageFileUsage get CurrentUsage")))
        (string-match "^[0-9]+\\>" str)
        (ring-insert symon--swap-status (read (match-string 0 str))))
    (ring-insert symon--swap-status nil))
  ;; Battery
  (ring-insert symon--battery-status (read (cdr (assoc ?p (w32-battery-status))))))

;; + provide

(provide 'symon)

;;; symon.el ends here
