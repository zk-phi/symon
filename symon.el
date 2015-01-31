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

;; + utilities

(defun symon--file-contents (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun symon--make-sparkline (list &optional lim)
  "make sparkline image from LIST."
  (let ((image-data
         (make-bool-vector
          (* (car symon-sparkline-size) (cdr symon-sparkline-size)) nil))
        (lim (if lim (float lim) 100.0))
        (num-samples (length list))
        (samples (apply 'vector list))
        (width (car symon-sparkline-size))
        (height (cdr symon-sparkline-size)))
    (unless (zerop num-samples)
      (let ((height-per-point (/ height (1+ lim)))
            (width-per-sample (/ width (float num-samples)))
            sample y)
        (dotimes (x width)
          (setq sample (aref samples (floor (/ x width-per-sample))))
          (when (numberp sample)
            (setq y (floor (* sample height-per-point)))
            (when (and (<= 0 y) (< y height))
              (aset image-data (+ (* (- height y 1) width) x) t))))))
    `(image :type xbm :data ,image-data :height ,height :width ,width :ascent 100)))

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

;; a symon fetcher is a vector of [SETUP-FN CLEANUP-FN]. each FN is a
;; function which takes no arguments. SETUP-FN is called on activation
;; of `symon-mode', and should set up emacs properly to commit status
;; values via `symon-commit-status' every `symon-refresh-rate'
;; seconds. CLEANUP-FN is called on deactivation of `symon-mode' and
;; expected to do some clean-ups to stop reporting status values.

(defmacro define-symon-fetcher (name &rest plist)
  `(put ',name 'symon-fetcher
        (vector (lambda () ,(plist-get plist :setup))
                (lambda () ,(plist-get plist :cleanup)))))

;; linux fetcher

(defvar symon--default-linux-last-cpu-ticks nil)
(defvar symon--default-linux-timer-object nil)

(define-symon-fetcher symon-default-linux-fetcher
  :setup (setq symon--default-linux-last-cpu-ticks nil
               symon--default-linux-timer-object
               (run-with-timer 0 symon-refresh-rate 'symon--default-linux-update-function))
  :cleanup (cancel-timer symon--default-linux-timer-object))

(defun symon--default-linux-update-function ()
  ;; CPU
  (if (file-exists-p "/proc/stat")
      (let* ((str (symon--file-contents "/proc/stat"))
             (_ (string-match "^cpu\\_>\\(.*\\)$" str))
             (lst (mapcar 'read (split-string (match-string 1 str) nil t)))
             (total (apply '+ lst))
             (idle (nth 3 lst)))
        (if symon--default-linux-last-cpu-ticks
            (let ((total-diff (- total (car symon--default-linux-last-cpu-ticks)))
                  (idle-diff (- idle (cdr symon--default-linux-last-cpu-ticks))))
              (symon-commit-status 'cpu (/ (* (- total-diff idle-diff) 100) total-diff)))
          (symon-commit-status 'cpu nil))
        (setq symon--default-linux-last-cpu-ticks (cons total idle)))
    (symon-commit-status 'cpu nil))
  ;; Memory / Swap
  (if (file-exists-p "/proc/meminfo")
      (let* ((str (symon--file-contents "/proc/meminfo"))
             (_ (string-match "^MemTotal:[\s\t]*\\([0-9]+\\)\\>" str))
             (memtotal (read (match-string 1 str)))
             (_ (string-match "^MemFree:[\s\t]*\\([0-9]+\\)\\>" str))
             (memfree (read (match-string 1 str)))
             (_ (string-match "^SwapTotal:[\s\t]*\\([0-9]+\\)\\>" str))
             (swaptotal (read (match-string 1 str)))
             (_ (string-match "^SwapFree:[\s\t]*\\([0-9]+\\)\\>" str))
             (swapfree (read (match-string 1 str))))
        (symon-commit-status 'memory (/ (* (- memtotal memfree) 100) memtotal))
        (symon-commit-status 'swap (/ (- swaptotal swapfree) 1000)))
    (symon-commit-status 'memory nil)
    (symon-commit-status 'swap nil))
  ;; Battery
  (symon-commit-status 'battery
                       (when battery-status-function
                         (read (cdr (assoc ?p (funcall battery-status-function)))))))

;; windows fetcher

(defvar symon--default-windows-timer-object nil)

(define-symon-fetcher symon-default-windows-fetcher
  :setup (progn
           (set-process-query-on-exit-flag
            (start-process-shell-command
             "symon-typeperf" " *symon-typeperf*"
             (format "typeperf -si %d \"\\Processor(_Total)\\%% Processor Time\""
                     symon-refresh-rate)) nil)
           (setq symon--default-windows-timer-object
                 (run-with-timer 0 symon-refresh-rate 'symon--default-windows-update-function)))
  :cleanup (progn
             (cancel-timer symon--default-windows-timer-object)
             (kill-buffer " *symon-typeperf*")))

(defun symon--default-windows-update-function ()
  ;; typeperf (CPU)
  (if (buffer-live-p (get-buffer " *symon-typeperf*"))
      (with-current-buffer " *symon-typeperf*"
        (save-excursion
          (if (search-backward-regexp "\",\"\\(.*\\)\"" nil t)
              (progn
                (symon-commit-status 'cpu (round (read (match-string 1))))
                (delete-region (point-min) (point)))
            (symon-commit-status 'cpu nil))))
    (symon-commit-status 'cpu nil))
  ;; wmic (Memory / Swap, Battery)
  (let* ((str (shell-command-to-string
               (eval-when-compile
                 (concat "(echo path Win32_ComputerSystem get TotalPhysicalMemory"
                         " && echo path Win32_OperatingSystem get FreePhysicalMemory"
                         " && echo path Win32_Battery get EstimatedChargeRemaining"
                         " && echo path Win32_PageFileUsage get CurrentUsage"
                         " && echo exit) | wmic"))))
         (_ (string-match "^TotalPhysicalMemory.*\n\\(.*\\)$" str))
         (memtotal (floor (/ (read (match-string 1 str)) 1000)))
         (_ (string-match "^FreePhysicalMemory.*\n\\(.*\\)$" str))
         (memfree (read (match-string 1 str)))
         (_ (string-match "^EstimatedChargeRemaining.*\n\\(.*\\)$" str))
         (battery (read (match-string 1 str)))
         (_ (string-match "^CurrentUsage.*\n\\(.*\\)$" str))
         (swap (read (match-string 1 str))))
    (when (file-exists-p "TempWmicBatchFile.bat")
      (delete-file "TempWmicBatchFile.bat"))
    (symon-commit-status 'memory  (/ (* (- memtotal memfree) 100) memtotal))
    (symon-commit-status 'swap    swap)
    (symon-commit-status 'battery battery)))

;; + symon core

(defvar symon--cleanup-function nil)
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
             (setq symon--cleanup-function (aref vec 1))))
         (setq symon--timer-objects
               (list (run-with-timer 0 symon-refresh-rate 'symon--redisplay)
                     (run-with-idle-timer symon-delay t 'symon--display)))
         (add-hook 'pre-command-hook 'symon-display-end))
        (t
         (remove-hook 'pre-command-hook 'symon-display-end)
         (mapc 'cancel-timer symon--timer-objects)
         (funcall symon--cleanup-function))))

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
       (concat "MEM:" (if (not (integerp memory)) "N/A " (format "%2d%%%% " memory))
               (when (and (integerp swap) (> swap 0)) (format "(%dMB Swapped) " swap))
               (propertize " " 'display (symon--make-sparkline memory-lst)) " "
               "CPU:" (if (not (integerp cpu)) "N/A " (format "%2d%%%% " cpu))
               (propertize " " 'display (symon--make-sparkline cpu-lst)) " "
               "BAT:" (if (not (integerp battery)) "N/A " (format "%d%%%% " battery))
               (propertize " " 'display (symon--make-sparkline battery-lst)))))
    (setq symon--display-active t)))

(defun symon--redisplay ()
  "update symon display."
  (when symon--display-active (symon--display)))

(defun symon-display-end ()
  "deactivate symon display."
  (setq symon--display-active nil))

(define-obsolete-function-alias 'symon-initialize 'symon-mode "1/30/2015")

;; + provide

(provide 'symon)

;;; symon.el ends here
