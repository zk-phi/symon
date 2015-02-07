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
;; and turn on `symon-mode'.
;;
;;   (symon-mode)
;;
;; then a tiny system monitor is displayed in minibuffer, during idle.

;;; Change Log:

;;; Code:

(require 'battery)
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

(defcustom symon-history-size 50
  "number of old values to keep. sparklines grow faster when set
smaller. *set this option BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-monitors
  (cond ((memq system-type '(gnu/linux cygwin))
         '(symon-linux-memory-monitor
           symon-linux-cpu-monitor
           symon-linux-battery-monitor))
        ((memq system-type '(windows-nt))
         '(symon-windows-memory-monitor
           symon-windows-cpu-monitor
           symon-windows-battery-monitor)))
  "list of monitors used to read system statuses. *set this
  option BEFORE enabling `symon-mode'.*")

(defcustom symon-sparkline-size '(80 . 11)
  "(WIDTH . HEIGHT) of sparkline."
  :group 'symon)

(defcustom symon-network-rx-upper-bound 300
  "upper-bound of sparkline for network RX status."
  :group 'symon)

(defcustom symon-network-tx-upper-bound 100
  "upper-bound of sparkline for network TX status."
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

(defun symon--make-history-ring ()
  "like `(make-ring symon-history-size)' but filled with `nil'."
  (cons 0 (cons symon-history-size (make-vector symon-history-size nil))))

;; + symon monitor

;; a symon monitor is a vector of 3 functions: [SETUP-FN CLEANUP-FN
;; DISPLAY-FN]. SETUP-FN is called on activation of `symon-mode', and
;; expected to setup Emacs to fetch status values in a specific
;; interval. CLEANUP-FN is called on deactivation and expected to tell
;; Emacs to stop fetching. DISPLAY-FN is called just before displaying
;; monitor, and must return display string for the monitor.

(defmacro define-symon-monitor (name &rest plist)
  (let* ((cell (make-vector 2 nil))
         (sparkline (plist-get plist :sparkline))
         (interval (or (plist-get plist :interval) 'symon-refresh-rate))
         (display (plist-get plist :display))
         (update-fn
          `(lambda ()
             (ring-insert (aref ,cell 0) ,(plist-get plist :fetch))))
         (setup-fn
          `(lambda ()
             (aset ,cell 0 (symon--make-history-ring))
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
               (let* ((lst (ring-elements (aref ,cell 0)))
                      (val (car lst)))
                 (concat ,(plist-get plist :index)
                         (if (not (numberp val)) "N/A "
                           (concat (format "%d%s " val ,(plist-get plist :unit))
                                   (let ((annot ,(plist-get plist :annotation)))
                                     (when annot (concat "(" annot ") ")))))
                         ,(when sparkline
                            `(when (window-system)
                               (concat (propertize " " 'display
                                                   (symon--make-sparkline
                                                    lst
                                                    ,(plist-get plist :lower-bound)
                                                    ,(plist-get plist :upper-bound)))
                                       " ")))))))))
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

;; + linux monitors

(defun symon-linux--read-lines (file reader indices)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char 1)
    (mapcar (lambda (index)
              (save-excursion
                (when (search-forward-regexp (concat "^" index "\\(.*\\)$") nil t)
                  (if reader
                      (funcall reader (match-string 1))
                    (match-string 1)))))
            indices)))

(defvar symon-linux--last-cpu-ticks nil)

(define-symon-monitor symon-linux-cpu-monitor
  :index "CPU:" :unit "%" :sparkline t
  :setup (setq symon-linux--last-cpu-ticks nil)
  :fetch (cl-destructuring-bind (cpu)
             (symon-linux--read-lines
              "/proc/stat" (lambda (str) (mapcar 'read (split-string str nil t))) '("cpu"))
           (let ((total (apply '+ cpu)) (idle (nth 3 cpu)))
             (prog1 (when symon-linux--last-cpu-ticks
                      (let ((total-diff (- total (car symon-linux--last-cpu-ticks)))
                            (idle-diff (- idle (cdr symon-linux--last-cpu-ticks))))
                        (/ (* (- total-diff idle-diff) 100) total-diff)))
               (setq symon-linux--last-cpu-ticks (cons total idle))))))

(define-symon-monitor symon-linux-memory-monitor
  :index "MEM:" :unit "%" :sparkline t
  :fetch (cl-destructuring-bind (memtotal memavailable memfree buffers cached)
             (symon-linux--read-lines
              "/proc/meminfo" (lambda (str) (and str (read str)))
              '("MemTotal:" "MemAvailable:" "MemFree:" "Buffers:" "Cached:"))
           (if memavailable
               (/ (* (- memtotal memavailable) 100) memtotal)
             (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal)))
  :annotation (cl-destructuring-bind (swaptotal swapfree)
                  (symon-linux--read-lines
                   "/proc/meminfo" 'read '("SwapTotal:" "SwapFree:"))
                (let ((swapped (/ (- swaptotal swapfree) 1000)))
                  (unless (zerop swapped) (format "%dMB Swapped" swapped)))))

(define-symon-monitor symon-linux-battery-monitor
  :index "BAT:" :unit "%" :sparkline t
  :fetch (when battery-status-function
           (read (cdr (assoc ?p (funcall battery-status-function))))))

(defvar symon-linux--last-network-rx nil)

(define-symon-monitor symon-linux-network-rx-monitor
  :index "RX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-rx-upper-bound
  :fetch (with-temp-buffer
           (insert-file-contents "/proc/net/dev")
           (goto-char 1)
           (let ((rx 0))
             (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
               (unless (string= (match-string 1) "lo")
                 (setq rx (+ rx (read (current-buffer))))))
             (prog1 (when symon-linux--last-network-rx
                      (/ (- rx symon-linux--last-network-rx) symon-refresh-rate 1000))
               (setq symon-linux--last-network-rx rx)))))

(defvar symon-linux--last-network-tx nil)

(define-symon-monitor symon-linux-network-tx-monitor
  :index "TX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-tx-upper-bound
  :fetch (with-temp-buffer
           (insert-file-contents "/proc/net/dev")
           (goto-char 1)
           (let ((tx 0))
             (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
               (unless (string= (match-string 1) "lo")
                 (forward-word 8)
                 (setq tx (+ tx (read (current-buffer))))))
             (prog1 (when symon-linux--last-network-tx
                      (/ (- tx symon-linux--last-network-tx) symon-refresh-rate 1000))
               (setq symon-linux--last-network-tx tx)))))

;; + windows monitors

(defvar symon-windows--wmi-process-reference-count 0)

(defun symon-windows--maybe-start-wmi-process ()
  (setq symon-windows--wmi-process-reference-count
        (1+ symon-windows--wmi-process-reference-count))
  (unless (get-buffer " *symon-wmi*")
    (let ((proc (start-process-shell-command
                 "symon-wmi" " *symon-wmi*"
                 (format
                  (concat "powershell -command while(1) {"
                          "echo ----;"
                          ;; memory usage
                          "$t = (gwmi Win32_ComputerSystem).TotalPhysicalMemory / 1000;"
                          "$f = (gwmi Win32_OperatingSystem).FreePhysicalMemory;"
                          "echo mem:$(($t - $f) * 100 / $t);"
                          ;; page file usage
                          "echo swap:$((gwmi Win32_PageFileUsage).CurrentUsage);"
                          ;; cpu load
                          "$w = gwmi Win32_PerfFormattedData_Counters_ProcessorInformation;"
                          "echo cpu:$($w.get(0).PercentProcessorTime);"
                          ;; battery
                          "echo bat:$((gwmi Win32_Battery).EstimatedChargeRemaining);"
                          ;; rx/tx
                          "$r = 0; $t = 0;"
                          "$w = gwmi Win32_PerfFormattedData_Tcpip_NetworkInterface;"
                          "foreach($x in $w){"
                          "$r = $r + $x.BytesReceivedPersec;"
                          "$t = $t + $x.BytesSentPersec }"
                          "echo rx:$($r / 1000);"
                          "echo tx:$($t / 1000);"
                          ;; loop
                          "sleep %d }") symon-refresh-rate)))
          (filter (lambda (proc str)
                    (when (get-buffer " *symon-wmi*")
                      (with-current-buffer " *symon-wmi*"
                        (when (and (string-match "-" str)
                                   (search-backward "----" nil t))
                          (delete-region 1 (point)))
                        (goto-char (1+ (buffer-size)))
                        (insert str))))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter proc filter))))

(defun symon-windows--maybe-kill-wmi-process ()
  (setq symon-windows--wmi-process-reference-count
        (1- symon-windows--wmi-process-reference-count))
  (when (and (zerop symon-windows--wmi-process-reference-count)
             (get-buffer " *symon-wmi*"))
    (kill-buffer " *symon-wmi*")))

(defun symon-windows--read-value (index)
  (when (get-buffer " *symon-wmi*")
    (with-current-buffer " *symon-wmi*"
      (when (save-excursion
              (search-backward-regexp (concat index ":\\([0-9]+\\)\\>") nil t))
        (read (match-string 1))))))

(define-symon-monitor symon-windows-cpu-monitor
  :index "CPU:" :unit "%" :sparkline t
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon-windows--maybe-kill-wmi-process)
  :fetch (symon-windows--read-value "cpu"))

(define-symon-monitor symon-windows-memory-monitor
  :index "MEM:" :unit "%" :sparkline t
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon-windows--maybe-kill-wmi-process)
  :fetch (symon-windows--read-value "mem")
  :annotation (let ((swapped (symon-windows--read-value "swap")))
                (when (and swapped (> swapped 0))
                  (format "%dMB Swapped" swapped))))

(define-symon-monitor symon-windows-battery-monitor
  :index "BAT:" :unit "%" :sparkline t
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon-windows--maybe-kill-wmi-process)
  :fetch (symon-windows--read-value "bat"))

(define-symon-monitor symon-windows-network-rx-monitor
  :index "RX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-rx-upper-bound
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon-windows--maybe-kill-wmi-process)
  :fetch (symon-windows--read-value "rx"))

(define-symon-monitor symon-windows-network-tx-monitor
  :index "TX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-tx-upper-bound
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon-windows--maybe-kill-wmi-process)
  :fetch (symon-windows--read-value "tx"))

;; + misc monitors

(define-symon-monitor symon-current-time-monitor
  :display (format-time-string "%H:%M"))

;; + provide

(provide 'symon)

;;; symon.el ends here
