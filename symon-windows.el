;;; symon-windows.el --- symon monitors for Windows

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
;; Version: 1.0.0

;;; Change Log:

;; 1.0.0 first release

;;; Code:

(require 'symon)

(defconst symon-windows-version "1.0.0")

(defgroup symon-windows nil
  "symon monitors for Windows."
  :group 'symon)

;; + customs

(defcustom symon-windows-cpu-history-size 50
  "number of old CPU values to keep."
  :group 'symon-windows)

(defcustom symon-windows-memory-history-size 50
  "number of old memory values to keep."
  :group 'symon-windows)

(defcustom symon-windows-battery-history-size 50
  "number of old battery values to keep."
  :group 'symon-windows)

;; + CPU

(define-symon-monitor symon-windows-cpu-monitor
  :index "CPU:" :unit "%"
  :history symon-windows-cpu-history-size
  :setup (set-process-query-on-exit-flag
          (start-process-shell-command
           "symon-typeperf" " *symon-typeperf*"
           (format "typeperf -si %d \"\\Processor(_Total)\\%% Processor Time\""
                   symon-refresh-rate)) nil)
  :cleanup (kill-buffer " *symon-typeperf*")
  :fetch (with-current-buffer " *symon-typeperf*"
           (when (save-excursion
                   (search-backward-regexp "\",\"\\(.*\\)\"" nil t))
             (prog1 (round (read (match-string 1)))
               (delete-region (point-min) (match-beginning 0))))))

;; + memory

(defvar symon-windows--swapped-memory nil)

(define-symon-monitor symon-windows-memory-monitor
  :index "MEM:" :unit "%"
  :history symon-windows-memory-history-size
  :setup (progn
           (setq symon-windows--swapped-memory nil)
           (set-process-query-on-exit-flag
            (start-process-shell-command
             "symon-wmi-memory" " *symon-wmi-memory*"
             (format (concat "powershell -command while(1) {"
                             "echo ----;"
                             "(gwmi Win32_ComputerSystem).TotalPhysicalMemory;"
                             "(gwmi Win32_OperatingSystem).FreePhysicalMemory;"
                             "(gwmi Win32_PageFileUsage).CurrentUsage;"
                             "sleep %d;"
                             "}") symon-refresh-rate)) nil))
  :cleanup (kill-buffer " *symon-wmi-memory*")
  :fetch (with-current-buffer " *symon-wmi-memory*"
           (save-excursion
             (when (search-backward "----" nil t)
               (goto-char (match-end 0))
               (delete-region (point-min) (match-beginning 0))
               (let ((memtotal (floor (/ (read (current-buffer)) 1000)))
                     (memfree (read (current-buffer)))
                     (swap (read (current-buffer))))
                 (setq symon-windows--swapped-memory swap)
                 (/ (* (- memtotal memfree) 100) memtotal)))))
  :annotation (when (and symon-windows--swapped-memory
                         (not (zerop symon-windows--swapped-memory)))
                (format "%dMB Swapped" symon-windows--swapped-memory)))

;; + battery

(define-symon-monitor symon-windows-battery-monitor
  :index "BAT:" :unit "%"
  :history symon-windows-battery-history-size
  :setup (set-process-query-on-exit-flag
          (start-process-shell-command
           "symon-wmi-battery" " *symon-wmi-battery*"
           (format (concat "powershell -command while(1) {"
                           "(gwmi Win32_Battery).EstimatedChargeRemaining;"
                           "sleep %d;"
                           "}") symon-refresh-rate)) nil)
  :cleanup (kill-buffer " *symon-wmi-battery*")
  :fetch (with-current-buffer " *symon-wmi-battery*"
           (save-excursion
             (when (search-backward-regexp "^[0-9]+\\>" nil t)
               (prog1 (read (match-string 0))
                 (delete-region (point-min) (match-beginning 0)))))))

;; + recomended setting

(unless symon-monitors
  (setq symon-monitors '(symon-windows-memory-monitor
                         symon-windows-cpu-monitor
                         symon-windows-battery-monitor)))

;; + provide

(provide 'symon-windows)

;;; symon-windows.el ends here
