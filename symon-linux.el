;;; symon-linux.el --- symon monitors for GNU/Linux

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
(require 'battery)

(defconst symon-linux-version "1.0.0")

(defgroup symon-linux nil
  "symon monitors for GNU/Linux."
  :group 'symon)

;; + customs

(defcustom symon-linux-cpu-history-size 50
  "number of old CPU status values to keep."
  :group 'symon-linux)

(defcustom symon-linux-memory-history-size 50
  "number of old memory status values to keep."
  :group 'symon-linux)

(defcustom symon-linux-battery-history-size 50
  "number of old battery status values to keep."
  :group 'symon-linux)

;; + utilities

(defun symon--file-contents (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;; + CPU monitor

(defvar symon-linux--last-cpu-ticks nil)

(define-symon-monitor symon-linux-cpu-monitor
  :index "CPU:" :unit "%"
  :history symon-linux-cpu-history-size
  :setup   (setq symon-linux--last-cpu-ticks nil)
  :fetch   (let* ((str (symon--file-contents "/proc/stat"))
                  (_ (string-match "^cpu\\_>\\(.*\\)$" str))
                  (lst (mapcar 'read (split-string (match-string 1 str) nil t)))
                  (total (apply '+ lst))
                  (idle (nth 3 lst)))
             (prog1 (when symon-linux--last-cpu-ticks
                      (let ((total-diff (- total (car symon-linux--last-cpu-ticks)))
                            (idle-diff (- idle (cdr symon-linux--last-cpu-ticks))))
                        (/ (* (- total-diff idle-diff) 100) total-diff)))
               (setq symon-linux--last-cpu-ticks (cons total idle)))))

;; + memory monitor

(defvar symon-linux--swapped-meomry nil)

(define-symon-monitor symon-linux-memory-monitor
  :index "MEM:" :unit "%"
  :history    symon-linux-memory-history-size
  :fetch      (let* ((str (symon--file-contents "/proc/meminfo"))
                     (_ (string-match "^SwapTotal:[\s\t]*\\([0-9]+\\)\\>" str))
                     (swaptotal (read (match-string 1 str)))
                     (_ (string-match "^SwapFree:[\s\t]*\\([0-9]+\\)\\>" str))
                     (swapfree (read (match-string 1 str)))
                     (_ (string-match "^MemTotal:[\s\t]*\\([0-9]+\\)\\>" str))
                     (memtotal (read (match-string 1 str))))
                (setq symon-linux--swapped-meomry (/ (- swaptotal swapfree) 1000))
                (if (string-match "^MemAvailable:[\s\t]*\\([0-9]+\\)\\>" str)
                    (/ (* (- memtotal (read (match-string 1 str))) 100) memtotal)
                  (let* ((_ (string-match "^MemFree:[\s\t]*\\([0-9]+\\)\\>" str))
                         (memfree (read (match-string 1 str)))
                         (_ (string-match "^Buffers:[\s\t]*\\([0-9]+\\)\\>" str))
                         (buffers (read (match-string 1 str)))
                         (_ (string-match "^Cached:[\s\t]*\\([0-9]+\\)\\>" str))
                         (cached (read (match-string 1 str))))
                    (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal))))
  :annotation (when (and symon-linux--swapped-meomry
                         (not (zerop symon-linux--swapped-meomry)))
                (format "%dMB Swapped" symon-linux--swapped-meomry)))

;; + battery monitor

(define-symon-monitor symon-linux-battery-monitor
  :index "BAT:" :unit "%"
  :history symon-linux-battery-history-size
  :setup   (require 'battery)
  :fetch   (when battery-status-function
             (read (cdr (assoc ?p (funcall battery-status-function))))))

;; + recomended setting

(unless symon-monitors
  (setq symon-monitors '(symon-linux-memory-monitor
                         symon-linux-cpu-monitor
                         symon-linux-battery-monitor)))

;; + provide

(provide 'symon-linux)

;;; symon-linux.el ends here
