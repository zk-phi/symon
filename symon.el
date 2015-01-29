(require 'ring)
(require 'cl-lib)
(require 'battery)

;; ---- options

;; set these variables BEFORE calling `symon-initialize'

(defvar symon-history-size 100)
(defvar symon-refresh-rate 2)
(defvar symon-delay 2)
(defvar symon-sparkline-size '(80 . 11))

(defvar symon-monitor
  (cl-case system-type
    ((gnu/linux cygwin) 'symon-default-linux-monitor)
    ((ms-dos windows-nt) 'symon-default-windows-monitor)))

;; ---- symon core

(defvar symon--memory-status nil)
(defvar symon--swap-status nil)
(defvar symon--cpu-status nil)
(defvar symon--battery-status nil)
(defvar symon--displaying nil)

(defun symon--make-ring (size init)
  (cons 0 (cons size (make-vector size init))))

(defun symon-initialize ()
  (interactive)
  (dolist (var '(symon--memory-status
                 symon--swap-status
                 symon--cpu-status
                 symon--battery-status))
    (set var (symon--make-ring symon-history-size nil)))
  (funcall symon-monitor)
  (run-with-timer 0 symon-refresh-rate 'symon--redisplay)
  (run-with-idle-timer symon-delay t 'symon-display)
  (add-hook 'pre-command-hook 'symon-display-end))

(defun symon--make-sparkline (ring)
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
  (interactive)
  (let ((memory (ring-ref symon--memory-status 0))
        (swap (ring-ref symon--swap-status 0))
        (cpu (ring-ref symon--cpu-status 0))
        (battery (ring-ref symon--battery-status 0))
        (message-log-max nil))
    (message
     (concat "MEM:"   (if (integerp memory)  (number-to-string memory)  "N/A") "%% "
             "(SWAP:" (if (integerp swap)    (number-to-string swap)    "N/A") "%%) "
             (propertize " " 'display (symon--make-sparkline symon--memory-status)) " "
             "CPU:"   (if (integerp cpu)     (number-to-string cpu)     "N/A") "%% "
             (propertize " " 'display (symon--make-sparkline symon--cpu-status)) " "
             "BAT:"   (if (integerp battery) (number-to-string battery) "N/A") "%% "
             (propertize " " 'display (symon--make-sparkline symon--battery-status)))))
  (setq symon--displaying t))

(defun symon--redisplay ()
  (when symon--displaying (symon-display)))

(defun symon-display-end ()
  (interactive)
  (setq symon--displaying nil))

;; ---- default linux monitor

(defvar symon--default-linux/last-cpu-ticks '(0 . 0))

(defun symon--default-linux/update-statuses ()
  ;; CPU
  (let* ((str (shell-command-to-string "cat /proc/stat"))
         (_ (string-match "^cpu\\_>\\(.*\\)$" str))
         (lst (mapcar 'read (split-string (match-string 1 str) nil t)))
         (total (apply '+ lst))
         (idle (nth 3 lst))
         (total-diff (- total (car symon--default-linux/last-cpu-ticks)))
         (idle-diff (- idle (cdr symon--default-linux/last-cpu-ticks))))
    (setq symon--default-linux/last-cpu-ticks (cons total idle))
    (ring-insert symon--cpu-status (/ (* (- total-diff idle-diff) 100) total-diff)))
  ;; Memory
  (let ((memory-stats
         (mapcar (lambda (line)
                   (setq line (cdr (split-string line)))
                   (/ (* (read (cadr line)) 100) (read (car line))))
                 (cdr (split-string (shell-command-to-string "free -o -m") "\n" t)))))
    (ring-insert symon--memory-status (car memory-stats))
    (ring-insert symon--swap-status   (cadr memory-stats)))
  ;; Battery
  (ring-insert symon--battery-status
               (read (cdr (assoc ?p (funcall battery-status-function))))))

(defun symon-default-linux-monitor ()
  (run-with-timer 0 symon-refresh-rate 'symon--default-linux/update-statuses))

;; ---- default windows monitor

(defun symon-default-windows-monitor ()
  (set-process-query-on-exit-flag
   (start-process-shell-command
    "symon-typeperf" " *symon-typeperf*"
    (format "typeperf -si %d \"\\Processor(_Total)\\%% Processor Time\""
            symon-refresh-rate))
   nil)
  (run-with-timer 0 symon-refresh-rate 'symon--default-windows/update-statuses))

(defun symon--default-windows/update-statuses ()
  ;; CPU
  (with-current-buffer " *symon-typeperf*"
    (save-excursion
      (if (search-backward-regexp "\",\"\\(.*\\)\"" nil t)
          (ring-insert symon--cpu-status (round (read (match-string 1))))
        (ring-insert symon--cpu-status "N/A"))))
  ;; Memory
  (cl-destructuring-bind (_ ph-total ph-free __ ___ vir-total vir-free . ____)
      (cadr (w32-memory-info))
    (ring-insert symon--memory-status (round (/ (* (- ph-total ph-free) 100) ph-total)))
    (ring-insert symon--swap-status (round (/ (* (- vir-total vir-free) 100) vir-total))))
  ;; Battery
  (ring-insert symon--battery-status (read (cdr (assoc ?p (w32-battery-status))))))

;; ---- provide

(provide 'symon)
