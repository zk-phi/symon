;;; symon-misc.el --- misc symon monitors

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

(defconst symon-misc-version "1.0.0")

(defgroup symon-misc nil
  "OS independent symon monitors."
  :group 'symon)

;; + time monitor

(define-symon-monitor symon-current-time-monitor
  :display (format-time-string "%H:%M"))

;; + provide

(provide 'symon-misc)

;;; symon-misc.el ends here
