;;; learn-step-one.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  haruspex

;; Author: haruspex <haruspex@haruspex-prime>
;; Keywords: asd

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun count-words-in-buffer ()
  "Return a number of words in the current buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (not (eobp))
	(forward-word 1)
	(setq count (1+ count)))
      (message "Number of words in the buffer: %d" count))))

(defun insert-current-date ()
  "Insert current date in current place in the buffer."
  (interactive)
  (insert (current-time-string)))

(defun toggle-case-region ()
  "Toggle text case."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
	    (end (region-end))
	    (text (buffer-substring-no-properties (region-beginning) (region-end))))
	(delete-region start end)
	(insert
	 (mapconcat
	  (lambda (c)
	    (if (eq (downcase c) c)
		(upcase c)
	      (downcase c)))
	  (string-to-list text)
	  "")))
    (message "Selected text not found.")))
;;;; hello my dear


(provide 'learn-step-one)
;;; learn-step-one.el ends here
