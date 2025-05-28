;;; org-clockify.el --- A simple Org mode interface to Clockify  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <mbork@mbork.pl>
;; Keywords: calendar

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

;; A simple Org-mode interface to Clockify, a time-tracking service.
;; Hooks into the Org-mode's clocking mechanism.

;;; Code:

(require 'json)
(require 'request)
(require 'cl-lib)

(defgroup org-clockify nil
  "An Org mode client for Clockify."
  :group 'applications)

(defcustom org-clockify-api-key "Fill this in your init.el"
  "The API key to authenticate at Clockify."
  :type 'string
  :group 'org-clockify)

(defcustom org-clockify-server-url "https://api.clockify.me"
  "The URL of the Clockify server."
  :type 'string
  :group 'org-clockify)

(defcustom org-clockify-default-timeout 10
  "Default Clockify timeout in seconds."
  :type 'number
  :group 'org-clockify)

(defcustom org-clockify-workspace-id nil
  "The default Clockify workspace id."
  :type 'string
  :group 'org-clockify)

(defun org-clockify--create-url (endpoint)
  "Create the URL for the given ENDPOINT."
  (format "%s/api/v1/%s" org-clockify-server-url endpoint))

(defun org-clockify--make-get-request (endpoint &optional params success-fun error-fun timeout)
  "Make a synchronous request to the Clockify API."
  (request (org-clockify--create-url endpoint)
    :type "GET"
    :params params
    :sync t
    :parser #'json-read
    :headers `(("X-Api-Key" . ,org-clockify-api-key))
    :success success-fun
    :error error-fun
    :timeout (or timeout org-clockify-default-timeout)))

(defun org-clockify--make-post-request (endpoint &optional data success-fun error-fun timeout)
  "Make an asynchronous request to the Clockify API."
  (request (org-clockify--create-url endpoint)
    :type "POST"
    :parser #'json-read
    :headers `(("Content-Type" . "application/json")
	       ("X-Api-Key" . ,org-clockify-api-key))
    :data data
    :success success-fun
    :error error-fun
    :timeout (or timeout org-clockify-default-timeout)))

(defvar org-clockify-workspaces ()
  "List of Clockify workspaces.")

(defun org-clockify-get-workspaces ()
  "Download all workspaces."
  (org-clockify--make-get-request
   "workspaces"
   ()
   (cl-function (lambda (&key data &allow-other-keys)
		  (setq org-clockify-workspaces
			(mapcar (lambda (workspace-data)
				  (list :id (alist-get 'id workspace-data)
					:name (alist-get 'name workspace-data)))
				data))
		  (message "Success!")))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
		  (error "Getting workspaces failed because %s" error-thrown)))))


(defun org-clockify-set-workspace (&optional refresh)
  "Show the workspaces and let the user choose one of them.
Set its id to `org-clockify-workspace-id'.  With prefix argument,
refresh workspaces.  Refresh the projects."
  (interactive "P")
  (when refresh (org-clockify-get-workspaces))
  (if org-clockify-workspaces
      (progn
	(setq org-clockify-workspace-id
	      (plist-get (seq-find
			  (lambda (entry)
			    (string= (plist-get entry :name)
				     (completing-read "Clockify workspace: "
						      (mapcar (lambda (entry)
								(plist-get entry :name))
							      org-clockify-workspaces)
						      nil t)))
			  org-clockify-workspaces)
			 :id))
	(org-clockify-get-projects))
    (error "No workspaces known, try using a prefix argument")))

(defvar org-clockify-projects ()
  "List of projects under the current workspace.")

(defun org-clockify-get-projects ()
  "Download all projects for `org-clockify-workspace-id'."
  (if org-clockify-workspace-id
      (org-clockify--make-get-request
       (format "workspaces/%s/projects" org-clockify-workspace-id)
       '(("page-size" . "999"))
       (cl-function (lambda (&key data &allow-other-keys)
		      (setq org-clockify-projects
			    (mapcar (lambda (workspace-data)
				      (list :id (alist-get 'id workspace-data)
					    :name (alist-get 'name workspace-data)
					    :client (alist-get 'clientName workspace-data)))
				    data))))
       (cl-function (lambda (&key error-thrown &allow-other-keys)
		      (error "Getting projects failed because %s" error-thrown))))
    (error "No workspace set, use `org-clockify-set-workspace' first")))

(defvar org-clockify-project-history nil
  "History variable for `org-clockify-set-project'.")

(defun org-clockify--format-project-name (project-data)
  "Format PROJECT-DATA's name for `org-clockify-set-project'."
  (format "%s::%s"
	  (plist-get project-data :client)
	  (plist-get project-data :name)))

(defun org-clockify-set-project (&optional refresh)
  "Choose the Clockify project for the current headline.
With prefix argument, refresh the project list first."
  (interactive "P")
  (when refresh (org-clockify-get-projects))
  (if org-clockify-projects
      (let* ((project-name (completing-read "Clockify project: "
					    (sort (mapcar #'org-clockify--format-project-name
							  org-clockify-projects)
						  #'string<)
					    nil t nil 'org-clockify-project-history))
	     (project (seq-find (lambda (project-data)
				  (string= (org-clockify--format-project-name project-data)
					   project-name))
				org-clockify-projects)))
	(org-set-property "org-clockify-project-client" (plist-get project :client))
	(org-set-property "org-clockify-project-name" (plist-get project :name))
	(org-set-property "org-clockify-project-id" (plist-get project :id)))
    (error "No projects known, try using a prefix argument")))

(defvar org-clockify-time-entry-start 0
  "The exact Unix timestamp of the last time entry start.")

(defvar org-clockify-time-entry-end 0
  "The exact Unix timestamp of the last time entry end.
At the beginning it is set up to be earlier than anything.")

(defun org-clockify--format-time (&optional unix-time local)
  "Format UNIX-TIME in UTC (or local if LOCAL is non-nil) as ISO-8601."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
		      (seconds-to-time unix-time)
		      (not local)))

(defun org-clockify--format-time-human (&optional unix-time)
  "Format UNIX-TIME for displaying."
  (format-time-string "%H:%M:%S" (seconds-to-time unix-time)))

(defun org-clockify-get-project-id ()
  "Get the project id from the properties of the current headline."
  (org-entry-get-with-inheritance "org-clockify-project-id"))

(defun org-clockify-get-description ()
  "Get the description, that is, the current headline."
  (substring-no-properties (org-get-heading t t t t)))

(defun time-to-unix (&optional time)
  "Convert TIME to an integer number of seconds since epoch."
  (floor (time-to-seconds time)))

(defun org-clockify-clock-start ()
  "Store the current time in `org-clockify-time-entry-start'."
  (setq org-clockify-time-entry-start (time-to-unix)))

(defun org-clockify-clock-cancel ()
  "Store the current time in `org-clockify-time-entry-start'."
  (setq org-clockify-time-entry-start 0))

(defun org-clockify-clock-end ()
  "Submit the finished time entry to Clockify.
For simplicity, use `current-time' instead of parsing the end of
the current clock entry.  This means that if the clock entry was not
stored by Org because `org-clock-out-remove-zero-time-clocks' is
non-nil, the entry still gets submitted."
  (let ((project-id (org-clockify-get-project-id)))
    (when (and (not (zerop org-clockify-time-entry-start))
	       project-id)
      (org-clockify-submit-time-entry
       :start-unix org-clockify-time-entry-start
       :end-unix (time-to-unix)
       :description (org-clockify-get-description)
       :project-id project-id)
      (setq org-clockify-time-entry-start 0))))

(cl-defun org-clockify-submit-time-entry (&key start-unix end-unix description project-id)
  "Submit a time entry to Clockify.
Store the time of the end of the entry in `org-clockify-time-entry-end'."
  (let* ((duration (- end-unix start-unix))
	 (message (format "submitting Clockify time entry \"%s\" (%s - from %s to %s)..."
			  description
			  (format-seconds "%hh %mm %z%ss" duration)
			  (org-clockify--format-time-human start-unix)
			  (org-clockify--format-time-human end-unix))))
    (message message)
    (setq org-clockify-time-entry-end end-unix)
    (org-clockify--make-post-request
     (format "workspaces/%s/time-entries" org-clockify-workspace-id)
     (json-encode `(("start" . ,(org-clockify--format-time start-unix))
		    ("end" . ,(org-clockify--format-time end-unix))
		    ("description" . ,description)
		    ("projectId" . , project-id)))
     (cl-function (lambda (&rest _)
		    (message (format "%sdone" message))))
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
	(message (format "%serror: %s" message error-thrown)))))))

(defun org-clockify-submit-time-entry-at-point ()
  "Submit the clock entry at point to Clockify.
Store the time of the end of the entry in `org-clockify-time-entry-end'."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'clock)
	(let* ((heading (org-clockify-get-description))
	       (project-id (org-clockify-get-project-id))
	       (timestamp (org-element-property :value element))
	       (year-start (org-element-property :year-start timestamp))
	       (month-start (org-element-property :month-start timestamp))
	       (day-start (org-element-property :day-start timestamp))
	       (hour-start (org-element-property :hour-start timestamp))
	       (minute-start (org-element-property :minute-start timestamp))
	       (year-end (org-element-property :year-end timestamp))
	       (month-end (org-element-property :month-end timestamp))
	       (day-end (org-element-property :day-end timestamp))
	       (hour-end (org-element-property :hour-end timestamp))
	       (minute-end (org-element-property :minute-end timestamp))
	       (start-time (encode-time
			    0
			    minute-start
			    hour-start
			    day-start
			    month-start
			    year-start))
	       (stop-time (encode-time
			   0
			   minute-end
			   hour-end
			   day-end
			   month-end
			   year-end)))
	  (org-clockify-submit-time-entry :start-unix (time-to-unix start-time)
					  :end-unix (time-to-unix stop-time)
					  :description heading
					  :project-id project-id))
      (error "No clock at point"))))

(define-minor-mode org-clockify-mode
  "Toggle a (global) minor mode for Org/Clockify integration.
When on, clocking out is automatically submitted to Clockify."
  :init-value nil
  :global t
  :lighter " 🕓"
  (if org-clockify-mode
      (progn
	(add-hook 'org-clock-in-hook #'org-clockify-clock-start)
	(add-hook 'org-clock-out-hook #'org-clockify-clock-end)
	(add-hook 'org-clock-cancel-hook #'org-clockify-clock-cancel))
    (remove-hook 'org-clock-in-hook #'org-clockify-clock-start)
    (remove-hook 'org-clock-out-hook #'org-clockify-clock-end)
    (remove-hook 'org-clock-cancel-hook #'org-clockify-clock-cancel)))

(provide 'org-clockify)
;;; org-clockify.el ends here
