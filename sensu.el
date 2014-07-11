;;; sensu.el --- Manage Sensu from Emacs.

;; Copyright (C) 2014 Toni Reina

;; Author: Toni Reina  <areina0@gmail.com>
;; Version: 0.1
;; Package-Requires: ((s "0.0.0"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

(require 'url-http)
(require 'json)
(require 's)
(require 'dash)

;;; Sensu customizable values.

(defgroup sensu nil
  "Sensu")

(defcustom sensu-user-email ""
  "Your email."
  :group 'sensu)

(defcustom sensu-api-base-url ""
  "Base URL for sensu API.
Ex: http://localhost:4567/ ."
  :group 'sensu)

(defcustom sensu-api-auth-user ""
  "String with user for sensu authorization."
  :group 'sensu)

(defcustom sensu-api-auth-password ""
  "String with password for sensu authorization."
  :group 'sensu)

(defface sensu-red-face
  '((((class color)) :foreground "#cd4d40"))
  "Red color indicating failure."
  :group 'sensu)

(defface sensu-grey-face
  '((((class color)) :foreground "#657b83"))
  "Grey color indicating success."
  :group 'sensu)

(defface sensu-yellow-face
  '((((class color)) :foreground "#e7e24c"))
  "Yellow color used to indicate something that is not success of failure.
An example is restarting a service."
  :group 'sensu)

;;; Sensu API.

(defun sensu-api-auth-credentials ()
  "Return a string with concatenation of \"Basic \" and user:pass in base64."
  (let ((userpass (concat sensu-api-auth-user ":" sensu-api-auth-password)))
    (concat "Basic " (base64-encode-string userpass))))

(defun sensu-api-request-headers ()
  "Return an alist with default headers for a request to Sensu API."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(sensu-api-auth-credentials))))

(defconst sensu-api-valid-responses
  '(("POST" . (201 202))
   ("GET" . (200)))
  "Association list with valid response codes per http method.")

(defun sensu-api-valid-response? (method response-status)
  "Return true if the list of valid response statuses for METHOD contains RESPONSE-STATUS."
  (-contains? (cdr (assoc method sensu-api-valid-responses)) response-status))

(defun sensu-api-request (method endpoint &optional data)
  "Make a http request to Sensu API. "
  (let ((url (concat sensu-api-base-url endpoint))
	(url-request-method method)
	(url-request-data data)
	(url-request-extra-headers (sensu-api-request-headers))
	(json-array-type 'list))
    (with-current-buffer
	(url-retrieve-synchronously url)
      (goto-char (1+ url-http-end-of-headers))
      (unless (sensu-api-valid-response? method url-http-response-status)
	(signal 'sensu-api-error (list "sensu api request failed"
				       url-http-response-status)))
      (prog1 (json-read) (kill-buffer)))))

(defun sensu-api-events ()
  "Request to sensu API to get events."
  (sensu-api-request "GET" "events"))

(defun sensu-api-resolve-event (client check)
  "Request to sensu API to solve an event sending CLIENT and CHECK."
  (let ((data `(("client" . ,client)
		("check" . ,check))))
    (sensu-api-request "POST" "resolve" (json-encode-alist data))))

;;;; Sensu Main.

(defvar sensu-events nil
  "List of events.")

(defun sensu-events ()
  "Return a list of events calling `sensu-api-events'."
  (setq sensu-events (sensu-api-events)))

(defvar sensu-statuses
  '((1 . ("Warning" 'sensu-yellow-face))
    (2 . ("Critical" 'sensu-red-face))
    (3 . ("Unknown" 'sensu-grey-face)))
  "Text equivalences for sensu integer statuses.")

(defun sensu-read-attr (attr event)
  "Get value of ATTR for EVENT alist."
  (cdr (assoc attr event)))

(defun sensu-event-status-text (status)
  "Return the equivalent string of STATUS."
  (car (sensu-read-attr status sensu-statuses)))

(defun sensu-event-status-face (status)
  "Return the face for STATUS."
  (cadr (sensu-read-attr status sensu-statuses)))

(defun sensu-event-id (event)
  "Return EVENT identifier."
  (let ((name (format "%s/%s"
		     (sensu-read-attr 'client event)
		     (sensu-read-attr 'check event))))
    (intern name)))

(defun sensu-list-entries ()
  "Return a list of events prepared for `tabulated-list-entries'.
This list is composed by a event id and a vector with event attrs
\(status, client and output\)."
  (mapcar (lambda (event)
	    (let* ((status (sensu-read-attr 'status event))
		   (client (sensu-read-attr 'client event))
		   (output (sensu-read-attr 'output event))
		   (id (sensu-event-id event)))
	      (list
	       id
	       (vector
		(sensu-marked-col id)
		(propertize (sensu-event-status-text status)
			    'face (sensu-event-status-face status))
		client
		(s-trim output)))))
	  (sensu-events)))

(defun sensu-resolve ()
  ""
  (interactive)
  (sensu-with-refresh
   (-map (lambda (event)
	   (let ((id (sensu-event-id event))
		 (client (sensu-read-attr 'client event))
		 (check (sensu-read-attr 'check event)))
	     (sensu-api-resolve-event client check)
	     (sensu-unmark-event id)))
	 (sensu-marked-events))))

;;; Sensu UI - Navigation

(defun sensu-refresh ()
  "Refresh sensu buffer."
  (interactive)
  (with-current-buffer (sensu-buffer)
    (tabulated-list-print :remember-pos)
    (hl-line-highlight)))

(defun sensu-next-event ()
  "Go to next event."
  (interactive)
  (condition-case nil
      (sensu-goto-next-line)
    (error
     (message "Cannot move further down"))))

(defun sensu-prev-event ()
  "Go to previous event."
  (interactive)
  (condition-case nil
      (sensu-goto-prev-line)
    (error
     (message "Cannot move further up"))))

(defun sensu-first-event ()
  "Go to first event."
  (interactive)
  (sensu-goto-first-line))

(defun sensu-last-event ()
  "Go to last event."
  (interactive)
  (sensu-goto-last-line))

(defun sensu-goto-next-line ()
  "Go to next line."
  (if (= (line-beginning-position 1)
         (line-beginning-position 2))
      (error "No next line")
    (goto-char (line-beginning-position 2))))

(defun sensu-goto-prev-line ()
  "Go to previous line."
  (if (= (line-beginning-position 0)
         (line-beginning-position 1))
      (error "No previous line")
    (goto-char (line-beginning-position 0))))

(defun sensu-goto-first-line ()
  "Go to first line."
  (goto-char (point-min)))

(defun sensu-goto-last-line ()
  "Go to last line."
  (goto-char
   (save-excursion
     (goto-char (point-max))
     (line-beginning-position 0))))

(defun sensu-mark-event ()
  "Mark event at point."
  (interactive)
  (sensu-with-refresh
   (let ((id (tabulated-list-get-id (line-beginning-position 1))))
     (add-to-list 'sensu-marked-events id))))

(defun sensu-unmark-event (&optional id)
  "Unmark event with ID or event at point."
  (interactive)
  (sensu-with-refresh
   (let ((id (or id (tabulated-list-get-id (line-beginning-position 1)))))
     (setq sensu-marked-events (delq id sensu-marked-events)))))

(defun sensu-find-event-by-id (id)
  "Find event by ID."
  (--first (eq (sensu-event-id it) id) sensu-events))

(defvar sensu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'sensu-next-event)
    (define-key map (kbd "p") 'sensu-prev-event)
    (define-key map (kbd "M-<") 'sensu-first-event)
    (define-key map (kbd "M->") 'sensu-last-event)
    (define-key map (kbd "m") 'sensu-mark-event)
    (define-key map (kbd "u") 'sensu-unmark-event)
    (define-key map (kbd "r") 'sensu-refresh)
    map)
  "Keymap for `sensu-mode'.")

(easy-menu-define sensu-mode-menu sensu-mode-map
  "Sensu menu"
  '("Sensu"
    ["Next event" sensu-next-event t]
    ["Previous event" sensu-prev-event t]
    ["First event" sensu-first-event t]
    ["Last service" sensu-last-event t]
    ["Refresh" sensu-refresh t]
    "---"
    ["Mark event" sensu-mark-event]
    ["Unmark event" sensu-unmark-event]))

;;; Sensu UI - Render

(defmacro sensu-with-refresh (&rest body)
  "Execute BODY and then refresh."
  `(progn ,@body (sensu-refresh)))

(defun sensu-refresh ()
  "Refresh list of events."
  (interactive)
  (if (sensu-buffer)
      (with-current-buffer (sensu-buffer)
	(tabulated-list-print :remember-pos)
	(hl-line-highlight))
      ))

(defconst sensu-list-format
  [("Marked" 6 t :right-align t)
   ("Status" 12 t)
   ("Client" 30 t)
   ("Output" 45 nil)]
  "List format.")

(defconst sensu-list-sort-key
  '("Status" . nil)
  "Sort table on this key.")

(defconst sensu-buffer-name "*sensu*"
  "Name of Sensu mode buffer.")

(defvar sensu-marked-events '())

(defun sensu-marked-events ()
  "Return a list of marked events."
  (let ((events
	 (-select (lambda (event)
		    (-contains? sensu-marked-events (sensu-event-id event)))
		  sensu-events)))
    (setq sensu-marked-events (--map (sensu-event-id it ) events))
    events))

(defun sensu-marked-col (event-id)
  "Return EVENT-ID marked column."
  (if (-contains sensu-marked-events event-id) "*" ""))

(defun sensu-buffer ()
  "Return sensu buffer if it exists."
  (get-buffer sensu-buffer-name))

;;;###autoload
(define-derived-mode sensu-mode tabulated-list-mode "Sensu"
  "Special mode for sensu buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Sensu")
  (setq major-mode 'sensu-mode)
  (use-local-map sensu-mode-map)
  (setq tabulated-list-format sensu-list-format)
  (setq tabulated-list-entries 'sensu-list-entries)
  (setq tabulated-list-sort-key sensu-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

;;;###autoload
(defun sensu ()
  "Work with Sensu API from Emacs."
  (interactive)
  (let ((buffer-p (sensu-buffer))
        (buffer (get-buffer-create sensu-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (sensu-mode))))

(provide 'sensu)

;;; sensu.el ends here
