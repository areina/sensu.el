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

(defcustom sensu-api-auth-basic ""
  "String with user:password encoded in base64."
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

(defvar sensu-api-default-request-headers
  `(("Content-Type" . "application/json")
    ("Authentication" . ,(concat "Basic " sensu-api-auth-basic)))
  "Default request headers (Content-Type and Authentication).")

(defun sensu-api-get-request (endpoint)
  "Make a GET request to sensu api's ENDPOINT."
  (let ((url (concat sensu-api-base-url endpoint))
	(url-request-method "GET")
	(url-request-extra-headers sensu-api-default-request-headers)
	(json-array-type 'list))
    (with-current-buffer
	(url-retrieve-synchronously url)
      (goto-char (1+ url-http-end-of-headers))
      (unless (= url-http-response-status 200)
	(signal 'sensu-api-error (list "sensu api request failed"
				       url-http-response-status)))
      (prog1 (json-read) (kill-buffer)))))

(defun sensu-api-events ()
  "Request to sensu API to get events."
  (sensu-api-get-request "events"))

;;;; Sensu Main.

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
		   (output (sensu-read-attr 'output event)))
	      (list
	       (sensu-event-id event)
	       (vector
		"" ;; marked. #TODO.
		(propertize (sensu-event-status-text status)
			    'face (sensu-event-status-face status))
		client
		(s-trim output)))))
	  (sensu-api-events)))

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

(defvar sensu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'sensu-next-event)
    (define-key map (kbd "p") 'sensu-prev-event)
    (define-key map (kbd "M-<") 'sensu-first-event)
    (define-key map (kbd "M->") 'sensu-last-event)
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
    ["Refresh" sensu-refresh t]))

;;; Sensu UI - Render

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
