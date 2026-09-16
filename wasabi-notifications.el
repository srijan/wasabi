;;; wasabi-notifications.el --- A WhatsApp Emacs client  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/wasabi

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; wasabi-notifications provide the notification functionality for
;;; wasabi. Notifications can: be disabled, use the built-in notifications, use
;;; knockknock package (https://github.com/konrad1977/knockknock) or a custom
;;; function.


;;; Code:

(defcustom wasabi-message-notification-function 'notifications
  "Function or symbol to handle message notifications.

This can be:
- nil to disable notifications.
- The symbol `notifications` for using the built-in notifications system.
- The symbol `knockknock` for using the Knockknock notification system.
- A custom function, which will be called with `funcall`."
  :type
  '(choice
    (const :tag "Disabled" nil)
    (const :tag "Use notifications" notifications)
    (const :tag "Use knockknock" knockknock)
    (function :tag "Custom function"))
  :group 'wasabi)

(defvar wasabi--notifications-unsupported-warned nil
  "Non-nil once we've said the built-in notifications backend can't run here.")

(defun wasabi--notify (message)
  "Display a notification with MESSAGE if needed."
  (when wasabi-message-notification-function
    (cond
     ((eq wasabi-message-notification-function 'notifications)
      ;; `notifications-notify' is D-Bus only. Emacs built without it (the
      ;; stock macOS build) would signal once per incoming message.
      (if (featurep 'dbusbind)
          (wasabi--notify-with-notifications message)
        (unless wasabi--notifications-unsupported-warned
          (setq wasabi--notifications-unsupported-warned t)
          (message "Wasabi: notifications need D-Bus, which this Emacs lacks. Set `wasabi-message-notification-function' to nil or `knockknock'."))))
     ((eq wasabi-message-notification-function 'knockknock)
      (wasabi--notify-with-knockknock message))
     ((functionp wasabi-message-notification-function)
      (funcall wasabi-message-notification-function message)))))

(defun wasabi--get-msg-content (target-id)
  "Get the content of the message with TARGET-ID."
  (let ((msg
         (seq-find
          (lambda (msg)
            (string= (map-elt msg :message-id) target-id))
          (map-elt wasabi-chat--chat :messages))))
    (when msg
      (map-elt msg :content))))

(defun wasabi--notify-with-notifications (message)
  "Display a notification with MESSAGE using the `notifications' package."
  (when (featurep 'notifications)
    (if (map-elt message :is-reaction)
        (notifications-notify
         :title (map-elt message :sender-name)
         :body
         (concat
          "Reacted to "
          (wasabi--get-msg-content (map-elt message :target-id))
          " with: "
          (map-elt message :emoji))
         :app-name "Wasabi"
         :app-icon (wasabi-icon--svg-file)
         :urgency 'normal)
      (notifications-notify
       :title (map-elt message :sender-name)
       :body (map-elt message :content)
       :app-name "Wasabi"
       :app-icon (wasabi-icon--svg-file)
       :urgency 'normal))))

(defun wasabi--notify-with-knockknock (message)
  "Display a notification with MESSAGE using the `knockknock' package."
  (when (featurep 'knockknock)
    (if (map-elt message :is-reaction)
        (knockknock-notify
         :title (map-elt message :sender-name)
         :message
         (concat
          "Reacted to "
          (wasabi--get-msg-content (map-elt message :target-id))
          " with: "
          (map-elt message :emoji))
         :app-name "Wasabi"
         :icon-file (wasabi-icon--svg-file))
      (knockknock-notify
       :title (map-elt message :sender-name)
       :message (map-elt message :content)
       :app-name "Wasabi"
       :icon-file (wasabi-icon--svg-file)))))


(provide 'wasabi-notifications)
;;; wasabi-notifications.el ends here
