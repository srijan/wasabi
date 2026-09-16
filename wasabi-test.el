;;; wasabi-test.el --- Checks for wasabi -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with: emacs -Q --batch -L . -l wasabi-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'map)
(require 'wasabi)

(ert-deftest wasabi-test-state-silent-refresh-is-preallocated ()
  "`map-put!' signals map-not-inplace unless the key already exists.
Both the HistorySync and OfflineSyncCompleted handlers set :silent-refresh."
  (let ((state (wasabi--make-state :wasabi-buffer (current-buffer))))
    (should (assq :silent-refresh state))
    (map-put! state :silent-refresh t)
    (should (map-elt state :silent-refresh))))

(ert-deftest wasabi-test-notification-sender-resolves-via-contacts ()
  "A live group message must name the sender, not the group.
CONTACT-NAME is the group's name in a group chat, so it can only be a
last-resort fallback."
  (let* ((contacts `((,(intern "123@s.whatsapp.net") . ((:full-name . "Jane Doe")))))
         (parsed (wasabi-chat--parse-notification
                  :p-message '((conversation . "hi"))
                  :p-info '((ID . "MSGID1")
                            (Sender . "123@s.whatsapp.net")
                            (Timestamp . "2025-11-11T12:00:00Z"))
                  :contact-name "Some Group"
                  :chat-jid "999@g.us"
                  :contacts contacts)))
    (should (equal "Jane Doe" (map-elt parsed :sender-name)))
    ;; `wasabi-chat--add-reaction' matches on :message-id; without it a
    ;; reaction to a message that arrived live can never attach.
    (should (equal "MSGID1" (map-elt parsed :message-id)))))

(ert-deftest wasabi-test-notification-sender-falls-back-to-chat-jid ()
  "With no contact and no PushName, fall back rather than erroring."
  (let ((parsed (wasabi-chat--parse-notification
                 :p-message '((conversation . "hi"))
                 :p-info '((ID . "MSGID2") (Sender . "456@s.whatsapp.net"))
                 :contact-name nil
                 :chat-jid "456@s.whatsapp.net"
                 :contacts nil)))
    (should (equal "456" (map-elt parsed :sender-name)))))

(ert-deftest wasabi-test-notification-reaction-shape ()
  (let ((parsed (wasabi-chat--parse-notification
                 :p-message '((reactionMessage . ((key . ((ID . "TARGET1")))
                                                  (text . "👍"))))
                 :p-info '((ID . "MSGID3") (Sender . "123@s.whatsapp.net"))
                 :contact-name "Some Group"
                 :chat-jid "999@g.us"
                 :contacts `((,(intern "123@s.whatsapp.net") . ((:push-name . "Janey")))))))
    (should (map-elt parsed :is-reaction))
    (should (equal "TARGET1" (map-elt parsed :target-id)))
    (should (equal "Janey" (map-elt parsed :sender-name)))))

(ert-deftest wasabi-test-chat-preview-pads-unparsable-timestamp ()
  "An empty string is truthy, so the time column has to collapse to nil."
  (let ((bad (wasabi--format-chat-preview :display-name "X" :is-group nil
                                          :last-updated "2025-11-11 12:00:00+00:00"))
        (none (wasabi--format-chat-preview :display-name "X" :is-group nil
                                           :last-updated nil)))
    (should (equal (substring bad 0 8) (substring none 0 8)))))

;;; LID (linked identity) merging

(defmacro wasabi-test--with-lid-map (pairs &rest body)
  "Run BODY with `wasabi--lid-map' stubbed to PAIRS of (lid-jid . pn-jid)."
  (declare (indent 1))
  `(let ((wasabi--lid-map-cache
          (let ((h (make-hash-table :test 'equal)))
            (dolist (p ,pairs h) (puthash (car p) (cdr p) h)))))
     ,@body))

(ert-deftest wasabi-test-lid-contact-borrows-phone-name ()
  "A LID contact with no name renders as a bare JID otherwise.
Most LID contacts have no entry at all, so this must also create one."
  (wasabi-test--with-lid-map '(("13542417801281@lid" . "12035040027@s.whatsapp.net")
                               ("139062702772332@lid" . "16503986891@s.whatsapp.net"))
    (let* ((contacts `((,(intern "12035040027@s.whatsapp.net") . ((:full-name . "Nidhi")))
                       (,(intern "13542417801281@lid") . ((:full-name . nil) (:push-name . "NG")))
                       (,(intern "16503986891@s.whatsapp.net") . ((:full-name . "Bharti Cook FC")))))
           (enriched (wasabi--enrich-contacts-with-lid-names contacts)))
      ;; "Nidhi" beats the LID's own push-name "NG".
      (should (equal "Nidhi" (map-elt (map-elt enriched (intern "13542417801281@lid")) :full-name)))
      ;; No LID contact existed at all for this one.
      (should (equal "Bharti Cook FC"
                     (map-elt (map-elt enriched (intern "139062702772332@lid")) :full-name)))
      ;; Phone-side contacts are untouched.
      (should (equal "Nidhi" (map-elt (map-elt enriched (intern "12035040027@s.whatsapp.net")) :full-name))))))

(ert-deftest wasabi-test-lid-contact-keeps-existing-name ()
  "Only gaps get filled; a LID that already has a name keeps it."
  (wasabi-test--with-lid-map '(("111@lid" . "222@s.whatsapp.net"))
    (let* ((contacts `((,(intern "222@s.whatsapp.net") . ((:full-name . "Phone Name")))
                       (,(intern "111@lid") . ((:full-name . "Lid Name")))))
           (enriched (wasabi--enrich-contacts-with-lid-names contacts)))
      (should (equal "Lid Name" (map-elt (map-elt enriched (intern "111@lid")) :full-name))))))

(ert-deftest wasabi-test-merge-lid-chats ()
  "The LID is the live identity, so it survives and the phone JID folds in."
  (wasabi-test--with-lid-map '(("13542417801281@lid" . "12035040027@s.whatsapp.net"))
    (let* ((index '(((:chat-jid . "13542417801281@lid") (:display-name . "Nidhi")
                     (:last-updated . "2026-09-16T15:04:00Z"))
                    ((:chat-jid . "12035040027@s.whatsapp.net") (:display-name . "Nidhi")
                     (:last-updated . "2026-09-15T13:32:00Z"))
                    ((:chat-jid . "999@g.us") (:display-name . "Family")
                     (:last-updated . "2026-09-16T15:04:00Z"))))
           (merged (wasabi--merge-lid-chats (copy-tree index))))
      (should (equal 2 (length merged)))
      (let ((nidhi (seq-find (lambda (c) (equal (map-elt c :chat-jid) "13542417801281@lid")) merged)))
        (should nidhi)
        (should (equal '("13542417801281@lid" "12035040027@s.whatsapp.net")
                       (map-elt nidhi :history-jids))))
      ;; The phone-JID row is gone, the group is untouched.
      (should-not (seq-find (lambda (c) (equal (map-elt c :chat-jid) "12035040027@s.whatsapp.net")) merged))
      (should (seq-find (lambda (c) (equal (map-elt c :chat-jid) "999@g.us")) merged)))))

(ert-deftest wasabi-test-merge-leaves-lone-phone-chat-alone ()
  "A phone chat with no LID chat must not be rewritten or dropped."
  (wasabi-test--with-lid-map '(("111@lid" . "222@s.whatsapp.net"))
    (let ((merged (wasabi--merge-lid-chats
                   (copy-tree '(((:chat-jid . "222@s.whatsapp.net") (:last-updated . "x")))))))
      (should (equal 1 (length merged)))
      (should (equal "222@s.whatsapp.net" (map-elt (car merged) :chat-jid)))
      (should-not (map-elt (car merged) :history-jids)))))

(ert-deftest wasabi-test-no-lid-map-is-a-no-op ()
  "No sqlite, no database, no mappings: everything must pass through."
  (wasabi-test--with-lid-map 'nil
    (let ((index '(((:chat-jid . "222@s.whatsapp.net") (:last-updated . "x")))))
      (should (equal index (wasabi--merge-lid-chats (copy-tree index))))
      (should (equal index (wasabi--enrich-contacts-with-lid-names (copy-tree index)))))))

(provide 'wasabi-test)
;;; wasabi-test.el ends here
