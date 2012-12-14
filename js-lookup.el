;;; js-lookup --- quickly look up JavaScript documentation

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Use `js-lookup' to prompt for a topic to look up and open in the
;; browser (`browse-url').

;;; Code:

(eval-when-compile (require 'cl))
(require 'ido)

(defvar js-lookup-root (file-name-directory load-file-name)
  "Data root directory for JavaScript lookup.")

(defvar js-lookup-base-url "https://developer.mozilla.org/en-US/docs/"
  "Root for JavaScript item lookups.")

(defvar js-lookup-db
  (with-temp-buffer
    (let ((standard-input (current-buffer))
          (db (expand-file-name "js-lookup-database.el" js-lookup-root))
          (table (make-hash-table :test 'equal)))
      (insert-file-contents-literally db)
      (labels ((stringify (name) (if (symbolp name) (symbol-name name) name)))
        (loop while (< (point) (point-max))
              for key = (stringify (read))
              for url = (setf (gethash key table) (stringify (read)))
              do (skip-chars-forward " \t\r\n\f")
              finally (return table)))))
  "Lookup database mapping items to look up against their URLs.")

(defvar js-lookup-list
  (loop for key being the hash-keys of js-lookup-db collect key)
  "List of items that can be looked up. (The keys of `js-lookup-db'.)")

(defun js-lookup (select)
  "Lookup something related to JavaScript. If called
interactively, prompts the user for an item to look up."
  (interactive (list (ido-completing-read "JS: " js-lookup-list nil t)))
  (browse-url (concat js-lookup-base-url (gethash select js-lookup-db))))

;;; js-lookup.el ends here
