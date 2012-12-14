;;; js-lookup --- quickly look up JavaScript documentation

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Use `js-lookup' to prompt for a topic to look up and open in the
;; browser (`browse-url').

;;; Code:

(require 'cl)
(require 'ido)

(defvar js-lookup-root (file-name-directory load-file-name)
  "Data root directory for JavaScript lookup.")

(defmacro js-lookup-with-base (url &rest body)
  (declare (indent defun)))

(defvar js-lookup-db
  ;; This evaluates a custom DSL, which is why it's messy.
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "js-lookup-database.el" js-lookup-root))
    (flet ((s (symbol) (symbol-name symbol))
           (sc (symbol) (remove ?_ (symbol-name symbol))))
      (eval
       `(let ((path "")
              (table (make-hash-table :test 'equal)))
          (macrolet ((root (part &rest body)
                       `(let ((path (concat path ,part))) ,@body))
                     (category (name &rest items)
                       `(progn
                          (puthash (concat ,(s name))
                                   (concat path ,(s name)) table)
                          ,@(loop for item in items collect
                              `(puthash (concat ,(s name) "." ,(s item))
                                        (concat path ,(s name) "/" ,(sc item))
                                        table)))))
            ,(read (current-buffer)))
          table))))
  "Lookup database mapping items to look up against their URLs.")

(defvar js-lookup-list
  (loop for key being the hash-keys of js-lookup-db collect key)
  "List of items that can be looked up. (The keys of `js-lookup-db'.)")

(defun js-lookup (select)
  "Lookup something related to JavaScript. If called
interactively, prompts the user for an item to look up."
  (interactive (list (ido-completing-read "JS: " js-lookup-list nil t)))
  (browse-url (gethash select js-lookup-db)))

;;; js-lookup.el ends here
