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

(defvar js-lookup-db (make-hash-table :test 'equal)
  "Lookup database mapping items to look up against their URLs.")

(defvar js-lookup-path ""
  "Current documentation path while loading the database (dynamically bound).")

(defvar js-lookup-list ()
  "List of items that can be looked up. (The keys of `js-lookup-db'.)")

;;;###autoload
(defun js-lookup (select)
  "Lookup something related to JavaScript. If called
interactively, prompts the user for an item to look up."
  (interactive (list (ido-completing-read "JS: " js-lookup-list nil t)))
  (browse-url (apply #'concat (gethash select js-lookup-db))))

;; Database DSL macros

(defmacro js-lookup/root (dir &rest body)
  "Append DIR to the currently established documentation
path. This path, `js-lookup-path' is initially the empty string."
  (declare (indent defun))
  `(let ((js-lookup-path (concat js-lookup-path ,dir))) ,@body))

(defmacro js-lookup/category (category &rest items)
  "Register CATEGORY in the database along with each of its ITEMS."
  (declare (indent defun))
  (let ((name (symbol-name category)))
    `(progn
       (puthash ,name (list js-lookup-path ,name) js-lookup-db)
       ,@(loop for item in items
               for string = (symbol-name item)
               for clean = (remove ?_ string)
               collect `(puthash (concat ,name "." ,string)
                                 (list js-lookup-path ,name "/" ,clean)
                                 js-lookup-db)))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(js-lookup/category\\)[ \t\r\n\f]+\\([^ \t\r\n\f()]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    ("js-lookup/root" . font-lock-keyword-face)))

(provide 'js-lookup)

;; Load the database
(eval-when (load eval)
  (load-file (expand-file-name "js-lookup-database.el" js-lookup-root))
  (setq js-lookup-list
        (loop for key being the hash-keys of js-lookup-db collect key)))

;;; js-lookup.el ends here
