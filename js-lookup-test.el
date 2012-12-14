;;; js-lookup-test.el --- tests for js-lookup

;;; Code:

(require 'js-lookup)

(defun js-lookup-test-urls (urls)
  "Return a list of T and NIL determining if the documentation
URL was valid (200) or not (anything else). Requires curl and
/dev/null."
  (with-temp-buffer
    (apply #'call-process "curl" nil t nil "-sIw" "%{http_code} "
           (loop for url in urls nconc (list "-o" "/dev/null" url)))
    (mapcar (lambda (code) (string= "200" code))
            (split-string (buffer-string)))))

(defun js-lookup-bad-urls ()
  "Return a list of URLs in the database that appear to be
broken. Requires curl and /dev/null."
  (let ((urls (loop for url being the hash-values of js-lookup-db
                    collect (apply #'concat (reverse url)))))
    (mapcan (lambda (url result) (unless result (list url)))
            urls (js-lookup-test-urls urls))))

;;; js-lookup-test.el ends here
