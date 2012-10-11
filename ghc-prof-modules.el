(defun ghc-prof-buffer-list ()
  "List of all haskellish buffers."
  (remove-if 
   (lambda (buffer)
     (let ((file-name (buffer-file-name buffer)))
       (or (eq nil file-name) (not (string-match ".+\\.l?hsc?\\'" file-name)))))
   (buffer-list)))

(defconst ghc-prof-module-identif-regexp
  (let ((id-re  "[[:upper:]]\\(\\w\\|'\\)*"))
    (concat "\\(" id-re "\\.\\)*" id-re)))

;;; FIX: if "module Module.Name" precedes real module statement 
;;; then module will be recognized incorrectly.
(defun ghc-prof-extract-module-name (source)
  "Extract from haskell source its module name"
  (let ((mod-re (concat "module +\\(" ghc-prof-module-identif-regexp "\\).*")))
    (car (search-line-by-line 
	  (lambda (line)
	    (when (string-match mod-re line)
	      (replace-regexp-in-string mod-re "\\1" line)))
	  source))))

(defun ghc-prof-find-buffer-by-module-name (module-name)
  (remove-if-not (lambda (buf) 
                   (let ((buf-mod-name
                          (with-current-buffer buf 
                            (ghc-prof-extract-module-name 
                             (buffer-substring-no-properties (point-min) (point-max))))))
                     (when buf-mod-name
                       (equal module-name buf-mod-name))))
                 (ghc-prof-buffer-list)))

(provide 'ghc-prof-modules)