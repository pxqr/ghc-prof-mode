;; stats :: Map ModuleName (Map CentreName Info)
;; Holds current selected report parsed and formed yet.
(defvar ghc-prof-current-stats nil)

(defun ghc-prof-select-report ()
  "Select report opened in current buffer.
   When ghc-prof attempt to find hotspots it will use the last selected report."
  (interactive)
  (setq ghc-prof-current-stats (ghc-prof-form-stats
				 (ghc-prof-report-extract-stats 
				  (buffer-substring-no-properties (point-min) (point-max))))))

(defun ghc-prof-merge-info (alist)
  "Merge info about the same function into one record. Rows are just summed."
  (cons (car alist)
        (apply 'mapcar* 
               (lambda (&rest x) (apply '+ (mapcar 'string-to-number x))) 
               (cdr alist))))

(defun ghc-prof-form-stats (parsed-report)
  (mapcar                                             ; make lookup from function to info and merge info
   (lambda (x) (cons (car x) (mapcar #'ghc-prof-merge-info (make-alist (cdr x)))))
   (make-alist                                        ; make lookup from module name to function->info
    (mapcar 'swap-snd-fst-rest                                
     (remove-if (lambda (x) (string-match "^CAF" (car x)))
                parsed-report)))))

(defun ghc-prof-report-extract-record (line)
  "Extract a profiling record from stats. It can be from either detailed stats or synopsis."
  (let ((xs (split-string (replace-regexp-in-string " *\\(.*\\)" "\\1" line) " +")))
    (cons (replace-regexp-in-string "^\\(.*?\\)\\..*\\'" "\\1" (car xs))
          (cdr xs))))

(defun ghc-prof-report-extract-stats (report)
  "Extract detailed stats from report. Return a table as it have been presented in report."
  (mapcar 'ghc-prof-report-extract-record
	  (cdr (cdr (cdr  ;; skip attribute line, blank line and first meaningless line with MAIN
                     (drop-while 
                      '(lambda (x) (when x (not (string-match "^COST +CENTRE +MODULE +no" x))))
                      (split-string report "\n")))))))

(defun ghc-prof-report-extract-line ()
  "Extract last command line passed before report have been generated."
  (split-string 
   (replace-regexp-in-string "[[:space:]]*\\(.*\\)" "\\1"
                             (buffer-substring-no-properties
                              (ghc-prof-line-position 3) 
                              (- (ghc-prof-line-position 4) 1)))
   " +"))

(provide 'ghc-prof-stats)