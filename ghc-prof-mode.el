;;; ============================ mode related  ===============================
(defvar ghc-prof-mode-hook nil)

(defvar ghc-prof-mode-map 
  (let ((map (make-keymap)))
    (define-key map "s" 'ghc-prof-select-report)
    (define-key map "r" 'ghc-prof-update-buffer)
    
    (define-key map "i" 'ghc-prof-initiate-profiling)
    (define-key map "t" 'ghc-prof-terminate-profiling)
    (define-key map "[" 'ghc-prof-initiate-profiling)
    (define-key map "]" 'ghc-prof-terminate-profiling)

    (define-key map "h" 'ghc-prof-highlight)
    (define-key map "c" 'ghc-prof-clear)
    map))

;;; set ghc-prof-mode when .prof file is opened
(add-to-list 'auto-mode-alist '("\\.prof\\'" . ghc-prof-mode))

(defun ghc-prof-pretty-lambda (&optional mode)
  (font-lock-add-keywords 'ghc-prof-mode
                          `(("\\\\"
                             (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                                       ,(make-char 'greek-iso8859-7 107))
                                       nil))))))

(defconst ghc-prof-font-lock-keywords
  (list 
   '("COST.*"                      . font-lock-keyword-face)
   '("\\<0\\.0\\>"                      . font-lock-constant-face)
   '("\\<[[:digit:]]+\\(\\(\\.\\|,\\)[[:digit:]]+\\)*\\>" . font-lock-variable-name-face)
   '(ghc-prof-module-identif-regexp . font-lock-type-face)
   ))

(defun ghc-prof-mode ()
  "Major mode for viewing ghc profiling reports."
  (interactive)

  (toggle-read-only)                  ; make opened buffer read only
  (kill-all-local-variables)
  ;; (set-syntax-table 
  (use-local-map ghc-prof-mode-map)
  (ghc-prof-pretty-lambda)
  (set (make-local-variable 'font-lock-defaults) '(ghc-prof-font-lock-keywords))
  (setq major-mode 'ghc-prof-mode)

  (setq mode-name "profiling-report")
  (setq mode-line-format (list " %m: " "%b " "(%l) "))

  (ghc-prof-select-report)
  (ghc-prof-watch-buffer)
  (run-hooks 'ghc-prof-mode-hook))

;;; ======================== notifications ===================================
;; Now notifications (file changed) works through polling.
;; TODO: try to interface with DBus or inotify.
;; (defvar ghc-prof-managed-timers nil)

(defun ghc-prof-update-buffer ()
  "Since report is read only update buffer without confirmation."
  (interactive) 
  (revert-buffer t t)
  (setq header-line-format nil)
  (message "Report have been updated."))

;;; since current buffer can vary, associated buffer handle holded on in closure
(defun ghc-prof-watch-buffer ()
  (run-at-time 0 2 'ghc-prof-check-external-modifications (current-buffer)))

;;; TODO!
(defun ghc-prof-kill-timer ()
  )

(defun ghc-prof-check-external-modifications (buffer)
  (when (not (verify-visited-file-modtime buffer))
    (with-current-buffer buffer 
      (setq header-line-format 
            (concat 
             (propertize "It seems like you get a new report. " 
                         'face '(:background "#f00"))
             " Press r to show the new report.")))))

;; ========================= utils    ========================================
(defun swap-snd-fst-rest (x)
  (cons (cadr x) (cons (car x) (cddr x))))

(defun search-line-by-line (pred str)
  "Apply function to each line and filter all returned nils."
  (remove-if 'nil (mapcar pred (split-string str "\n" t))))

(defun make-alist (list)  
  "Make associative list from list of pairs. 
   Dublicates of keys are discarded and corresponding values are merged in list"
  (mapcar (lambda (x) (cons (caar x) (mapcar 'cdr x))) 
	  (group-by (lambda (p q) (string= (car p) (car q)))
		    (sort list (lambda (p q) (string< (car p) (car q)))))))

;;; haskell-like functions
(defun drop-while (f list)
  (let ((res list))
    (while (funcall f (car res)) (pop res))
    res))

(defun foldl (f x list)
  (let ((li list) (x2 x))
    (while li (setq x2 (funcall f x2 (pop li))))
    x2))

(defun group-by (pred list)
  (foldl (lambda (xs x) 
	   (if (funcall pred (caar xs) x)
	       (cons (cons x (car xs)) (cdr xs))
	       (cons (cons x nil) xs)))
	 '()
	 list))


;;; ========================= buffers  ========================================
;;; unfortunately ghc-mod info does not support hsc (haskell interfaces to C)
(defun ghc-prof-buffer-list ()
  "List of all haskellish buffers."
  (remove-if 
   (lambda (buffer)
     (let ((file-name (buffer-file-name buffer)))
       (or (eq nil file-name) (not (string-match ".+\\.l?hs\\'" file-name)))))
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
               
;;; ======================== report ==============================================
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

;; TODO: verify 
(defun ghc-prof-merge-info (alist)
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
                (mapcar (lambda (x)                        ; drop 
                          (cons (replace-regexp-in-string "^\\(.*?\\)\\..*\\'" "\\1" (car x))
                                (cdr x)))
                parsed-report))))))

(defun ghc-prof-report-extract-stats (report)
  "Extract detailed stats from report. Return a table as it have been presented in report."
  (mapcar '(lambda (x) (split-string (replace-regexp-in-string " *\\(.*\\)" "\\1" x) " +")) 
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

;;; ========================== some  math          ============================
;;; TODO: we can make it in one pass.
(defun ghc-prof-mean (list)
  (/ (apply '+ list) (list-length list)))

(defun ghc-prof-std (list) )

;;; ========================== interactive         ============================
(defun ghc-prof-highlight-current ()
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
	 (module-name (ghc-prof-extract-module-name content)))
    (when module-name
      (let ((module-stats (cdr (assoc module-name ghc-prof-current-stats))))
	(when module-stats
	  (let ((file-name (buffer-file-name (current-buffer))))
	    (when file-name
	      (mapc (lambda (cost-centre) 
		      (ghc-prof-indicate-cost-centre file-name module-name (car cost-centre)))
		    module-stats))))))))

(defun ghc-prof-highlight ()
  (interactive)
  (ghc-prof-clear)
  (mapc (lambda (x) (with-current-buffer x (ghc-prof-highlight-current)))
	(ghc-prof-buffer-list)))

(defun ghc-prof-clear ()
  (interactive)
  (ghc-prof-remove-indicators))

(defvar ghc-prof-profiling-process-handle nil)

(defun ghc-prof-initiate-profiling ()
  "Starts profiling process. It takes parameters from report and put it in shell."
  (interactive)
  (if ghc-prof-profiling-process-handle
      (if (y-or-n-p "Profiling process still running. Kill it and start again? ")
          (progn
            (ghc-prof-terminate-profiling)
            (ghc-prof-initiate-profiling))
          (message "Continue profiling."))
    (let* ((line (ghc-prof-report-extract-line)))
           (if t
               (let* ((progr (car line))
                      (args  (cdr line)) 
                      (proc  (apply (apply-partially 'start-process-shell-command 
                                                     "prof-proc" 
                                                     (concat progr "-output") 
                                                     (concat "./" progr))
                                   args)))
                 (setq ghc-prof-profiling-process-handle proc))
               (message "It seems that report is broken.")))))

(defun ghc-prof-terminate-profiling ()
  "Terminate spawned by initiate-profiling process."
  (interactive)
  (if ghc-prof-profiling-process-handle
      (let ((name (process-name ghc-prof-profiling-process-handle)))
        (interrupt-process ghc-prof-profiling-process-handle)
        (let (status (process-exit-status ghc-prof-profiling-process-handle))
          (message (format "Profiling process '%s' terminated with exit status %s." name status)))
        (setq ghc-prof-profiling-process-handle nil))
      (message "No running process yet.")))
  
;; ========================== ghc-mod interfacing ============================
(defun ghc-prof-function-info (file-name module name)
  (process-lines "ghc-mod" "info" file-name module name))

(defun ghc-prof-position-from-info (info)
  (string-to-number
   (replace-regexp-in-string ".*:\\([[:digit:]]+\\):[[:digit:]]+\\'" "\\1" 
			    (car (last info)))))

(defun ghc-prof-indicate-cost-centre (file-name module-name cost-centre)
  (let ((position (ghc-prof-position-from-info 
		   (ghc-prof-function-info file-name module-name cost-centre))))
    (when position
      (ghc-prof-create-indicator position 'vertical-bar))))

;;; TODO: Check if ghc-mod is available but not in inner loop obviously.
;;; (if (not (ghc-which ghc-module-command))
;;;    (message "%s not found" ghc-module-command)

;; ==========================  Fringes =======================================
;;; TODO: Add color and side parameters to ghc-prof-create-indicator.
(defvar ghc-prof-indicators nil)
(define-fringe-bitmap 'ghc-prof-fringe-bitmap [255 0])

(defun ghc-prof-create-indicator (line-number bitmap)
  (let* ((pos (ghc-prof-line-position line-number))
         (overlay (ghc-prof-insert-bitmap bitmap 
					  pos
                                          'left-fringe 
                                          'font-lock-warning-face)))
    (push (cons pos overlay) ghc-prof-indicators)))

(defun ghc-prof-insert-bitmap (bitmap pos &optional side face)
  (let* ((display-string `(,(or side 'left-fringe) ,bitmap .
                           ,(when face (cons face nil))))
	 (before-string (propertize "!" 'display display-string))
	 (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string before-string)
    (overlay-put ov 'fringe-helper t)
    ov))

(defun ghc-prof-line-position (line-number)
  "Get start position for nth line. Index starts from one, so first line have index one."
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line-number 1))
    (point)))

(defun ghc-prof-remove-indicators ()
  "Remove all managed indicators in all buffers."
  (if ghc-prof-indicators
      (progn
        (mapc (lambda (pair) (delete-overlay (cdr pair)))
              ghc-prof-indicators)
        (setq ghc-prof-indicators nil))))

(provide 'ghc-prof-mode)
