;; ============================ mode related  =======================================
;; This file
(defvar ghc-prof-mode-hook nil)

;; keymap
; in haskell-mode
;    (define-key map "\C-j" 'goto-scc-report-info)    
; in ghc-prof-mode
;    (define-key map "\C-g C-t" 'goto-scc-source) 
(defvar ghc-prof-mode-map 
  "Keymap for ghc-prof major mode"
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map))

;; set ghc-prof-mode when .prof file is opened
(add-to-list 'auto-mode-alist '("\\.prof\\'" . ghc-prof-mode))

(defconst ghc-prof-font-lock-keywords
  (list
   '("[[:digit:]]+\\.[[:digit:]]+" . font-lock-variable-name-face))
  "Minimal highlighting for ghc-prof mode")

(defun ghc-prof-mode ()
  "Major mode for viewing ghc profiling reports"
  (interactive)
  (kill-all-local-variables)
;  (set-syntax-table 
  (use-local-map ghc-prof-mode-map)
  (set (make-local-variable 'font-lock-defaults) 
       '(ghc-prof-font-lock-keywords))
  (setq major-mode 'ghc-prof-mode)
  (setq mode-name "ghc prof report")
  (toggle-read-only) ;; make opened buffer read only
  (ghc-prof-select-report)
  (run-hooks 'ghc-prof-mode-hook))

;; ========================= utils    ========================================
(defun search-line-by-line (pred str)
  "Apply function to each line and filter all returned nils."
  (remove-if 'nil (mapcar pred (split-string str "\n" t))))

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

(defun make-alist (list)  
  (mapcar (lambda (x) (cons (caar x) (mapcar 'cdr x))) 
	  (group-by (lambda (p q) (string= (car p) (car q)))
		    (sort list (lambda (p q) (string< (car p) (car q)))))))


(defun swap-snd-fst-rest (x)
  (cons (cadr x) (cons (car x) (cddr x))))

;; ========================= buffers  ========================================
(defun ghc-prof-buffer-list ()
  "List all haskellish buffers."
  (interactive)
  (remove-if 
   (lambda (buffer)
     (let ((file-name (buffer-file-name buffer)))
       (or (eq nil file-name) (not (string-match ".+\\.l?hs" file-name)))))
   (buffer-list)))

;; FIX: if "module Module.Name" precedes real module statement 
;; then module will be recognized incorrectly.
(defun ghc-prof-extract-module-name (source)
  "Extract from haskell source its module name"
  (let* ((id-re  "[[:upper:]]\\(\\w\\|'\\)*")
	 (hid-re (concat "\\(" id-re "\\.\\)*" id-re))
	 (mod-re (concat "module +\\(" hid-re "\\).*")))
    (car (search-line-by-line 
	  (lambda (line)
	    (when (string-match mod-re line)
	      (replace-regexp-in-string mod-re "\\1" line)))
	  source))))
               
;; ======================== report ==============================================
; :: Map ModuleName (Map CentreName Info)
(defvar ghc-prof-current-stats nil)

(defun ghc-prof-select-report ()
  "Select report opened in current buffer.
  When ghc-prof attempt to find hotspots it will use the last selected report."
  (interactive)
  (setq ghc-prof-current-stats (ghc-prof-form-stats
				 (ghc-prof-report-extract-stats 
				  (buffer-substring-no-properties (point-min) (point-max))))))  

(defun ghc-prof-form-stats (parsed-report)
  (mapcar      ; make lookup from function to info
   (lambda (x) (cons (car x) (make-alist (cdr x)))) 
   (make-alist ; make lookup from module name to function->info
    (mapcar 'swap-snd-fst-rest parsed-report))))

(defun ghc-prof-report-extract-stats (report)
  "Extract detailed stats from report. Return a table as it have been presented in report."
  (mapcar '(lambda (x) (split-string (replace-regexp-in-string " *\\(.*\\)" "\\1" x) " +")) 
	  (cdr (cdr (drop-while 
		     '(lambda (x) (not (string-match "^COST CENTRE MODULE +no." x)))
		     (split-string report "\n"))))))

;; ========================== some  math          ============================
(defun ghc-prof-mean (list)
  (/ (apply '+ list) (list-length list)))

(defun ghc-prof-std (list)
  )

;; ========================== interactive         ============================
(defun ghc-prof-hotspot-current ()
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

(defun ghc-prof-hotspots ()
  (interactive)
  (ghc-prof-clear)
  (mapc (lambda (x) (with-current-buffer x (ghc-prof-mark-buffer)))
	(ghc-prof-buffer-list)))

(defun ghc-prof-clear ()
  (interactive)
  (ghc-prof-remove-indicators))

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
    (ghc-prof-create-indicator position 'vertical-bar)))

;(if (not (ghc-which ghc-module-command))
;    (message "%s not found" ghc-module-command)

;; ==========================  Fringes =======================================
;; pieces of code taken from rfringes minor mode
;  (ghc-prof-create-indicator 6000 'vertical-bar)
;  (ghc-prof-remove-indicators)
; (ghc-prof-unmark)

(defvar ghc-prof-indicators nil)
(define-fringe-bitmap 'ghc-prof-fringe-bitmap [255 0])

(defun ghc-prof-create-indicator (line-number bitmap)
  (let ((overlay (ghc-prof-insert-bitmap bitmap 
					 (ghc-prof-line-position line-number) 
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
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line-number 1))
    (point)))

(defun ghc-prof-remove-indicators ()
  (if ghc-prof-indicators
      (progn
        (mapc (lambda (pair) (delete-overlay (cdr pair)))
              ghc-prof-indicators)
        (setq ghc-prof-indicators nil))))


(provide 'ghc-prof-mode)
