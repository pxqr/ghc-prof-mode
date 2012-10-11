(require 'ghc-prof-indicators)
(require 'ghc-prof-modules)
(require 'ghc-prof-highlighting)
(require 'ghc-prof-navigation)
(require 'ghc-prof-notifications)
(require 'ghc-prof-stats)
(require 'ghc-prof-quick-prof)
(require 'ghc-prof-utils)


(defvar ghc-prof-mode-hook nil)

(defvar ghc-prof-mode-map 
  (let ((map (make-keymap)))
    (define-key map "s" 'ghc-prof-select-report)
    (define-key map "r" 'ghc-prof-update-buffer)
    
    ;; quick profiling
    (define-key map "i" 'ghc-prof-initiate-profiling)
    (define-key map "t" 'ghc-prof-terminate-profiling)
    (define-key map "[" 'ghc-prof-initiate-profiling)
    (define-key map "]" 'ghc-prof-terminate-profiling)

    ;; indicators
    (define-key map "h" 'ghc-prof-highlight)
    (define-key map "c" 'ghc-prof-clear)

    ;; navigation
    (define-key map "g" 'ghc-prof-goto-function)
    (define-key map (kbd "<return>") 'ghc-prof-goto-function)
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

  (use-local-map ghc-prof-mode-map)
  (ghc-prof-pretty-lambda)
  (set (make-local-variable 'font-lock-defaults) '(ghc-prof-font-lock-keywords))
  (setq major-mode 'ghc-prof-mode)

  (setq mode-name "profiling-report")
  (setq mode-line-format (list " %m: " "%b " "(%l) "))

  (ghc-prof-select-report)
  (ghc-prof-watch-buffer)
  (ghc-prof-highlight)

  (run-hooks 'ghc-prof-mode-hook))

(add-hook 'haskell-mode-hook 'ghc-prof-highlight-current)

(provide 'ghc-prof-mode)
