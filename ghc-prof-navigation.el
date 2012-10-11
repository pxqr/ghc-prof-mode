(defun ghc-prof-goto-function ()
  "When in report buffer goto position of function in current line."
  (interactive)
  (let* ((rec (ghc-prof-report-extract-record (ghc-prof-current-line-content)))
         (function-name (car rec))
         (module-name  (cadr rec))
         (buffers (ghc-prof-find-buffer-by-module-name module-name))
         ;; pick the first buffer with the module name 
         (buffer-def (car buffers)))
    (when buffer-def
      (with-current-buffer buffer-def
        (let ((position (ghc-prof-function-position function-name)))
          (when position 
            (switch-to-buffer-other-window buffer-def)
            (goto-char position)
            (beginning-of-line)))))))

(defun ghc-prof-current-line-content ()
  "Get content of current line in current buffer, but not affect on position. "
  (save-excursion 
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (let ((end (point)))
        (buffer-substring-no-properties beg end)))))

(provide 'ghc-prof-navigation)