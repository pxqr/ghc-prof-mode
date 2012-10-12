(defun ghc-prof-goto-function ()
  "When in report buffer goto position of function in current line."
  (interactive)
  (let* ((rec (ghc-prof-report-extract-record (ghc-prof-current-line-content)))
         (function-name (car rec))
         (module-name  (cadr rec))
         (buffers (remove-if 'nil 
                             (mapcar (lambda (buf) 
                                       (with-current-buffer buf
                                         (let ((position (ghc-prof-function-position function-name)))
                                           (when position
                                             (cons buf (cons position nil))))))
                                     (ghc-prof-find-buffer-by-module-name module-name))))
         ;; pick the first buffer with the module name and the scc name
         (buffer-pos (car buffers)))
    (if buffer-pos
        (progn 
          (switch-to-buffer-other-window (car buffer-pos))
          (goto-char (cadr buffer-pos))
          (beginning-of-line))
        (message "Can't find such function."))))

(defun ghc-prof-current-line-content ()
  "Get content of current line in current buffer, but not affect on position. "
  (save-excursion 
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (let ((end (point)))
        (buffer-substring-no-properties beg end)))))

(provide 'ghc-prof-navigation)