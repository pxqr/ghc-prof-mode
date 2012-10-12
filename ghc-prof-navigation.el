(defun ghc-prof-goto-function ()
  "When in report buffer goto position of function in current line."
  (interactive)
  (let* ((rec (ghc-prof-report-extract-record (ghc-prof-current-line-content)))
         (function-name (car rec))
         (module-name  (cadr rec))
         (buffers (ghc-prof-search-function module-name function-name))
         (bufs-len (length buffers)))
    (cond ((= bufs-len 0) (message "Can't find such function."))
          ((= bufs-len 1) (ghc-prof-focus-function (car-safe buffers)))
          (t (let* ((prompt-hint (mapcar (lambda (buf-pos) 
                                           (format " %s "
                                                   (buffer-file-name (car buf-pos)))) buffers))
                    (answer (read-from-minibuffer 
                             (apply 'concat (cons "Ambiguous! Choose number of one you want: " prompt-hint))
                              "1" nil 'string-to-number)))
               (when (and (/= answer 0) (<= answer bufs-len))
                 (ghc-prof-focus-function (nth (- answer 1) buffers))))))))

(defun ghc-prof-focus-function (buffer-pos)
  (switch-to-buffer-other-window (car buffer-pos))
  (goto-char (cadr buffer-pos))
  (beginning-of-line))

;;; TODO: Can't find `function-name` in `module-name`, but it in `module-name` or `buffer-name`. Jump to it?

(defun ghc-prof-search-function (module-name function-name)
  (remove-if 'nil 
             (mapcar (lambda (buf)
                       (with-current-buffer buf
                         (let ((position (ghc-prof-function-position function-name)))
                           (when position
                             (cons buf (cons position nil))))))
                     (ghc-prof-find-buffer-by-module-name module-name))))

(defun ghc-prof-current-line-content ()
  "Get content of current line in current buffer, but not affect on position. "
  (save-excursion 
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (let ((end (point)))
        (buffer-substring-no-properties beg end)))))

(provide 'ghc-prof-navigation)