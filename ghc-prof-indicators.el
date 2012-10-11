(defvar ghc-prof-indicators nil)

;; TODO: ghc-prof-fringe-bitmap misbehaving
(defun ghc-prof-create-indicator (bitmap pos side face)
  (let* ((overlay (ghc-prof-insert-bitmap bitmap  pos side face)))
    (push (cons pos overlay) ghc-prof-indicators)))

(defun ghc-prof-insert-bitmap (bitmap pos side face)
  (let* ((display-string `(,side ,bitmap . ,(cons face nil)))
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

(provide 'ghc-prof-indicators)