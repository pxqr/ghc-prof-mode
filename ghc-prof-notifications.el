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

(provide 'ghc-prof-notifications)