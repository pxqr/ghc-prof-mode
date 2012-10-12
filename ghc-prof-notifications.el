;; Now notifications (file changed) works through polling.
;; TODO: try to interface with DBus or inotify.
;; (defvar ghc-prof-managed-timers nil)

(defun ghc-prof-update-buffer ()
  "Since report is read only update buffer without confirmation."
  (interactive)
  (if (ghc-prof-check-if-busy (current-buffer))
      (if (y-or-n-p "Profiling process not terminated yet possibly. You sure that you want to update report? ")
          (ghc-prof-update-report-buffer)
          (message "Keep current report."))
      (ghc-prof-update-report-buffer)))

(defun ghc-prof-update-report-buffer ()
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
    ; we don't want get notifications when profiling process just emptied file
    (when (not (ghc-prof-check-if-busy buffer))
      (with-current-buffer buffer 
        (setq header-line-format 
              (concat 
               (propertize "It seems like you get a new report. " 
                           'face '(:background "#f00"))
               " Press r to show the new report."))))))

(defun ghc-prof-check-if-busy (buffer)
  (equal 0 (nth 7 (file-attributes (buffer-file-name buffer)))))

(provide 'ghc-prof-notifications)