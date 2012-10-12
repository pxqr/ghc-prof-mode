;; TODO: when process terminates set ghc-prof-profiling-process-handle to nil
(defvar ghc-prof-profiling-process-handle nil)
(defvar ghc-prof-process-terminated-hook  nil)
(defvar ghc-prof-process-spawned-hook     nil)

(defun ghc-prof-initiate-profiling ()
  "Start asynchonously running profiling process. Arguments are extracted from third line of report."
  (interactive)
  (ghc-prof-reset-if-terminated)
  (if ghc-prof-profiling-process-handle 
      (if (y-or-n-p "Profiling process still running. Kill it and start again? ")
          (progn
            (ghc-prof-terminate-profiling)
            (ghc-prof-initiate-profiling)
            (message "Restart process."))
          (message "Continue profiling."))
    (ghc-prof-spawn-process))) 

(defun ghc-prof-terminate-profiling ()
  "Terminate spawned by initiate-profiling process."
  (interactive)
  (ghc-prof-reset-if-terminated)
  (if ghc-prof-profiling-process-handle
      (let ((name (process-name ghc-prof-profiling-process-handle)))
        (interrupt-process ghc-prof-profiling-process-handle)
        (let (status (process-exit-status ghc-prof-profiling-process-handle))
          (message (format "Profiling process '%s' terminated with exit status %s." name status)))
        (ghc-prof-after-terminated))
      (message "No running process yet.")))

(defun ghc-prof-spawn-process ()
  (let* ((line (ghc-prof-report-extract-line)))
    (if line 
        (let* ((progr (car line))
               (args  (cdr line))
               (output-name (concat progr "-output"))
               (proc  (apply (apply-partially 'start-process
                                              "prof-proc"
                                               output-name
                                              (concat default-directory progr))
                             args)))

          (setq ghc-prof-profiling-process-handle proc)
          (run-hooks 'ghc-prof-process-spawned-hook))
      (message "It seems that report is broken."))))

(defun ghc-prof-reset-if-terminated ()
  (when ghc-prof-profiling-process-handle
    (when (not (equal 'run (process-status ghc-prof-profiling-process-handle)))
      (ghc-prof-after-terminated))))

(defun ghc-prof-after-terminated ()
  (run-hooks 'ghc-prof-process-terminated-hook)
  (setq ghc-prof-profiling-process-handle nil))

(provide 'ghc-prof-quick-prof)