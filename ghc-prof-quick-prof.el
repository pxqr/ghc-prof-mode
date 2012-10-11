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

(provide 'ghc-prof-quick-prof)