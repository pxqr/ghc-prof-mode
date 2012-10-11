(defun ghc-prof-highlight-current ()
  "Attach indicator to all that possible in current focused buffer."
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
	 (module-name (ghc-prof-extract-module-name content)))
    (when module-name
      (let ((module-stats (cdr (assoc module-name ghc-prof-current-stats))))
	(when module-stats
          (mapc (lambda (cc) 
                  (when cc (ghc-prof-indicate-cost-centre cc)))
                module-stats))))))

(defun ghc-prof-highlight ()
  "Attach indicators to all that possible in all haskell buffers."
  (interactive)
  (ghc-prof-clear)
  (mapc (lambda (x) (with-current-buffer x (ghc-prof-highlight-current)))
	(ghc-prof-buffer-list)))

(defun ghc-prof-clear ()
  "Remove from fringes all indicators in all frames."
  (interactive)
  (ghc-prof-remove-indicators))



(defface ghc-prof-face-low     
  `((((class color)) (:foreground "Green"))) 
  "middle  face")
(defface ghc-prof-face-mid 
  `((((class color)) (:foreground "yellow"))) 
  "middle  face")
(defface ghc-prof-face-high
  `((((class color)) (:foreground "red"))) 
  "middle  face")

(setq ghc-prof-low-boundary 20.0)
(setq ghc-prof-high-boundary 50.0)

(defun ghc-prof-show-color (color)
 (concat "#" (mapconcat (apply-partially 'format "%02X") color "")))

(defun ghc-prof-get-face (k) 
  (if (< k ghc-prof-low-boundary)
      'ghc-prof-face-low
    (if (< k ghc-prof-high-boundary)
	'ghc-prof-face-mid
        'ghc-prof-face-high)))

(defun ghc-prof-function-position (name)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^" name) (point-max) t)))

(defun ghc-prof-indicate-cost-centre (cost-centre)
    (let* ((name (car cost-centre))
           (info (cdr cost-centre))
           (position (ghc-prof-function-position name))
           (indiv-time  (nth 2 info))
           (indiv-alloc (nth 3 info))
           (inher-time  (nth 4 info))
           (inher-alloc (nth 5 info)))
      (when position
        (let ((pos-next (save-excursion 
                          (goto-char position)
                          (forward-line)  
                          (point))))
          (ghc-prof-create-indicator 'filled-square position 'left-fringe  (ghc-prof-get-face inher-time))
          (ghc-prof-create-indicator 'filled-square position 'right-fringe (ghc-prof-get-face inher-alloc))
          (ghc-prof-create-indicator 'hollow-square pos-next 'left-fringe  (ghc-prof-get-face indiv-time))
          (ghc-prof-create-indicator 'hollow-square pos-next 'right-fringe (ghc-prof-get-face indiv-alloc))
          ))))


(provide 'ghc-prof-highlighting)