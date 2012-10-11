(defun swap-snd-fst-rest (x)
  (cons (cadr x) (cons (car x) (cddr x))))

(defun search-line-by-line (pred str)
  "Apply function to each line and filter all returned nils."
  (remove-if 'nil (mapcar pred (split-string str "\n" t))))

(defun make-alist (list)  
  "Make associative list from list of pairs. 
   Dublicates of keys are discarded and corresponding values are merged in list"
  (mapcar (lambda (x) (cons (caar x) (mapcar 'cdr x))) 
	  (group-by (lambda (p q) (string= (car p) (car q)))
		    (sort list (lambda (p q) (string< (car p) (car q)))))))

;;; =====================  haskell-like functions ==============================
(defun drop-while (f list)
  (let ((res list))
    (while (funcall f (car res)) (pop res))
    res))

(defun foldl (f x list)
  (let ((li list) (x2 x))
    (while li (setq x2 (funcall f x2 (pop li))))
    x2))

(defun group-by (pred list)
  (foldl (lambda (xs x) 
	   (if (funcall pred (caar xs) x)
	       (cons (cons x (car xs)) (cdr xs))
	       (cons (cons x nil) xs)))
	 '()
	 list))

;;; ========================== some  math          ============================
(defun ghc-prof-lepr (k a b)
  "Linear interpolation. It takes two ints and blending floating coeff."
  (round (+ (* (float a) (- 1.0 k)) (* (float b) k))))

(defun ghc-prof-lepr-list (k a b)
  (mapcar* (apply-partially 'ghc-prof-lepr k) a b))

;;; TODO: we can make it in one pass.
(defun ghc-prof-mean (list)
  (/ (apply '+ list) (list-length list)))

(defun ghc-prof-std (list) )


(provide 'ghc-prof-utils)