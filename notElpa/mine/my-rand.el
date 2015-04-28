(random t) ;seed random with time

;;;###autoload
(defun rand (min max) ;TODO: use a custom implementation for speed.
  (+ min
     (random (+ 1
                (- max min)))))

;;;###autoload
(defun roll ()
  (rand 1 1000))

;;;###autoload
(defun rollCheck (val)
  (>= val (roll)))