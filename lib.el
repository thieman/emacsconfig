(defmacro thieman-after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

(defmacro thieman-when-not-arm (&rest body)
  `(when (or (string-match "darwin" (emacs-version))
             (not (string-match "arm" (emacs-version))))
     (progn ,@body)))
