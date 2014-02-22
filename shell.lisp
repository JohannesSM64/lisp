(defun parse (line)
  (let (result (acc "") inquotes)
    (loop for c across line do
          (case c
            (#\Space
             (if inquotes
                 (setq acc (concatenate 'string acc (string c)))
                 (progn (push acc result)
                        (setq acc ""))))
            (#\"
             (setq inquotes (not inquotes)))
            (otherwise
              (setq acc (concatenate 'string acc (string c))))))
    (if (not (string= acc ""))
        (push acc result))
    (nreverse result)))

(loop (princ "> ")
      (let ((line (parse (read-line))))
        (run-program (car line) :arguments (cdr line))))
