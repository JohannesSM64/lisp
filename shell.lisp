;; Current features:
;; - Basic line editing, history and file name completion (readline)
;; - Multi-word arguments with '"
;; - Escape the next character with \

(defun push-char (chr str)
  (concatenate 'string str (string chr)))

(defun parse (line)
  (let (result (acc "") inquotes backslashed)
    (loop for c across line do
          (case c
            (#\\
             (if backslashed
                 (setq acc (push-char c acc))
                 (setq backslashed t)))
            ((#\Space #\Tab)
             (if (or inquotes backslashed)
                 (progn (setq acc (push-char c acc))
                        (setq backslashed nil))
                 (progn (push acc result)
                        (setq acc ""))))
            ((#\" #\')
             (if backslashed
                 (setq acc (push-char c acc))
                 (progn (setq inquotes (not inquotes))
                        (setq backslashed nil))))
            (t
             (setq acc (push-char c acc)))))
    (if (not (string= acc ""))
        (push acc result))
    (nreverse result)))

(loop (princ "> ")
      (let ((line (parse (read-line))))
        (run-program (car line) :arguments (cdr line))))
