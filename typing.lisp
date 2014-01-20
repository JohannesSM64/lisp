(load "utils")

(defvar *lines*
  '("This is not a palindrome"
    "Wine was mixed with water in Ancient Greece"))

(defun test ()
  (let ((cleared 0)
        (lines (shuffle *lines*)))
    (dolist (l lines)
      (prinf l)
      (if (equal (read-line) l)
        (progn (incf cleared)
               (prinf "=== Correct! ==="))
        (prinf "=== Failed! ===")))
    (format t "You cleared ~a out of ~a lines.~%"
            cleared
            (length lines))))
