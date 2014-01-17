(load "utils.lisp")

(defvar *lines*
  '("This is not a palindrome"
    "Wine was mixed with water in Ancient Greece"))

(defun test ()
  (let ((cleared 0)
        (lines (shuffle *lines*)))
    (unwind-protect
      (dolist (l lines)
        (prinf l)
        (if (equal (read-line) l)
          (progn (incf cleared)
                 (format t "=== Correct! ===~%" cleared))
          (prinf "=== Failed! ==="))))
    (prinf cleared)))
