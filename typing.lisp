(load "utils.lisp")

(defvar *lines*
  '("This is not a palindrome"
    "Wine was mixed with water in Ancient Greece"))

(defun test ()
  (let ((cleared 0))
    (unwind-protect
      (loop (let ((line (randnth *lines*)))
              (prinf line)
              (if (equal (read-line) line)
                (progn (incf cleared)
                       (format t "=== Correct! ===~%" cleared))
                (prinf "=== Failed! ==="))))
      (prinf cleared))))
