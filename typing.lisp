(load "utils.lisp")

(defvar *lines*
  '("This is not a palindrome"
    "Wine was mixed with water in Ancient Greece"))

(defun test ()
  (loop (let ((line (randnth *lines*)))
          (prinf line)
          (prinf (if (equal (read-line) line)
                   "=== Correct! ==="
                   "=== Failed ===")))))
