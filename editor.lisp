(defvar *filename* nil)
(defvar *buffer* "")

(defun read-file (name)
  (setf *buffer*
        (if (probe-file name)
            (with-open-file (f name)
              (read f))
            ""))
  (setf *filename* name))

(defun write-file ()
  (with-open-file (f *filename* :direction :output)
    (print *buffer* f)))

(defun alias (x y)
  (setf (symbol-function x) y))

(defun cursor-at ()
  (screen:window-cursor-position screen:*window*))
(defun setcursor (x y)
  (screen:set-window-cursor-position screen:*window* x y))
(alias 'cchar #'system::input-character-char)
(alias 'ckey #'system::input-character-key)

(screen:with-window
  (with-keyboard
    (loop (multiple-value-bind (line col) (cursor-at)
            (case (cchar (read-char *keyboard-input*))
              (#\h (setcursor line (1- col)))
              (#\j (setcursor (1+ line) col))
              (#\k (setcursor (1- line) col))
              (#\l (setcursor line (1+ col)))
              (#\0 (setcursor line 0))
              (#\g (setcursor 0 0))
              (#\i (loop (let ((c (cchar (read-char *keyboard-input*))))
                           (if (equal c #\Escape)
                               (return-from nil)
                               (princ c screen:*window*)))))
              (#\q (return-from nil)))))))
