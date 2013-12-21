;; Functional part
(defmacro property (name func)
  `(defun ,name (book)
     (,func book)))

(property author-of first)
(property name-of second)
(property year-of third)

(defmacro filter (name &rest body)
  `(defun ,name (x books)
     (remove-if (lambda (y) ,@body)
                books)))

(filter author (not (equal x (author-of y))))
(filter name (not (equal x (name-of y))))
(filter year (not (equal x (year-of y))))
(filter before (< x (year-of y)))
(filter after (> x (year-of y)))

(defun range (x y books)
  (after x (before y books)))

(defun sort-year (books)
  (let ((temp (copy-list books)))
    (sort temp (lambda (x y) (< (year-of x) (year-of y))))))

;; Imperative part
(defun format-books (books)
  (dolist (b books)
    (format t "~a: ~a (~a)~%"
            (author-of b) (name-of b) (year-of b))))

(defun read-books ()
  (with-open-file (f "book-list")
    (setf *books* (read f))))

(defun write-books ()
  (with-open-file (f "book-list" :direction :output)
    (print *books* f)))

(read-books)
