;; Functional part
(defun get-author (book) (first book))
(defun get-name (book) (second book))
(defun get-year (book) (third book))

(defun filter-author (author books)
  (remove-if (lambda (x) (not (equal author (get-author x))))
             books))

(defun filter-name (name books)
  (remove-if (lambda (x) (not (equal name (get-name x))))
             books))

(defun filter-year (year books)
  (remove-if (lambda (x) (not (equal year (get-year x))))
             books))

(defun filter-before (year books)
  (remove-if (lambda (x) (< year (get-year x)))
             books))

(defun filter-after (year books)
  (remove-if (lambda (x) (> year (get-year x)))
             books))

(defun filter-range (after before books)
  (filter-after after (filter-before before books)))

(defun sort-year (books)
  (let ((temp (copy-list books)))
    (sort temp (lambda (x y) (< (get-year x) (get-year y))))))

;; Imperative part
(defun format-books (books)
  (dolist (b books)
    (format t "~a: ~a (~a)~%"
            (get-author b) (get-name b) (get-year b))))

(defun read-books ()
  (with-open-file (f "book-list")
    (setf *books* (read f))))

(defun write-books ()
  (with-open-file (f "book-list" :direction :output)
    (print *books* f)))

(read-books)
