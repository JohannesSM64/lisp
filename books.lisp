(macrolet ((mac (name func)
             `(defun ,name (book)
                (,func book))))
  (mac author-of first)
  (mac name-of second)
  (mac year-of third))

(macrolet ((mac (name &rest body)
             `(defun ,name (x &optional (books *books*))
                (remove-if (lambda (y) ,@body)
                           books))))
  (mac flt-author (not (equal x (author-of y))))
  (mac flt-name (not (equal x (name-of y))))
  (mac flt-year (not (equal x (year-of y))))
  (mac flt-before (< x (year-of y)))
  (mac flt-after (> x (year-of y))))

(defun range (x y books)
  (after x (before y books)))

(macrolet ((mac (name fn key)
             `(defun ,name (books)
                (let ((temp (copy-list books)))
                  (sort temp ,fn :key ,key)))))
  (mac sort-year #'< #'year-of)
  (mac sort-author #'string-lessp #'author-of)
  (mac sort-name #'string-lessp #'name-of))

(defun fb (books) ; format-books
  (dolist (b books)
    (format t "~a: ~a (~a)~%"
            (author-of b) (name-of b) (year-of b))))

(defun read-books (file)
  (with-open-file (f file)
    (setf *books* (read f))))

(defun write-books (file)
  (with-open-file (f file :direction :output)
    (print *books* f)))

(read-books "book-list")
