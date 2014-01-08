(load "utils.lisp")

(defvar *values* '(A 2 3 4 5 6 7 8 9 10 J Q K))
(defvar *suits* '((clubs  . #\U2663) (diamonds . #\U2666)
                  (hearts . #\U2665) (spades   . #\U2660)))

(defun make-deck ()
  (loop for s in *suits* append
        (loop for v in *values* collect
              (cons (car s) v))))

(defun card-suit        (card) (car card))
(defun card-suit-symbol (card) (cdr (assoc (car card) *suits*)))
(defun card-value       (card) (cdr card))

(defun format-cards (cards)
  (dolist (c cards)
    (format t "~a~a " (card-suit-symbol c) (card-value c))))

(defun make-hand (deck)
  (subseq (shuffle deck) 0 5))

(defun check-flush (hand)
  (let ((lst (mapcar #'card-suit hand)))
    (not (member nil (mapcar (lambda (x) (eq x (car lst)))
                             lst)))))

(defun check-pair (hand)
  (let ((lst (mapcar #'card-value hand)))
    (dolist (v *values*)
      (if (>= (count v lst) 2)
        (return-from nil t)))))

(defmacro test-func (&rest body)
  `(loop with x = 1 do
         (if ,@body
           (return-from nil x)
           (incf x))))

(defun average (lst)
  (/ (apply #'+ lst) (length lst)))
