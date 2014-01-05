(load "utils.lisp")

(defvar *values* '(A 2 3 4 5 6 7 8 9 10 J Q K))
(defvar *suits* '((spades   . #\U2660) (hearts . #\U2665)
                  (diamonds . #\U2666) (clubs  . #\U2663)))

(defun make-deck ()
  (let (deck)
    (dolist (s *suits*)
      (setf deck (append deck
                         (mapcar (lambda (c) (cons (car s) c))
                                 *values*))))
    deck))

(defun card-suit        (card) (car card))
(defun card-suit-symbol (card) (cdr (assoc (car card) *suits*)))
(defun card-value       (card) (cdr card))

(defun format-cards (cards)
  (dolist (c cards)
    (format t "~a~a " (card-suit-symbol c) (card-value c))))

(defun make-hand (deck)
  (subseq (shuffle deck) 0 5))

(defun hand-flush (hand)
  (let ((lst (mapcar #'card-suit hand)))
    (not (member nil (mapcar (lambda (x) (eq x (car lst)))
                             lst)))))

(defmacro test-func (&rest body)
  `(loop with x = 0 do
         (if ,@body
           (return-from nil x)
           (incf x))))
