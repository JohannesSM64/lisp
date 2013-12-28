(load "utils.lisp")

(defvar *cards* '(A 2 3 4 5 6 7 8 9 10 J Q K))
(defvar *suits* '((spades . #\U2660) (hearts . #\U2665)
                  (diamonds . #\U2666) (clubs . #\U2663)))

(defun make-deck ()
  (let (deck)
    (dolist (s *suits*)
      (setf deck (append deck
                         (mapcar (lambda (c) (cons s c))
                                 *cards*))))
    deck))

(defun format-deck (deck)
  (dolist (card deck)
    (format t "~a~a " (cdar card) (cdr card))))

(format-deck (subseq (shuffle (make-deck)) 0 5))
