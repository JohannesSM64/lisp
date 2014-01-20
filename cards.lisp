(load "utils")

(defvar *values* '(2 3 4 5 6 7 8 9 10 J Q K A))
(defvar *suits* '((clubs  . #\U2663) (diamonds . #\U2666)
                  (hearts . #\U2665) (spades   . #\U2660)))

(defun make-deck ()
  (loop for s in *suits* append
        (loop for v in *values* collect
              (cons (car s) v))))

(defun make-hand (deck)
  (loop repeat 5 collect (pop deck)))

(defun card-suit        (card) (car card))
(defun card-suit-symbol (card) (cdr (assoc (car card) *suits*)))
(defun card-value       (card) (cdr card))

(defun format-cards (cards)
  (dolist (c cards)
    (format t "~a~a " (card-suit-symbol c) (card-value c))))

(defun check-flush (hand)
  (let ((hand (mapcar #'card-suit hand)))
    (not (member nil (mapcar (lambda (x) (eq x (car hand)))
                             hand)))))

(macrolet ((sames (name n)
             `(defun ,name (hand)
                (let ((hand (mapcar #'card-value hand)))
                  (dolist (v *values*)
                    (if (>= (count v hand) ,n)
                      (return-from nil t)))))))
  (sames check-pair 2)
  (sames check-three-alike 3)
  (sames check-four-alike 4))

(defun check-straight (hand)
  (let ((hand (mapcar #'card-value hand)))
    (loop for n to 8 do
          (or (member nil (mapcar (lambda (x) (find x hand))
                                  (subseq *values* n (+ n 5))))
              (return-from nil t)))))

(defun check-straight-flush (hand)
  (and (check-straight hand) (check-flush hand)))

(defun check-royal-straight-flush (hand)
  (and (check-flush hand)
       (let ((hand (mapcar #'card-value hand)))
         (not (member nil (mapcar (lambda (x) (member x hand))
                                  (nthcdr 8 *values*)))))))

(defun check-two-pairs (hand)
  (let* ((hand (mapcar #'card-value hand))
         (l (remove-if (lambda (v) (< (count v hand) 2)) *values*)))
      (= 2 (length l))))

(defun check-full-house (hand)
  (let* ((hand (mapcar #'card-value hand))
         (l (mapcar (lambda (v) (count v hand)) *values*)))
    (and (member 2 l) (member 3 l))))

(defun test-check (fn)
  (funcall fn (make-hand (shuffle (make-deck)))))

(defmacro count-attempts (&rest body)
  `(loop with x = 1 do
         (if ,@body
           (return-from nil x)
           (incf x))))

(defun average (lst)
  (/ (apply #'+ lst) (length lst)))
