(load "utils")

(defvar *suits* '((clubs  . #\U2663) (diamonds . #\U2666)
                  (hearts . #\U2665) (spades   . #\U2660)))
(defvar *values* '(2 3 4 5 6 7 8 9 10 J Q K A))
(defvar *deck* (loop for s in *suits* append
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

(defvar *hierarchy*
  '(royal-flush
    straight-flush
    four-alike
    full-house
    flush
    straight
    three-alike
    two-pairs
    one-pair
    high-card))

(defun sort-hand-val (hand)
  (sort hand #'> :key (lambda (x) (position (card-value x) *values*))))

(defun sort-hand-suit (hand)
  (sort hand #'> :key (lambda (x) (position (card-suit x)
                                            (mapcar #'car *suits*)))))

;;; This function should really do different checks depending on ranks;
;;; i.e. if both hands are one-pair, it should check which pair has the
;;; highest value (or suit), rather than which hand has the highest card
;;; overall. It seems this would take a good deal of work to implement,
;;; so I am lazily leaving this as it is for now.
(defun hand> (hand1 hand2)
  ;; Compare ranks
  (let ((x (position (check-hand hand1) *hierarchy*))
        (y (position (check-hand hand2) *hierarchy*)))
    (if (not (= x y))
        (return-from hand> (if (< x y)
                               t
                               nil))))
  ;; Compare values
  (loop for x in (mapcar #'card-value (sort-hand-val hand1))
        and y in (mapcar #'card-value (sort-hand-val hand2)) do
        (if (not (eq x y))
            (return-from hand> (if (after x y *values*)
                                   t
                                   nil))))
  ;; Compare suits
  (loop for x in (mapcar #'card-suit (sort-hand-suit hand1))
        and y in (mapcar #'card-suit (sort-hand-suit hand2)) do
        (if (not (eq x y))
            (return-from hand> (if (after x y (mapcar #'car *suits*))
                                   t
                                   nil))))
  (error "The hands are identical or something went wrong.")))

(defun check-hand (hand)
  (dolist (x *hierarchy*)
    (if (funcall x hand)
        (return-from nil x))))

(defun royal-flush (hand)
  (and (flush hand)
       (let ((rhand (mapcar #'card-value hand)))
         (and (not (member nil (mapcar (lambda (x) (member x rhand))
                                       (nthcdr 8 *values*))))
              hand))))

(defun straight-flush (hand)
  (and (straight hand) (flush hand) hand))

(defun full-house (hand)
  (let* ((rhand (mapcar #'card-value hand))
         (l (mapcar (lambda (v) (count v rhand)) *values*)))
    (and (member 2 l) (member 3 l) hand)))

(defun flush (hand)
  (let ((rhand (mapcar #'card-suit hand)))
    (and (not (member nil (mapcar (lambda (x) (eq x (car rhand)))
                                  rhand)))
         hand)))

(defun straight (hand)
  (let* ((rhand (mapcar #'card-value hand))
         (checks (mapcar (lambda (x) (if (member x rhand) t)) *values*))
         (seq (nthcdr (position t checks) checks)))
    (and (>= (length seq) 5)
         (and (not (member nil (subseq seq 0 5)))
              hand))))

(macrolet ((sames (name n)
             `(defun ,name (hand)
                (let ((rhand (mapcar #'card-value hand)))
                  (dolist (v *values*)
                    (if (>= (count v rhand) ,n)
                        (return-from nil hand)))))))
  (sames one-pair 2)
  (sames three-alike 3)
  (sames four-alike 4))

(defun two-pairs (hand)
  (let* ((rhand (mapcar #'card-value hand))
         (l (remove-if (lambda (v) (< (count v rhand) 2)) *values*)))
    (if (= 2 (length l))
        hand)))

(defun high-card (hand)
  (let ((rhand (mapcar #'card-value hand)))
    (dolist (v (reverse *values*))
      (if (member v rhand)
          (return-from nil v)))))

(defmacro count-attempts (&rest body)
  `(loop for x from 1 do
         (let ((ret ,@body))
           (if ret
               (return-from nil (values x ret))))))

(defun average (lst)
  (/ (apply #'+ lst) (length lst)))
