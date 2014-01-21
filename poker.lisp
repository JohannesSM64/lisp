(load "utils")

(defvar *values* '(2 3 4 5 6 7 8 9 10 J Q K A))
(defvar *suits* '((clubs  . #\U2663) (diamonds . #\U2666)
                  (hearts . #\U2665) (spades   . #\U2660)))
(defvar *deck*
  (let ((deck (make-array '(52)))
        (index 0))
    (dolist (s *suits*)
      (dolist (v *values*)
        (setf (aref deck index) (cons (car s) v)
              index (1+ index))))
    deck))

(defun make-hand ()
  (let (hand nums (i 0))
    (tagbody foo
      (tagbody bar
        (let ((n (rand 52)))
          (if (member n nums)
            (go bar)
            (progn (push n nums)
                   (push (aref *deck* n) hand)))))
      (if (< i 4)
        (progn (incf i)
               (go foo))))
    hand))

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

(defun hand> (hand1 hand2)
  (< (position (check-hand hand1) *hierarchy*)
     (position (check-hand hand2) *hierarchy*)))

(defun check-hand (hand)
  (dolist (x *hierarchy*)
    (if (funcall x hand)
      (return-from nil x))))

(defun royal-flush (hand)
  (and (flush hand)
       (let ((hand (mapcar #'card-value hand)))
         (not (member nil (mapcar (lambda (x) (member x hand))
                                  (nthcdr 8 *values*)))))))

(defun straight-flush (hand)
  (and (straight hand) (flush hand)))

(defun full-house (hand)
  (let* ((hand (mapcar #'card-value hand))
         (l (mapcar (lambda (v) (count v hand)) *values*)))
    (and (member 2 l) (member 3 l))))

(defun flush (hand)
  (let ((hand (mapcar #'card-suit hand)))
    (not (member nil (mapcar (lambda (x) (eq x (car hand)))
                             hand)))))

(defun straight (hand)
  (let* ((hand (mapcar #'card-value hand))
         (checks (mapcar (lambda (x) (if (member x hand) t)) *values*))
         (seq (nthcdr (position t checks) checks)))
    (and (>= (length seq) 5)
         (not (member nil (subseq seq 0 5))))))

(macrolet ((sames (name n)
             `(defun ,name (hand)
                (let ((hand (mapcar #'card-value hand)))
                  (dolist (v *values*)
                    (if (>= (count v hand) ,n)
                      (return-from nil t)))))))
  (sames one-pair 2)
  (sames three-alike 3)
  (sames four-alike 4))

(defun two-pairs (hand)
  (let* ((hand (mapcar #'card-value hand))
         (l (remove-if (lambda (v) (< (count v hand) 2)) *values*)))
      (= 2 (length l))))

(defun high-card (hand)
  (let ((hand (mapcar #'card-value hand)))
    (dolist (v (reverse *values*))
      (if (member v hand)
        (return-from nil v)))))

(defmacro count-attempts (&rest body)
  `(loop with x = 1 do
         (if ,@body
           (return-from nil x)
           (incf x))))

(defun average (lst)
  (/ (apply #'+ lst) (length lst)))
