(load "utils")

(defvar *values* '(2 3 4 5 6 7 8 9 10 J Q K A))
(defvar *suits* '((clubs  . #\U2663) (diamonds . #\U2666)
                  (hearts . #\U2665) (spades   . #\U2660)))
(defvar *deck*
  (let ((deck (make-array '(52)))
        (index 0))
    (dolist (s *suits*)
      (dolist (v *values*)
        (setf (aref deck index) (cons (car s) v))
        (incf index)))
    deck))

(defun make-hand ()
  (let (hand nums n)
    (loop repeat 5 do
          (while (member n nums)
            (setq n (random 52)))
          (push n nums)
          (push (aref *deck* n) hand))
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

(defun sort-hand (hand)
  (sort hand #'> :key (lambda (x) (position (card-value x) *values*))))

(defun hand> (hand1 hand2)
  (if (< (position (check-hand hand1) *hierarchy*)
         (position (check-hand hand2) *hierarchy*))
      t
      (loop for x in (sort-hand hand1) and y in (sort-hand hand2) do
            (if (> (position (cdr x) *values*)
                   (position (cdr y) *values*))
                (return-from nil t))
                (if (not (= (position (cdr x) *values*)
                            (position (cdr y) *values*)))
                    (return-from nil nil)))))

(defun check-hand (hand)
  (dolist (x *hierarchy*)
    (if (funcall x hand)
        (return-from nil (values x hand)))))

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
