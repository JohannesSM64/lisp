;;; Gosu Yatzy
;;; Written by Johannes Langøy, December 2013

;; Bugs:
;; - No error handling for "Check which box?" at :188.

(defun ones   (dice) (and (<= 1 (count 1 dice)) (* 1 (count 1 dice))))
(defun twos   (dice) (and (<= 1 (count 2 dice)) (* 2 (count 2 dice))))
(defun threes (dice) (and (<= 1 (count 3 dice)) (* 3 (count 3 dice))))
(defun fours  (dice) (and (<= 1 (count 4 dice)) (* 4 (count 4 dice))))
(defun fives  (dice) (and (<= 1 (count 5 dice)) (* 5 (count 5 dice))))
(defun sixes  (dice) (and (<= 1 (count 6 dice)) (* 6 (count 6 dice))))

(defun one-pair (dice)
  (let (s)
    (dolist (n '(6 5 4 3 2 1))
      (when (<= 2 (count n dice))
        (setf s (reduce #'+ (subseq (remove-if (lambda (x)
                                                 (not (= x n))) dice)
                                    0 2)))
        (return-from nil)))
    s))

(defun two-pairs (dice)
  (let ((l (mapcar (lambda (n) (list n (count n dice)))
                   '(1 2 3 4 5 6))))
    (when (= 2 (count 2 (mapcar #'cadr l)))
      (reduce #'+
              (remove-if #'null (mapcar (lambda (x)
                                          (if (= (cadr x) 2)
                                            (* (car x) (cadr x))))
                                        l))))))

(defun three-of-a-kind (dice)
  (let (s)
    (dolist (n '(6 5 4 3 2 1))
      (when (<= 3 (count n dice))
        (setf s (reduce #'+ (subseq (remove-if (lambda (x)
                                                 (not (= x n))) dice)
                                    0 3)))
        (return-from nil)))
    s))

(defun four-of-a-kind (dice)
  (let (s)
    (dolist (n '(6 5 4 3 2 1))
      (when (<= 4 (count n dice))
        (setf s (reduce #'+ (subseq (remove-if (lambda (x)
                                                 (not (= x n))) dice)
                                    0 4)))
        (return-from nil)))
    s))

(defun small-straight (dice)
  (unless (member nil (mapcar (lambda (n) (find n dice))
                              '(1 2 3 4 5)))
    15))

(defun large-straight (dice)
  (unless (member nil (mapcar (lambda (n) (find n dice))
                              '(2 3 4 5 6)))
    20))

(defun house (dice)
  (let* ((l (mapcar (lambda (n) (list n (count n dice)))
                    '(1 2 3 4 5 6)))
         (lc (mapcar #'cadr l)))
    (when (and (find 2 lc) (find 3 lc))
      (reduce #'+
              (remove-if #'null (mapcar (lambda (x)
                                          (if (member (cadr x) '(2 3))
                                            (* (car x) (cadr x))))
                                        l))))))

(defun chance (dice)
  (reduce #'+ dice))

(defun yatzy (dice)
  (when (apply #'= dice)
    50))

(defvar *goals*
  '((ones "Ones")
    (twos "Twos")
    (threes "Threes")
    (fours "Fours")
    (fives "Fives")
    (sixes "Sixes")
    (one-pair "One pair")
    (two-pairs "Two pairs")
    (three-of-a-kind "Three of a kind")
    (four-of-a-kind "Four of a kind")
    (small-straight "Small straight")
    (large-straight "Large straight")
    (house "House")
    (chance "Chance")
    (yatzy "Yatzy")))

(defun check-all (dice)
  (mapcar (lambda (g) (cons (car g) (funcall (car g) dice)))
          *goals*))

(defun goal-string (goal)
  (cadr (assoc goal *goals*)))

(defun all-goal-symbols ()
  (mapcar #'car *goals*))

(defun remove-unfulfilled (checks)
  (remove-if (lambda (x) (null (cdr x)))
             checks))

(defun remove-fulfilled (checks)
  (remove-if (lambda (x) (not (null (cdr x))))
             checks))

;; Imperative part
(defun roll-dice (amount)
  (loop repeat amount collect (1+ (random 6))))

(defun roll-unkept-dice (dice)
  (append dice (roll-dice (- 5 (length dice)))))

(defun one-turn ()
  (let ((n 1) dice)
    (loop named turn repeat 3 do
          (setf dice (roll-unkept-dice dice))
          (format t "Roll ~a: " n)
          (princ dice)
          (fresh-line)
          (unless (= 3 n)
            (loop named roll do
                  (let (i d)
                    (princ "Enter which dice to keep: ")
                    (setf d (read-line))
                    (when (equal d "all")
                      (return-from roll))
                    (setf d (mapcar #'digit-char-p
                                    (loop for x across d
                                          collect x)))
                    (dolist (x d)
                      (when (not (member x dice))
                        (setf i t)))
                    (dolist (n '(1 2 3 4 5 6))
                      (when (> (count n d) (count n dice))
                        (setf i t)))
                    (cond (i (format t "Invalid input.~%"))
                          (t (setf dice d)
                             (return-from roll)))))
            (incf n))
          (if (= 5 (length dice))
            (return-from turn)))
    dice))

(defun game-loop ()
  (defvar *checked-boxes* nil)
  (format t "Welcome to Gosu Yatzy.~%")
  (loop named game do
        ;; Interactively roll the dice.
        (setf *dice* (one-turn))
        ;; UI improvement (I can probably think of something better..)
        (sleep 1)
        ;; Get a list of choices for what do do with these dice.
        (setf *choices* (check-all *dice*))
        ;; Delete the choices that are already used.
        (dolist (x *checked-boxes*)
          (setf *choices* (remove-if (lambda (y) (eq (car y) (car x)))
                                     *choices*)))
        ;; Sort the choices for easier reading.
        (sort *choices* (lambda (x y) (null (cdr y))))
        ;; Print the choices.
        (loop for n below (length *choices*) do
              (princ (1+ n))
              (princ ") ")
              (if (cdr (nth (1+ n) *choices*))
                (progn (princ (goal-string (car (nth (1+ n)
                                                     *choices*))))
                       (princ ": ")
                       (princ (cdr (nth (1+ n) *choices*))))
                (progn (princ "__")
                       (princ (goal-string (car (nth (1+ n)
                                                     *choices*))))
                       (princ "__")))
              (fresh-line))
        ;; Ask which choice to use, confirming that it's valid.
        (loop named check do
              (format t "Check which box? ")
              (setf *selection* (car (nth (parse-integer (read-line))
                                     *choices*)))
              (if (assoc *selection* (remove-unfulfilled *choices*))
                (return-from check)
                ;; else
                (if (and (member *selection* (all-goal-symbols))
                         (y-or-n-p "Cross out ~a? " *selection*))
                  (return-from check))))
        ;; Update the list of checked boxes.
        (push (cons *selection* (cdr (assoc *selection* *choices*)))
              *checked-boxes*)
        ;; Check if it's time to end the game.
        (when (= (length *checked-boxes*) (length (all-goal-symbols)))
          (format t "Game over.~%")
          (dolist (x *checked-boxes*)
            (format t "~a: ~a~%"
                    (cadr (assoc (car x) *goals*))
                    (cdr x)))
          (format t "Score: ~a~%" (reduce #'+ (remove-if #'null (mapcar #'cdr *checked-boxes*))))
          (return-from game))))

(game-loop)
