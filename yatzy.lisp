;;; Gosu Yatzy
;;; Written by Johannes Langøy, December 2013
(load "source/utils")

(defun numbers (n dice)
  (and (<= 1 (count n dice))
       (* n (count n dice))))

(defun one-pair (dice)
  (let ((s nil))
    (dolist (n '(6 5 4 3 2 1))
      (when (<= 2 (count n dice))
        (setf s (reduce #'+ (subseq (remove-if (lambda (x)
                                                 (not (= x n))) dice)
                                    0 2)))
        (return-from nil)))
    s))

(defun two-pairs (dice)
  (let* ((l (mapcar (lambda (n) (list n (count n dice)))
                    '(1 2 3 4 5 6))))
    (when (= 2 (count 2 (mapcar #'cadr l)))
      (reduce #'+
              (remove-if #'null (mapcar (lambda (x)
                                          (if (= (cadr x) 2)
                                            (* (car x) (cadr x))))
                                        l))))))

(defun three-of-a-kind (dice)
  (let ((s nil))
    (dolist (n '(6 5 4 3 2 1))
      (when (<= 3 (count n dice))
        (setf s (reduce #'+ (subseq (remove-if (lambda (x)
                                                 (not (= x n))) dice)
                                    0 3)))
        (return-from nil)))
    s))

(defun four-of-a-kind (dice)
  (let ((s nil))
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

(defun check-all (dice)
  `((ones "Ones" ,(numbers 1 dice))
    (twos "Twos" ,(numbers 2 dice))
    (threes "Threes" ,(numbers 3 dice))
    (fours "Fours" ,(numbers 4 dice))
    (fives "Fives" ,(numbers 5 dice))
    (sixes "Sixes" ,(numbers 6 dice))
    (one-pair "One pair" ,(one-pair dice))
    (two-pairs "Two pairs" ,(two-pairs dice))
    (three-of-a-kind "Three of a kind" ,(three-of-a-kind dice))
    (four-of-a-kind "Four of a kind" ,(four-of-a-kind dice))
    (small-straight "Small straight" ,(small-straight dice))
    (large-straight "Large straight" ,(large-straight dice))
    (house "House" ,(house dice))
    (chance "Chance" ,(chance dice))
    (yatzy "Yatzy" ,(yatzy dice))))

(defun remove-unfulfilled (checks)
  (remove-if (lambda (x) (null (third x)))
             checks))

;; Imperative part
(defun roll-dice (amount)
  (loop repeat amount collect (1+ (random 6))))

(defun roll-unkept-dice (dice)
  (append dice (roll-dice (- 5 (length dice)))))

(defun one-turn ()
  (let ((dice nil) (n 1))
    (loop named turn repeat 3 do
          (setf dice (roll-unkept-dice dice))
          (format t "Roll ~a: " n)
          (princ dice)
          (fresh-line)
          (unless (= 3 n)
            (princ "Enter which dice to keep: ")
            (setf dice (read))
            (incf n))
          (if (= 5 (length dice))
            (return-from turn)))
    dice))

(defun game-loop ()
  (defvar *score* 0)
  (defvar *checked-boxes* nil)
  (loop named game do
        ;; Interactively roll the dice.
        (setf *dice* (one-turn))
        ;; Get a list of choices for what do do with these dice.
        (setf *choices* (remove-unfulfilled (check-all *dice*)))
        ;; Delete the choices that are already used.
        (dolist (x *checked-boxes*)
          (delete-if (lambda (y) (eq (first y) (first x)))
                     *choices*))
        ;; Print the choices.
        (dolist (x *choices*)
          (format t "~a: ~a~%" (second x) (third x)))
        ;; Ask which choice to use.
        (loop named check do
              (format t "Check which box? ")
              (setf *selection* (read))
              (if (assoc *selection* *choices*)
                (return-from check)
                (if (y-or-n-p "Cross out ~a? " *selection*)
                  (return-from check))))
        ;; Extract the points, if any.
        (setf *points* (third (assoc *selection* *choices*)))
        ;; Update the list of checked boxes.
        (push (list *selection* *points*) *checked-boxes*)
        ;; Update the score.
        (and *points* (incf *score* *points*))
        ;; Check if it's time to end the game.
        (when (= (length *checked-boxes*) 15)
          (format t "Game over. Final score: ~a~%" *score*)
          (return-from game))))
