;;; Gosu Yatzy
;;; Written by Johannes Langøy, December 2013 - January 2014

(defmacro numfunc (name n)
  `(defun ,name (dice)
     (if (<= 1 (count ,n dice))
       (* ,n (count ,n dice)))))

(numfunc ones  1) (numfunc twos  2) (numfunc threes 3)
(numfunc fours 4) (numfunc fives 5) (numfunc sixes  6)

(defun one-pair (dice)
  (loop for n from 6 downto 1 do
    (when (<= 2 (count n dice))
      (return-from nil (* n 2)))))

(defun two-pairs (dice)
  (let ((l (remove-if (lambda (n) (not (>= (count n dice) 2)))
                      '(1 2 3 4 5 6))))
    (if (= 2 (length l))
      (apply #'+ (mapcar (lambda (n) (* 2 n)) l)))))

(defun three-of-a-kind (dice)
  (loop for n from 6 downto 1 do
    (when (<= 3 (count n dice))
      (return-from nil (* n 3)))))

(defun four-of-a-kind (dice)
  (loop for n from 6 downto 1 do
    (when (<= 4 (count n dice))
      (return-from nil (* n 4)))))

(defun small-straight (dice)
  (unless (member nil (loop for n from 1 to 5 collect (find n dice)))
    15))

(defun large-straight (dice)
  (unless (member nil (loop for n from 2 to 6 collect (find n dice)))
    20))

(defun house (dice)
  (let ((r (loop for n from 1 to 6 collect (count n dice))))
    (if (and (find 2 r) (find 3 r))
      (+ (* 2 (1+ (position 2 r)))
         (* 3 (1+ (position 3 r)))))))

(defun chance (dice)
  (apply #'+ dice))

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

(defun goal-string (goal)
  (cadr (assoc goal *goals*)))

(defun all-goal-symbols ()
  (mapcar #'car *goals*))

(defun check-bonus (boxes)
  (if (>= (apply #'+ (mapcar (lambda (g) (cdr (assoc g boxes)))
                             '(ones twos threes fours fives sixes)))
          63)
    50))

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
                                    (loop for x across d collect x)))
                    (dolist (x d)
                      (when (not (member x dice))
                        (setf i t)))
                    (dolist (n '(1 2 3 4 5 6))
                      (when (> (count n d) (count n dice))
                        (setf i t)))
                    (if i
                      (format t "Invalid input.~%")
                      (progn (setf dice d)
                             (return-from roll)))))
            (incf n))
          (if (= 5 (length dice))
            (return-from turn)))
    dice))

(defun game-loop ()
  (defvar boxes nil)
  (format t "Welcome to Gosu Yatzy.~%")
  (loop named game do
        ;; Initialize random.
        (setf *random-state* (make-random-state t))
        ;; Interactively roll the dice.
        (setf dice (one-turn))
        ;; Initialize choices.
        (setf choices *goals*)
        ;; Remove the choices that are already used.
        (dolist (x boxes)
          (setf choices (remove-if (lambda (y) (eq (car y) (car x)))
                                   *goals*)))
        ;; Get our choices.
        (setf choices (mapcar (lambda (g)
                                (cons (car g) (funcall (car g) dice)))
                              choices))
        ;; Separate fulfilled and unfulfilled goals.
        (setf cross-choices (mapcar #'car
                                    (remove-if (lambda (x) (cdr x))
                                               choices)))
        (setf choices (remove-if (lambda (x) (null (cdr x)))
                                 choices))
        ;; Print the choices.
        (loop for n below (length choices) do
              (format t "~a) ~a: ~a~%"
                      (1+ n)
                      (goal-string (car (nth n choices)))
                      (cdr (nth n choices))))
        (loop for n below (length cross-choices) do
              (format t "~a) __~a__~%"
                      (code-char (+ 65 n))
                      (goal-string (nth n cross-choices))))
        ;; Get a selection.
        (setf selection nil)
        (loop named check do
              (princ "Enter your selection: ")
              (let ((input (read-line)))
                (if (not (= (length input) 1))
                  (format t "Invalid input.~%")
                  (let ((int (parse-integer input :junk-allowed t)))
                    (if int
                      (setf selection (nth (1- int) choices))
                      (let ((cc (- (char-code (char-upcase
                                                (char input 0)))
                                   65)))
                        (if (>= cc 0)
                          (let ((chr (nth cc cross-choices)))
                            (if chr
                              (setf selection (list chr)))))))
                    (if selection
                      (return-from check)
                      (format t "Invalid input.~%"))))))
        ;; Update the list of checked boxes.
        (push selection boxes)
        ;; Check if it's time to end the game.
        (when (= (length boxes) (length (all-goal-symbols)))
          (format t "Game over.~%")
          (sort boxes (lambda (x y) (null (cdr y))))
          (dolist (x (mapcar (lambda (x) (assoc x boxes))
                             (all-goal-symbols)))
            (format t "~a: ~a~%"
                    (cadr (assoc (car x) *goals*))
                    (or (cdr x) "--")))
          (setf score
                (apply #'+ (remove-if #'null (mapcar #'cdr boxes))))
          (setf bonus (check-bonus boxes))
          (if bonus (incf score bonus))
          (format t "Bonus: ~a~%" bonus)
          (format t "Score: ~a~%" score)
          (return-from game))))
