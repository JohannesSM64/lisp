;;; Gosu Yatzy
;;; Written by Johannes Langøy, December 2013 - January 2014

(load "utils")

(labels ((numfunc (n)
           (lambda (dice)
             (if (<= 1 (count n dice))
               (* n (count n dice))))))
  (defvar *goals*
    `(("Ones" ,(numfunc 1))
      ("Twos" ,(numfunc 2))
      ("Threes" ,(numfunc 3))
      ("Fours" ,(numfunc 4))
      ("Fives" ,(numfunc 5))
      ("Sixes" ,(numfunc 6))
      ("One pair"
       ,(lambda (dice)
         (loop for n from 6 downto 1 do
               (when (<= 2 (count n dice))
                 (return-from nil (* n 2))))))
      ("Two pairs"
       ,(lambda (dice)
         (let ((l (remove-if (lambda (n) (< (count n dice) 2))
                             '(1 2 3 4 5 6))))
           (if (= 2 (length l))
             (apply #'+ (mapcar (lambda (n) (* 2 n)) l))))))
      ("Three of a kind"
       ,(lambda (dice)
         (loop for n from 6 downto 1 do
               (when (<= 3 (count n dice))
                 (return-from nil (* n 3))))))
      ("Four of a kind"
       ,(lambda (dice)
         (loop for n from 6 downto 1 do
               (when (<= 4 (count n dice))
                 (return-from nil (* n 4))))))
      ("Small straight"
       ,(lambda (dice)
         (unless (member nil (loop for n from 1 to 5
                                   collect (find n dice)))
           15)))
      ("Large straight"
       ,(lambda (dice)
         (unless (member nil (loop for n from 2 to 6
                                   collect (find n dice)))
           20)))
      ("House"
       ,(lambda (dice)
         (let ((l (loop for n from 1 to 6 collect (count n dice))))
           (let ((two   (position 2 l))
                 (three (position 3 l)))
             (and two three
                  (+ (* 2 (1+ two))
                     (* 3 (1+ three))))))))
      ("Chance"
       ,(lambda (dice)
         (apply #'+ dice)))
      ("Yatzy"
       ,(lambda (dice)
         (when (apply #'= dice)
           50))))))

;; Imperative part
(defun game-loop ()
  (let (boxes choices cross-choices selection (score 0) (bonus 0))
    (format t "Welcome to Gosu Yatzy.~%")
    (loop named game do
          ;; Initialize random.
          (setq *random-state* (make-random-state t))
          ;; Interactively roll the dice.
          (setq dice nil)
          (loop named turn repeat 3 for n from 1 do
                ;; Roll the needed amount of dice.
                (setq dice (append dice (loop repeat (- 5 (length dice))
                                              collect (1+ (random 6)))))
                (format t "Roll ~a: ~a~%" n dice)
                (unless (= n 3)
                  (loop named check do
                        (princ "Enter which dice to keep: ")
                        (let ((input (read-line)))
                          ;; If no dice are kept, just move on.
                          (if (string= "" input)
                            (progn (setq dice nil)
                                   (return-from check)))
                          ;; a means keep all dice.
                          (if (equal input "a")
                            (return-from turn dice))
                          (let ((inv nil)
                                ;; Convert "123" into (1 2 3).
                                (l (loop for x across input collect
                                         (digit-char-p x))))
                            ;; Check if the player tries to cheat.
                            (loop for n from 1 to 6 do
                                  (if (> (count n l) (count n dice))
                                    (setq inv t)))
                            ;; Check for invalid chars.
                            (dolist (x l)
                              (or (member x dice)
                                  (setq inv t)))
                            (if inv
                              (format t "Invalid input.~%")
                              (progn (setq dice l)
                                     (return-from check)))))))
                ;; The user typed in all dice, clearly unaware of "a".
                (if (= 5 (length dice))
                  (return-from turn dice)))
          ;; Initialize choices.
          (setq choices (mapcar #'car *goals*))
          ;; Remove the choices that are already used.
          (dolist (x boxes)
            (setq choices (remove-if (lambda (y) (eq (car x) y))
                                     choices)))
          ;; Get our choices.
          (setq choices
                (mapcar (lambda (g)
                          (cons g
                                (funcall (second (assoc g *goals*))
                                         dice)))
                        choices))
          ;; Separate fulfilled and unfulfilled goals.
          (setq cross-choices (flatten (remove-if #'cdr choices)))
          (setq choices (remove-if #'single choices))
          ;; Print the choices.
          (loop for i in choices and n from 1 do
                (format t "~a) ~a: ~a~%" n (car i) (cdr i)))
          (loop for i in cross-choices and n from 0 do
                (format t "~a) __~a__~%" (code-char (+ 65 n)) i))
          ;; Get a selection.
          (setq selection nil)
          (loop named check do
                (princ "Enter your selection: ")
                (let ((input (read-line)))
                  (if (not (= (length input) 1))
                    (format t "Invalid input.~%")
                    (let ((int (parse-integer input :junk-allowed t)))
                      (if int
                        (setq selection (nth (1- int) choices))
                        (let ((cc (- (char-code (char-upcase
                                                  (char input 0)))
                                     65)))
                          (if (>= cc 0)
                            (let ((chr (nth cc cross-choices)))
                              (if chr
                                (setq selection (list chr)))))))
                      (if selection
                        (return-from check)
                        (format t "Invalid input.~%"))))))
          ;; Update the list of checked boxes.
          (push selection boxes)
          ;; Update score (and bonus?)
          (when (cdr selection) ; not crossing
            (incf score (cdr selection))
            (when (member (car selection)
                          '("Ones" "Twos" "Threes"
                            "Fours" "Fives" "Sixes")
                          :test #'equal)
              (progn (incf bonus (cdr selection))
                     (format t "Bonus: ~a/63~%" bonus))))
          ;; Check if it's time to end the game.
          (when (= (length boxes) (length *goals*))
            (format t "Game over.~%")
            (sort boxes (lambda (x y) (null (cdr y))))
            ;; Sort according to *goals*.
            (dolist (x (mapcar (lambda (x) (assoc x boxes))
                               (mapcar #'first *goals*)))
              (format t "~a: ~a~%"
                      x
                      (or (cdr x) "--")))
            (if (>= bonus 63)
              (incf score 50))
            (format t "Bonus: ~a/63~%" bonus)
            (format t "Score: ~a~%" score)
            (return-from game)))))
