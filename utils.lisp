;; (prinf x) {{{
(defun prinf (x)
  (let ((r (princ x)))
    (fresh-line)
    r))
;; }}}
;; (rand n) {{{
(defun rand (n)
  (let ((*random-state* (make-random-state t)))
    (random n)))
;; }}}
;; (randnth lst) {{{
(defun randnth (lst)
  (nth (rand (length lst)) lst))
;; }}}
;; (cdnth (idx lst)) {{{
(defun cdnth (idx lst)
  (nthcdr idx lst))

;; support for (cdnth <idx> <lst>) as an assignable place
;; Thanks to Rainer Joswig of StackOverflow for this
(define-setf-expander cdnth (idx lst &environment env)
                      (multiple-value-bind (dummies vals newval setter getter)
                        (get-setf-expansion lst env)
                        (let ((store (gensym))
                              (idx-temp (gensym)))
                          (values dummies
                                  vals
                                  `(,store)
                                  `(let ((,idx-temp ,idx))
                                     (progn
                                       (if (zerop ,idx-temp)
                                         (progn (setf ,getter ,store))
                                         (progn (rplacd (nthcdr (1- ,idx-temp) ,getter) ,store)))
                                       ,store))
                                  `(nthcdr ,idx ,getter)))))
;; }}}
;; (shuffle lst) {{{
(defun shuffle (lst)
  (let ((l (copy-list lst)))
    (loop for n below (length l) do
          (rotatef (nth n l)
                   (nth (rand (length l)) l)))
    l))
;; }}}
;; (coinflip) {{{
(defun coinflip (&rest ignore)
  (> (rand 10) 4))
;; }}}
;; On Lisp {{{
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;; Efficiently checks if list x is longer than y
(defun longer (x y)
  (and (consp x)
       (or (null y)
           (longer (cdr x) (cdr y)))))

;; Like mapcar with remove-if #'null
(defun filter (fn lst)
  (let (acc)
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

;; Group list into sublists of length num, with extra elements in a
;; final list
(defun group (lst n)
  (if (zerop n) (error "zero length"))
  (if lst
    (if (>= (length lst) n)
      (cons (subseq lst 0 n) (group (nthcdr n lst) n))
      (list lst))))

;; Makes a list of all atoms in a tree
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                    (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; Recursive remove-if
(defun prune (test tree)
  (labels ((rec (tree acc)
                (cond ((null tree) (nreverse acc))
                      ((consp (car tree))
                       (rec (cdr tree)
                            (cons (rec (car tree) nil) acc)))
                      (t (rec (cdr tree)
                              (if (funcall test (car tree))
                                acc
                                (cons (car tree) acc)))))))
    (rec tree nil)))


;; Like find-if, but also returns the value returned by the function
(defun find2 (fn lst)
  (and lst
       (let ((val (funcall fn (car lst))))
         (if val
           (values (car lst) val)
           (find2 fn (cdr lst))))))

;; Efficiently checks if x occurs before y
(defun before (x y lst &key (test #'eql))
  (if lst
    (cond ((funcall test (car lst) y) nil)
          ((funcall test (car lst) x) lst)
          (t (before x y (cdr lst) :test test)))))

;; Or x after y (only this one verifies that both elements occur in the
;; list; before returns once x is found)
(defun after (x y lst &key (test #'eql))
  (let ((r (before y x lst :test test)))
    (and r (member x r :test test))))

;; Checks if obj is duplicated in lst
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

;; Return elements before and after the first that meets a predicate;
;; primarily useful for sorted data
(defun split-if (fn lst)
  (let (acc)
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;; Non-destructive mapcan; uses append instead of nconc
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

;; Recursive mapcar
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar
           (lambda (&rest args)
             (apply #'rmapcar fn args))
           args)))
;; }}}
;; (remove2 fn lst) {{{
;; Like remove-if, but also returns the removed elements
(defun remove2 (fn lst)
  (and lst
       (let (keep disc)
         (dolist (x lst)
           (if (funcall fn x)
             (push x disc)
             (push x keep)))
         (values (nreverse keep) (nreverse disc)))))
;; }}}
