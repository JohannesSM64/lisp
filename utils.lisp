;; (for (x a b) body) {{{
;; (do ((variable initial update)...) ((test1_body)...) body)
(defmacro for (l &rest body)
  `(do ((,(first l) ,(second l) (1+ ,(first l))))
     ((> ,(first l) ,(third l)))
     ,@body))
;; }}}

;; (rand n) {{{
(defmacro rand (&rest args)
  `(let ((*random-state* (make-random-state t)))
     (random ,@args)))
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

;; (randnth lst) {{{
(defun randnth (lst)
  (nth (rand (length lst)) lst))
;; }}}
