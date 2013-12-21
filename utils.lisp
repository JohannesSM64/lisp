;; (for (x a b) body) {{{
;; (do ((variable initial update)...) ((test1_body)...) body)
(defmacro for (l &rest body)
  `(do ((,(first l) ,(second l) (1+ ,(first l))))
     ((> ,(first l) ,(third l)))
     ,@body))
;; }}}

;; (cdnth (idx list)) {{{
;; Thanks to Rainer Joswig of StackOverflow for this
(defun cdnth (idx list)
  (nthcdr idx list))

;; support for (cdnth <idx> <list>) as an assignable place
(define-setf-expander cdnth (idx list &environment env)
                      (multiple-value-bind (dummies vals newval setter getter)
                        (get-setf-expansion list env)
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
