(defun Gfn (str) 
  (print '(in Gfn)) (print str) 
  (cond ((member (car str) '(x y z w)) (cdr str)) 
        (t (append (cdr str) (list 'err)))))

(defun Aprimefn (str) 
  (print '(in Aprimefn)) (print str) 
  (cond ((eql (car str) 'o) 
         (let ((g-result (Gfn (cdr str)))) 
           (cond ((null g-result) (append (cdr str) (list 'err))) 
                 (t (Aprimefn g-result))))) 
        (t str)))  ; ε case 

(defun Efn (str) 
  (print '(in Efn)) (print str) 
  (let ((g-result (Gfn str))) 
    (cond ((null g-result) (append (cdr str) (list 'err))) 
          (t (Aprimefn g-result))))) 

(defun Lprimefn (str) 
  (print '(in Lprimefn)) (print str) 
  (cond ((eql (car str) 's) (Lprimefn (cdr str))) 
        (t str)))  ; ε case 

(defun Lfn (str) 
  (print '(in Lfn)) (print str) 
  (cond ((eql (car str) 's) (Lprimefn (cdr str))) 
        (t (append (cdr str) (list 'err))))) 

(defun Bfn (str) 
  (print '(in Bfn)) (print str) 
  (cond ((eql (car str) 'b) (cdr str)) 
        (t (append (cdr str) (list 'err)))))

(defun Sfn (str)
  (print '(in Sfn)) (print str)
  (cond ((eql (car str) 's)
         (let ((s-result (cdr str)))
           (if (and (eql (car s-result) 'e) (eql (cadr s-result) 's))
               s-result
               (append (cdr str) (list 'err "Expected es after s")))))
        ((eql (car str) 'd)
         (let ((l-result (Lfn (cdr str))))
           (cond ((null l-result) (append (cdr str) (list 'err)))
                 (t (Bfn l-result)))))
        (t (append (cdr str) (list 'err)))))

(defun Ifn (str)
  (print '(in Ifn)) (print str)
  (let ((result
         (cond ((eql (car str) 'i)
                (let ((e-result (Efn (cdr str))))
                  (cond ((null e-result)
                         (append (cdr str) (list 'err)))
                        (t
                         (let ((s-result (Sfn e-result)))
                           (cond ((and (consp s-result) (member 'err s-result))
                                  s-result)
                                 ((null s-result)
                                  (list 'err "Expected es at end"))
                                 ((and (consp s-result) (eql (car s-result) 'e) (eql (cadr s-result) 's))
                                  (if (null (cddr s-result))
                                      nil
                                      (append (cddr s-result) (list 'err "Extra tokens after es"))))
                                 (t
                                  (append s-result (list 'err "Expected es at end")))))))))
               (t (append (cdr str) (list 'err))))))
    (print result)
    (values)))
