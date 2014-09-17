(load "pmatch.scm")

(define eval
  (lambda (exp env)
    (pmatch exp
      (,x (guard (symbol? x)) 
          (lookup x env))
      ((lambda (,y) ,body)         
       `(closure ,y ,body ,env))
      ((,rator ,rand) 
       (let ((arg (eval rand env)))
         (pmatch (eval rator env)
           ((closure ,x ,body ,env^)
            (eval body 
                  `((,x . ,arg) . ,env^)))))))))

(define lookup
  (lambda (x env)
    (pmatch env
      (() (error 'unbound))
      (((,y . ,v) . ,env^)
       (if (eq? x y)
           v
           (lookup x env^))))))

(eval
 '(((lambda (x)
      (lambda (y) x))
    (lambda (z) z))
   (lambda (w) w))
 '())
; (closure z z ())

(eval
 '((lambda (x) (lambda (y) y))
   (lambda (z) z))
 '())
; (closure y y ((x closure z z ())))
