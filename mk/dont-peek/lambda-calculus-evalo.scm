(load "../mk.scm")

(define lookupo
  (lambda (x env val)
    (fresh (y v rest)
      (symbolo x)
      (symbolo y)
      (== `((,y . ,v) . ,rest) env)
      (conde
        [(== x y) (== v val)]
        [(=/= x y) (lookupo x rest val)]))))

(define evalo
  (lambda (expr env val)
    (conde
      [(fresh (datum)
         (== `(quote ,datum) expr)
         (== datum val))]
      [(symbolo expr)
       (lookupo expr env val)]
      [(fresh (x e)
         (== `(lambda (,x) ,e) expr)
         (== `(closure ,x ,e ,env) val)
         (symbolo x))]
      [(fresh (e1 e2 x e env^ v2)
         (== `(,e1 ,e2) expr)
         (evalo e1 env `(closure ,x ,e ,env^))
         (evalo e2 env v2)
         (evalo e `((,x . ,v2) . ,env^) val))])))
