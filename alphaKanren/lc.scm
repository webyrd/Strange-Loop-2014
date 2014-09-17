(load "alphaKanren.scm")

;; Adopted from examples/lam.apl from alphaProlog release 'aprolog-0.3'
;; (see http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/)

(define betao
  (lambda (t1 E^^)
    (fresh (b)
      (exist (E E^)
        (== `(app (lam ,(tie b E)) ,E^) t1)
        (substo (tie b E) E^ E^^)))))

(define substo
  (lambda (id/tm E out)
    (conde
      ((fresh (a)
         (== (tie a `(var ,a)) id/tm)
         (== E out)))
      ((fresh (a)
         (exist (B)
           (hash a B)
           (== (tie a `(var ,B)) id/tm)
           (== `(var ,B) out))))
      ((fresh (a b)
         (exist (E1 E1^)
           (hash b E)
           (== (tie a `(lam ,(tie b E1))) id/tm)
           (== `(lam ,(tie b E1^)) out)
           (substo (tie a E1) E E1^))))
      ((fresh (a)
         (exist (E1 E2 E1^ E2^)
           (== (tie a `(app ,E1 ,E2)) id/tm)
           (== `(app ,E1^ ,E2^) out)
           (substo (tie a E1) E E1^)
           (substo (tie a E2) E E2^)))))))

(define stepo
  (lambda (t1 t2)
    (conde
      ((betao t1 t2))
      ((exist (M N M^)
         (== `(app ,M ,N) t1)
         (== `(app ,M^ ,N) t2)
         (stepo M M^)))
      ((exist (M N N^)
         (== `(app ,M ,N) t1)
         (== `(app ,M ,N^) t2)
         (stepo N N^))))))

;; reflexive transitive closure of stepo
(define stepso
  (lambda (t1 t2)
    (conde
      ((== t1 t2))
      ((exist (t)
         (stepo t1 t)
         (stepso t t2))))))

(define step-equalo
  (lambda (t1 t2)
    (conde
      ((== t1 t2))
      ((exist (t1^)
         (stepo t1 t1^)
         (step-equalo t1^ t2)))
      ((exist (t2^)
         (stepo t2 t2^)
         (step-equalo t1 t2^))))))

#!eof

;;; program that steps to itself
(run 1 (Q) (fresh (z) (hash z Q) (stepo Q Q)))

(run 1 (Y)
  (fresh (x)
    (exist (U)
      (hash x Y)
      (step-equalo `(app ,Y (var ,x)) `(app (var ,x) (app ,Y (var ,x)))))))

(run 1 (Y)
  (fresh (f x)
    (exist (U)
      (== `(lam ,(tie f `(app ,U ,U))) Y)
      (hash x Y)
      (step-equalo `(app ,Y (var ,x)) `(app (var ,x) (app ,Y (var ,x)))))))
