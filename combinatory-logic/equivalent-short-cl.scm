(load "mk.scm")

; Equivalent code to that in short-cl.scm, written in terms of conde
; instead of defmatche.

; Axioms for S, K, and I combinators
(define (contracto T T^)
  (conde
    ((fresh (x) (== `((I ,x) ,x) `(,T ,T^)))) ; Ix = x  (optional, since I can be defined using S and K)
    ((fresh (x y) (== `(((K ,x) ,y) ,x) `(,T ,T^)))) ; Kxy = x
    ((fresh (x y z) (== `((((S ,x) ,y) ,z) ((,x ,z) (,y ,z))) `(,T ,T^)))))) ; Sxyz = xz(yz)

; Perform a single combinator reduction, anywhere in the term
(define (->1wo T T^)
  (conde
    ((contracto T T^)) ; contract top-level term T, producing T'
    ((fresh (M N M^) ; Recur on M in term MN, producing M'N
       (== `((,M ,N) (,M^ ,N)) `(,T ,T^))
       (->1wo M M^)))
    ((fresh (M N N^) ; Recur on N in term MN, producing MN' 
       (== `((,M ,N) (,M ,N^)) `(,T ,T^))
       (->1wo N N^)))))

; Weak reduction (reflexive, transitive closure of ->1wo)
(define (->wo T T^)
  (conde
    ((== T T^)) ; M weakly reduces to itself: M ->w M
    ((fresh (N) (->1wo T N) (->wo N T^)))))
    ; if M ->1w N, and N ->w P, then M ->w P




; Relation between combinatory logic term T and call-by-name lambda-calculus term T'
; (see http://en.wikipedia.org/wiki/Combinatory_logic#Reverse_conversion)
(define Lo
  (lambda (T T^)
    (conde
      ((== `(I (lambda (x) x)) `(,T ,T^))) ; I = \x.x
      ((== `(K (lambda (x) (lambda (y) x))) `(,T ,T^))) ; K = \xy.x
      ((== `(S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z)))))) `(,T ,T^))) ; S = \xyz.xz(yz)
      ((fresh (M N M^ N^) ; If T = MN, recur in both M and N
         (== `((,M ,N) (,M^ ,N^)) `(,T ,T^))
         (Lo M M^) (Lo N N^))))))

; Identical to Lo, above, except T' is a term in call-by-value lambda-calculus
; This is achieved by performing eta expansion to the applications inside S
(define L-etao
  (lambda (T T^)
    (conde
      ((== `(I (lambda (x) x)) `(,T ,T^))) ; I = \x.x
      ((== `(K (lambda (x) (lambda (y) x))) `(,T ,T^))) ; K = \xy.x
      ((== `(S (lambda (x) (lambda (y) (lambda (z) (lambda (w) (((lambda (v) ((x z) v)) (lambda (v) ((y z) v))) w)))))) `(,T ,T^))) ; S = \xyzw.((\v.xzv \v.yzv) w)
      ((fresh (M N M^ N^) ; If T = MN, recur in both M and N
         (== `((,M ,N) (,M^ ,N^)) `(,T ,T^))
         (L-etao M M^) (L-etao N N^))))))
