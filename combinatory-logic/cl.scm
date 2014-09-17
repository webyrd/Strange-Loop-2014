(load "mk.scm")

;; weak-reducer for Combinatory Logic, from pages 24 and 25 of Hindley
;; and Seldin 'Lambda-Calculus and Combinators: An Introduction',
;; 2008, Cambridge University Press

; see short-cl.scm for a shorter but equivalent version of this code using Prolog-style syntax

; Contract an occurrence of a weak redex, using
; axioms for S, K, and I combinators
(define contracto
  (lambda (T T^)
    (conde
      ((== `(I ,T^) T)) ; Ix = x  (optional, since I can be defined using S and K)
      ((fresh (y)
         (== `((K ,T^) ,y) T))) ; Kxy = x
      ((fresh (x y z)
         (== `(((S ,x) ,y) ,z) T) ; Sxyz = xz(yz)
         (== `((,x ,z) (,y ,z)) T^))))))

; Perform a single combinator reduction, anywhere in the term
(define ->1wo
  (lambda (T T^)
    (conde
      ((contracto T T^)) ; contract top-level term T, producing T'
      ((fresh (M N P)
         (== `(,M ,N) T)
         (conde
           ((== `(,P ,N) T^) ; Reduce M in term MN to P, producing PN
            (->1wo M P))
           ((== `(,M ,P) T^) ; Reduce N in term MN to P, producing MP
            (->1wo N P))))))))

; Weak reduction (reflexive, transitive closure of ->1wo)
(define ->wo
  (lambda (M N)
    (conde
      ((== M N)) ; M weakly reduces to itself
      ((fresh (P) (->1wo M P) (->wo P N))))))
      ; if M ->1w P, and P ->w N, then M ->w N


; Relation between combinatory logic term T and call-by-name lambda-calculus term T'
; (see http://en.wikipedia.org/wiki/Combinatory_logic#Reverse_conversion)
(define Lo
  (lambda (T T^)
    (conde
      ((== 'I T) (== '(lambda (x) x) T^)) ; I = \x.x
      ((== 'K T) (== '(lambda (x) (lambda (y) x)) T^)) ; K = \xy.x
      ((== 'S T) (== '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) T^)) ; S = \xyz.xz(yz)
      ((fresh (M N M^ N^) ; If T = MN, recur in both M and N
         (== `(,M ,N) T)
         (== `(,M^ ,N^) T^)
         (Lo M M^)
         (Lo N N^))))))

; Identical to Lo, above, except T' is a term in call-by-value lambda-calculus
; This is achieved by performing eta expansion to the applications inside S  
(define L-etao
  (lambda (T T^)
    (conde
      ((== 'I T) (== '(lambda (x) x) T^)) ; I = \x.x
      ((== 'K T) (== '(lambda (x) (lambda (y) x)) T^)) ; K = \xy.x
      ((== 'S T)  ; S = \xyzw.((\v.xzv \v.yzv) w)
       (== '(lambda (x)
              (lambda (y)
                (lambda (z)
                  (lambda (w)
                    (((lambda (v) ((x z) v))
                      (lambda (v) ((y z) v)))
                     w)))))
           T^))
      ((fresh (M N M^ N^) ; If T = MN, recur in both M and N
         (== `(,M ,N) T)
         (== `(,M^ ,N^) T^)
         (L-etao M M^)
         (L-etao N N^))))))
