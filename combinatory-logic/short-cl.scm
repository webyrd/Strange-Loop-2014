(load "mk.scm")
(load "matche.scm")

; The combinatory logic rules from cl.scm, implemented using
; Prolog-style syntax.

; Axioms for S, K, and I combinators
(defmatche (contracto T T^)
  (((I ,x) ,x)) ; Ix = x  (optional, since I can be defined using S and K)
  ((((K ,x) ,y) ,x)) ; Kxy = x
  (((((S ,x) ,y) ,z) ((,x ,z) (,y ,z))))) ; Sxyz = xz(yz)

;; (defmatche (contracto T T^)
;;   (((((S ,M) ,N) ,X) ((,M ,X) (,N ,X))))  ; Sxyz = xz(yz)
;;   (((((F ,O) ,M) ,N) ,M) (conde [(== 'S O)] [(== 'F O)]))
;;   (((((F (,P ,Q)) ,M) ,N) ((,N ,P) ,Q))
;;    (fresh (M^)
;;      (conde
;;        [(== 'S P)]
;;        [(== `(S ,M^) P)]
;;        [(== 'F P)]
;;        [(== `(F ,M^) P)]))))

;; (run* (q)
;;   (contracto '(((F F) foo) bar) q))

; Perform a single combinator reduction, anywhere in the term
(defmatche (->1wo T T^)
  ((,M ,M^) (contracto M M^)) ; contract top-level term M, producing M'
  (((,M ,N) (,M^ ,N)) (->1wo M M^)) ; Recur on M in term MN, producing M'N
  (((,M ,N) (,M ,N^)) (->1wo N N^))) ; Recur on N in term MN, producing MN' 

; Weak reduction (reflexive, transitive closure of ->1wo)
(defmatche (->wo T T^)
  ((,M ,M)) ; M weakly reduces to itself: M ->w M
  ((,M ,P) (fresh (N) (->1wo M N) (->wo N P))))
  ; if M ->1w N, and N ->w P, then M ->w P

(run 1 (K)
  (== '(F F) K)
  (eigen (x y)
    (->wo K x)))

(run 1 (K)
  (== '(F F) K)
  (eigen (x y)
    (->wo K x)))

(run 1 (K)
  (eigen (x y)
    (->wo K x)))



; Relation between combinatory logic term T and call-by-name lambda-calculus term T'
; (see http://en.wikipedia.org/wiki/Combinatory_logic#Reverse_conversion)
(defmatche (Lo T T^)
  ((I (lambda (x) x))) ; I = \x.x
  ((K (lambda (x) (lambda (y) x)))) ; K = \xy.x
  ((S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))) ; S = \xyz.xz(yz)
  (((,M ,N) (,M^ ,N^)) (Lo M M^) (Lo N N^))) ; If T = MN, recur in both M and N

; Identical to Lo, above, except T' is a term in call-by-value lambda-calculus
; This is achieved by performing eta expansion to the applications inside S
(defmatche (L-etao T T^)
  ((I (lambda (x) x))) ; I = \x.x
  ((K (lambda (x) (lambda (y) x)))) ; K = \xy.x
  ((S (lambda (x) (lambda (y) (lambda (z) (lambda (w) (((lambda (v) ((x z) v)) (lambda (v) ((y z) v))) w))))))) ; S = \xyzw.((\v.xzv \v.yzv) w)
  (((,M ,N) (,M^ ,N^)) (L-etao M M^) (L-etao N N^))) ; If T = MN, recur in both M and N
