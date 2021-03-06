;; can load short-cl.scm or equivalent-short-cl.scm instead
(load "cl.scm")

(load "test-check.scm")

;; These tests each take on the order of ten minutes or more
(printf "slow tests...\n")

; Synthesize a fixpoint combinator, Y, from its definition:
; exists Y . forall x . Yx = x(Yx)
(time (test "Fixpoint-combinator"
        (run 1 (Y) (eigen (x) (->wo `(,Y ,x) `(,x (,Y ,x)))))
        '(((S I) (((S (S (K (S I)))) I) ((S (S (K (S I)))) I))))))

; Put all the pieces together.
;
; Use miniKanren to synthesize and return a self-applicative fipoint
; combinator, Y, then convert to a call-by-value lambda-calculus term,
; F. Extract F from the list returned by run1, then eval the list
; representing the term F, to produce a Scheme procedure (also called
; F). Apply F to the fixpoint-friendly version of factorial, yielding
; !5 = 120.
(test "Fixpoint-combinator-L-etao-fact"
  (let ((F (eval (car (run 1 (F)
                        (fresh (Y)
                          (eigen (x)
                            (->wo `(,Y ,x) `(,x (,Y ,x)))
                            (L-etao Y F)))))
                 (environment '(rnrs)))))
    ((F (lambda (f)
          (lambda (n)
            (if (= n 0)
                1
                (* n (f (- n 1)))))))
     5))
  120)
