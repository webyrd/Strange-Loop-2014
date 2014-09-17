(load "cl-pairs.scm")

(load "test-check.scm")

;; Tests for improper-list-based (dotted pair) term representation
;; These tests each take on the order of 30 seconds to a minute
(printf "medium pairs tests...\n")

; Synthesize a fixpoint combinator, given the
; definition exists Y . forall x . Yx = x(Yx)
; and the hint Y = ?? for some combinator ?
(time (test "Fixpoint-combinator-self-app"
        (run 1 (Y)
          (fresh (?)
            (eigen (x)
              (== `(,? . ,?) Y)
              (->wo `(,Y . ,x) `(,x . (,Y . ,x))))))
        '((((S . (S . (K . (S . I)))) . I) .
           ((S . (S . (K . (S . I)))) . I)))))

; Put all the pieces together.
;
; Use miniKanren to synthesize and return a self-applicative fipoint
; combinator, Y, then convert to a call-by-value lambda-calculus term,
; F. Extract F from the list returned by run1, then eval the list
; representing the term F, to produce a Scheme procedure (also called
; F). Apply F to the fixpoint-friendly version of factorial, yielding
; !5 = 120.
(test "Fixpoint-combinator-self-app-L-etao-fact"
  (let ((F (eval (car (run 1 (F)
                        (fresh (Y ?)
                          (eigen (x)
                            (== `(,? . ,?) Y)
                            (->wo `(,Y . ,x) `(,x . (,Y . ,x)))
                            (L-etao Y F)))))
                 (environment '(rnrs)))))
    ((F (lambda (f)
          (lambda (n)
            (if (= n 0)
                1
                (* n (f (- n 1)))))))
     5))
  120)
