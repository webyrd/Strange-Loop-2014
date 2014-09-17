(load "cl-pairs.scm")

(load "test-check.scm")

;; Tests for improper-list-based (dotted pair) term representation
(printf "fast pairs tests...\n")

; exercise 2.17b from Hindley and Seldin:
; find a CL term in the SKI basis equivalent to the W combinator, given the axiom
; Wxyz = xyy
(test "Generate W"
  (run 1 (W)
    (eigen (x y)
      (->wo `((,W . ,x) . ,y) `((,x . ,y) . ,y))))
  '(((S . S) . (S . K))))

; find a CL term in the SK basis equivalent to the I combinator, given the axiom
; Ix = x
(test "Generate I"
  (run 1 (I)
    (eigen (x)
      (absento 'I I) ; don't use the I combinator to define I!  S and K only!
      (->wo `(,I . ,x) x)))
  '((((S . K) . _.0) (absento (I _.0))))) ; for example, SKK

; Verify that ((S (S (K (S I)))) I) applied to itself
; is a fixpoint combinator
(test "Fixpoint-combinator-self-app-verify"
  (run 1 (Y)
    (fresh (?)
      (eigen (x)
        (== '(((S . (S . (K . (S . I)))) . I) .
              ((S . (S . (K . (S . I)))) . I))
            Y)
        (== `(,? . ,?) Y)
        (->wo `(,Y . ,x) `(,x . (,Y . ,x))))))
  '((((S . (S . (K . (S . I)))) . I) .
     ((S . (S . (K . (S . I)))) . I))))

; Verify that ((S I) (((S (S (K (S I)))) I) ((S (S (K (S I)))) I)))
; is a fixpoint combinator
(test "Fixpoint-combinator-verify"
  (run 1 (Y)
    (fresh (?)
      (eigen (x)
        (== '((S . I) . (((S . (S . (K . (S . I)))) . I) . ((S . (S . (K . (S . I)))) . I)))
            Y)
        (->wo `(,Y . ,x) `(,x . (,Y . ,x))))))
  '(((S . I) . (((S . (S . (K . (S . I)))) . I) . ((S . (S . (K . (S . I)))) . I)))))

; Translate the self-applicative fixpoint combinator
; (((S (S (K (S I)))) I) ((S (S (K (S I)))) I))
; into a call-by-name lambda-calculus term.
; This combinator diverges in Scheme.
(test "Lo"
  (run* (q)
    (Lo
      '(((S . (S . (K . (S . I)))) . I) . ((S . (S . (K . (S . I)))) . I))
      q))
  '(((((lambda (x)
         (lambda (y)
           (lambda (z)
             ((x z) (y z)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z) ((x z) (y z)))))
        ((lambda (x)
           (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z) ((x z) (y z)))))
          (lambda (x) x)))))
      (lambda (x) x))
     (((lambda (x)
         (lambda (y)
           (lambda (z) ((x z) (y z)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z) ((x z) (y z)))))
        ((lambda (x)
           (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z) ((x z) (y z)))))
          (lambda (x) x)))))
      (lambda (x) x)))))


; Translate the self-applicative fixpoint combinator
; (((S (S (K (S I)))) I) ((S (S (K (S I)))) I))
; into a call-by-value lambda-calculus term
(test "L-etao"
  (run* (q)
    (L-etao
     '(((S . (S . (K . (S . I)))) . I) . ((S . (S . (K . (S . I)))) . I))
     q))
  '(((((lambda (x)
         (lambda (y)
           (lambda (z)
             (lambda (w)
               (((lambda (v) ((x z) v)) (lambda (v) ((y z) v)))
                w)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z)
              (lambda (w)
                (((lambda (v) ((x z) v))
                  (lambda (v) ((y z) v)))
                 w)))))
        ((lambda (x) (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z)
                (lambda (w)
                  (((lambda (v) ((x z) v))
                    (lambda (v) ((y z) v)))
                   w)))))
          (lambda (x) x)))))
      (lambda (x) x))
     (((lambda (x)
         (lambda (y)
           (lambda (z)
             (lambda (w)
               (((lambda (v) ((x z) v)) (lambda (v) ((y z) v)))
                w)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z)
              (lambda (w)
                (((lambda (v) ((x z) v))
                  (lambda (v) ((y z) v)))
                 w)))))
        ((lambda (x) (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z)
                (lambda (w)
                  (((lambda (v) ((x z) v))
                    (lambda (v) ((y z) v)))
                   w)))))
          (lambda (x) x)))))
      (lambda (x) x)))))
