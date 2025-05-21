#lang racket



; When using (cons a b), b is at lower addr (eg 0x7f56facfc070) and a is at higher addr (eg 0x7f56facfc078)

; Full Tree
; (let ([n1 (cons 6 7)]) 
;   (let ([n2 (cons 14 15)])
;   (let ([n4 (cons 9 10)])
;   (let ([n5 (cons 3 4)])
;   (let ([n8 (cons 1 2)]) 
;   (let ([n9 (cons 19 23)])
;   (let ([n11 (cons 34 11)])
;   (let ([n12 (cons 2 8)])
;   (let ([n3 (cons n1 n2)])
;   (let ([n6 (cons n4 n5)])
;   (let ([n10 (cons n8 n9)])
;   (let ([n13 (cons n11 n12)])
;   (let ([n7 (cons n6 n3)])
;   (let ([n14 (cons n10 n13)])
;   (let ([n15 (cons n7 n14)])
;     (begin 
;         (let ((y (box 8)))
;                 2
;         )
           
;     (collect-garbage))))))))))))))))
; )



; Gap between tree and an inserted node

; (let ([n16 (make-vector 2 9)])
;   (let ([v1 (make-vector 20 4)])
;   (let ([n1 (cons 6 7)]) 
;   (let ([n2 (cons 14 15)])
;   (let ([n4 (cons 9 10)])
;   (let ([n5 (cons 3 4)])
;   (let ([n8 (cons 1 2)]) 
;   (let ([n9 (cons 19 23)])
;   (let ([n11 (cons 34 11)])
;   (let ([n12 (make-vector 2 8)])
;   (let ([n3 (cons n1 n2)])
;   (let ([n6 (cons n4 n5)])
;   (let ([n10 (cons n8 n9)])
;   (let ([n13 (cons n11 n12)])
;   (let ([n7 (cons n6 n3)])
;   (let ([n14 (cons n10 n13)])
;   (let ([n15 (cons n7 n14)])
;     (begin 
        
;     (vector-set! n12 1 n16)      
;     (collect-garbage))))))))))))))))))
; )



; Linked List with gaps

; (let ([b1 (box 0)]) 
;   (let ([b2 (box b1)])
;   (let ([c1 (cons 6 7)])
;   (let ([b3 (box b2)])
;   (let ([v1 (make-vector 15 8)])
;   (let ([b4 (box b3)])
;   (let ([b1 (make-string 23 #\a)])
;   (let ([b5 (box b4)])
;     (begin 
;         (let ((y (box 8)))
;                 2
;         )
           
;     (collect-garbage)))))))))
; )


; Hash Table
; (let ([n1 (box 7)]) 
;   (let ([n2 (box n1)])
;   (let ([n3 (box n2)])
;   (let ([n4 (box 9)])
;   (let ([n5 (box n4)])
;   (let ([n6 (box 2)])
;   (let ([n7 (box 3)])
;   (let ([n8 (box n7)])
;   (let ([n9 (box 11)])
;   (let ([n10 (box n9)])
;   (let ([h1 (make-vector 5 0)])
;     (begin
;     (vector-set! h1 0 n3)
;     (begin
;     (vector-set! h1 1 n5)
;     (begin
;     (vector-set! h1 2 n6)
;     (begin
;     (vector-set! h1 3 n8)
;     (begin
;     (vector-set! h1 4 n10)
;     (begin 
;     (mark-hot h1)
;     (begin 
;     (mark-hot n1)
;     (begin 
;     (mark-hot n2)
;     (begin 
;     (mark-hot n3)
;     (collect-garbage))))))))))))))))))))
; )

