#lang racket
;;(values (write-byte 97) (write-byte 98) (write-byte 99))

 ;;(begin (values) (values))
 ;;(values (begin (values 1) (values 1 2 3)))
 ;;(values (values 1 2 3))
 ;;(values (values 1) (values 2 3 4) (values 3))
 ;;(let-values ([(x y z) (values (make-vector (values (values 3)) (values 5))(make-vector (values (values 3)) (values 5))(make-vector (values (values 3)) (values 5)))]) (cons x (cons y (cons z '()))))

;;test if vector set works as intended
;;(vector-set! (make-vector 4 4) 2 (values 1))
;;(vector-set! (make-vector 4 4) 2 (values)) ;;err
;;(vector-set! (make-vector 4 4) 2 (values 1 2 3)) ;;err











    
