#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r11 'r11)
(define r10 'r10)

;; type CEnv = [Listof Variable]
  
;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register	   
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
               ; restore callee-save register
           ;Determines if sending multiple values or single
          (let ((done (gensym 'done)) 
                (mul (gensym 'mul)) 
                (loop (gensym 'loop))) 
                ;; Checks if value in rax has type-new or single value in rax
                (seq (Mov r9 rax)
                      (And r9 ptr-mask)
                      (Cmp r9 type-new)
                      (Je mul)
                       ; Create and return unary vector holding the result
                      (Mov r8 1)
                      (Mov (Offset rbx 0) r8)  ; write size of vector, 1
                      (Mov (Offset rbx 8) rax) ; write rax as single element of vector
                      (Mov rax rbx)    
                      (Jmp done)
                      ;; Get rid of type for pointer to values data structure
                      (Label mul)
                      (Xor rax type-new) ;;gets rid of the type tag
                      (Label done)))
          
          
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(App f es)         (compile-app f es c)]
    [(Values es)         (compile-val es c)]
    [(Let-Values xs e e0) (compile-let-values xs e e0 c)]))

(define (compile-let-values xs e e0 c)
  (let ((v (gensym 'values)) (done (gensym 'done)) (loop (gensym 'loop)) (emp (gensym 'empty)) (done2 (gensym 'done)))
  (seq (compile-e e c)
        (Mov r9 rax)
        (And r9 ptr-mask)
        (Cmp r9 type-new)
        (Je v)
        (Mov r10 (length xs))
        (Cmp r10 1)
        (Jne 'raise_error_align)
        (Push rax)
        (compile-e e0 (append xs c))
        (Add rsp 8)
        (Jmp done)

        (Label v)
        (Xor rax type-new)
        (Mov r10 (Offset rax 0))
        (Mov r11 (length xs))
        (Cmp r10 r11)
        (Jne 'raise_error_align)
        (Cmp r10 0)
        (Je emp)
        
        

        (Add rax 8)

        (Label loop)
        (Mov r8 (Offset rax 0))
        (Push r8)
        (Add rax 8)
        (Sub r10 1)
        (Cmp r10 0)
        (Jne loop)
        (Label emp)
        (compile-e e0 (append xs c))
        (Add rsp (* 8 (length xs)))
        (Label done))))

(define (compile-val es c)
 (let ((loop (gensym 'loop)) 
        (done (gensym 'done))
        (empty (gensym 'empty))
        (one (gensym 'one))) 
  (seq  
        (compile-es es c)
        (Mov r9 rbx)
        (Or r9 type-new)
        (Mov r8 (length es))
        (Cmp r8 0)
        (Je empty)
        (Cmp r8 1)
        (Je one)
        (Mov (Offset rbx 0) r8)
        (Add rbx 8)

        (Label loop)
        (Pop rax)
        (Mov r10 rax)
        (And r10 ptr-mask)
        (Cmp r10 type-new)
        (Je 'raise_error_align)
        (Mov (Offset rbx 0) rax)
        (Add rbx 8)
        (Sub r8 1)
        (Cmp r8 0)
        (Jne loop)
        (Mov rax r9)
        (Jmp done)

        (Label empty)
        (Mov (Offset rbx 0) r8)
        (Mov rax r9)
        (Jmp done)

        (Label one)
        (Pop rax)
        (Mov r11 rax)      ;;1 arg might return a values type on the heap which would give us an error
        (And r11 ptr-mask) ;;this checks if it's a type-new which would be an error
        (Cmp r11 type-new)
        (Je 'raise_error_align)
        (Label done))))
        

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))
        ))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i))
          )))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str)
              (Mov r11 1))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))
                  ))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)
       ))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)
       ))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)
       ))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
          (Mov r9 rax)
          (And r9 ptr-mask)
          (Cmp r9 type-new)
          (Je 'raise_error_align)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2)
         
          )))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)
       ))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
      (Mov r9 rax)
      (And r9 ptr-mask)
      (Cmp r9 type-new)
      (Je 'raise_error_align)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)
       ))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es2 es (cons #f c))
         (Jmp (symbol->label f))
         (Label r)
         
         )))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))


(define (compile-es2 es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Mov r9 rax)
          (And r9 ptr-mask)
          (Cmp r9 type-new)
          (Je 'raise_error_align)
          (Push rax)
          (compile-es es (cons #f c)))]))





;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
