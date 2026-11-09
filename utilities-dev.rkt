#lang curly-fn racket
(require racket/system racket/match racket/hash racket/generator net/http-client)
(require racket/struct (for-syntax racket/function) srfi/1 racket/control  (for-syntax racket/syntax) (for-syntax racket/list))
;(require "shell-like.rkt")
;(require racket/treelist)

(define-for-syntax (format+ . args)
  (λ (x)
    (apply format `(,@args ,x))))
  
(define (procedurize el)
  (match el
    ((? procedure?) el)
    ((? output-port?) #{copy-port (open-input-string* %) el})
    (_ (const el))))

(define (falsy? . args)
  (andmap (λ (var) (or (not var) (equal? var 0) (equal? var '()) (equal? var #()))) args))

(define-syntax-rule (when-falsy expr body ...)
  (when (falsy? expr) body ...))

(define-for-syntax (~> . args)
  (let ((mapped-args (map (λ (el) (if (procedure? el) el (const el))) args)))
    (foldl compose (car mapped-args) (cdr mapped-args))))

(define curmap (curry map))
(define curfilter (curry filter))

;(define (~> . args)
;  (reset (reduce compose #f (map procedurize args))))


(define (each lst)
  (shift k (for-each k lst)))

(define (sum lst)
  (shift k (foldl + 0 (map k lst))))



(define-syntax-rule (dbg-show var)
  (dbg-print-name-value 'var var))

(define-syntax ds (make-rename-transformer #'dbg-show))

(define-syntax-rule (dbg-apply fn args ...)
  (let ()
    (define dbg-result (fn args ...))
    (dbg-print-name-value '(fn -result) dbg-result)
    dbg-result))

(define (open-input-string* . s) (open-input-string (apply ~a s)))


(define (~>fn . args) (apply compose (reverse (map (λ (el)
                                                     (cond
                                                       ((procedure? el) el)
                                                       ;((output-port? el) #{copy-port (open-input-string* %) el})
                                                       (else (λ _ el))))
                                                   args))))

(define-syntax ~>
  (syntax-rules (!)

    [(_ arg !)
     ((~> arg))]

    [(_ arg1 ! arg2 ...)
     (~> (~> arg1 !) arg2 ...)]

    [(_ arg1 arg2 arg3 ...)
     (~> (~>fn arg1 arg2) arg3 ...)]

    [(_ arg ...)
     (~>fn arg ...)]
    ))

(define-syntax-rule (~>! arg ...) (~> arg ... !))

(define (dbg-print-name-value name value)
  (let/cc return
    ((~>
      (if (list? name) (apply ~a name) name)
      (curry printf "(DEBUG) ~a:")))
  
    (let ((formatted-var (pretty-format value)))
      (if #{string-contains? formatted-var "\n"}
          (begin (printf "~a ~n~a~n" name formatted-var) (return value))
          (begin (printf "~a ~a~n" name formatted-var) (return value))))))


;(define (~out-port~> out-port) (~> ~a open-input-string #{copy-port % out-port}))

(define-syntax-rule (~define~> pipe-steps ... name)
  (define name ((~> pipe-steps ...))))

(define-syntax-rule (~define-fn~> pipe-steps ... name)
  (define name (~> pipe-steps ...)))

(define-syntax-rule (defparameter name default)
  (define name (make-parameter default)))

(define-syntax letfine
  (syntax-rules (_ =>)
    [(_ ()) (void)]

    [(_ ((name value) ... (_ valuef)))
     (begin
       (define name value) ...
       valuef)]

    [(_ ((name value) ...))
     (begin
       (define name value) ...)]))


(define-syntax let**
  (syntax-rules (_ =>)

    [(_ ((name value) ... (_ value1 => var-name)))
     (define var-name (let** ((name value) ... (_ value1))))]
    
    [(_ ((name value) ... (_ value1)))
     (let* ((name value) ... ) value1)]

    [(_ arg ((name value) ... (_ value1)))
     (λ (arg) (let** ((name value) ... (_ value1))))]))

;(define-syntax-rule (printf~ fmt var1)
;  (begin (printf fmt var1) var1))

(define-syntax printf~
  (syntax-rules ()
    ((_ fmt var0)
     (let ((var1 var0)) (printf fmt var1) var1))

    ((_ fmt var0 ... varN)
     (let ((varN1 varN)) (printf fmt var0 ... varN1) varN1))))

(define-syntax-rule (import-env name default)
  (define name (or (getenv (symbol->string 'name)) default)))

(define-syntax-rule (λ/1 arg-name body ...)
  (lambda (arg-name) body ...))

(define-syntax-rule (λ/2 arg1-name arg2-name body ...)
  (lambda (arg1-name arg2-name) body ...))

(define-syntax-rule (λ/3 arg1-name arg2-name arg3-name body ...)
  (lambda (arg1-name arg2-name arg3-name) body ...))

(define-syntax-rule (λ/4 arg1-name arg2-name arg3-name arg4-name body ...)
  (lambda (arg1-name arg2-name arg3-name arg4-name) body ...))

(define (substitute pred to val) (if (pred val) to val))



(define (s/ pat replace)
  (match pat
    ((? pregexp? re) #{regexp-replace re % replace})
    ((? procedure? fn) (curry substitute fn replace))
    ((var ?_pat) (curry substitute (curry equal? ?_pat) replace))))


(define split
  (~> (curry regexp-split #px"\\s+") (curry filter non-empty-string?)))



(define (switch-trim switch)
  ((~> switch ~a #{substring % 2} string->symbol)))


(define (hw) (displayln "Hello, world!"))
(define (li) (displayln "Lorem ipsum dolor sit amet, consectetur adipiscing elit"))

(define (eval-string str ns) (eval (read (open-input-string str)) ns))

(define (list_of_pairs->pair_of_lists lst)
  (list (map car lst) (map cdr lst)))

;(define (~ input pattern)
(define set$ (lambda (p #:foo foo #:q q) #t))

(define intermediates #f)


;(define-values (kws1 kws2) (procedure-keywords f))


(define-syntax-rule (define-v name value)
  (begin
    (displayln (~a (~s 'name) " set to " (~s value)))
    (define name value)))

(define (set-current-command-line-arguments . args)
  (current-command-line-arguments (list->vector (map ~a args))))

(define-syntax-rule (set-- args ...)
  (set-current-command-line-arguments (quote args) ...))

(define (thunky proc . args)
  (thunk (apply proc args)))

;; ensure that you get a number if possible and don't fail
(define ($# f . args)
  (let ((val (apply f args)))
    (if (number? val) val (string->number (~a val)))))

(define (argv . args)
  (define set-argv (~> list->vector current-command-line-arguments))
  (cond
    ((null? args) (current-command-line-arguments))
    ((> (length args) 1) (set-argv args))
    ((list? (car args)) (set-argv (car args)))
    ((vector? (car args)) (current-command-line-arguments (car args)))
    (else (current-command-line-arguments (vector (car args))))))

(define (thapply func . args)
  (thunk (apply func args)))

(provide dbg-print-name-value dbg-show dbg-apply ~> ~>! ~define~> ~define-fn~> letfine
         import-env substitute s/ switch-trim let**  λ/1 λ/2  eval-string hw li argv when-falsy falsy?
         curmap curfilter printf~ define-v set-current-command-line-arguments set-- thunky $# thapply ~>fn procedurize)
