#lang racket

(require racket/string)

;; =============================================================================
;; Instruction set generator from BytecodeList.def
;; =============================================================================

(struct inst (name args) #:transparent #:mutable)

(define instructions (make-hash))
(define instruction-order '())
(define jump-macros (make-hash))

(define (parse-args-str s)
  (if (string=? s "") '() (map string-trim (string-split s ","))))

(define (parse-c-macro line)
  (let ([m (regexp-match #px"^([A-Za-z0-9_]+) *\\((.*)\\)" line)])
    (if m (values (list-ref m 1) (list-ref m 2)) (values #f #f))))

(define (register-inst! name args)
  (unless (hash-has-key? instructions name)
    (set! instruction-order (cons name instruction-order))
    (hash-set! instructions name (inst name args))))

(define (patch-op! name idx prefix)
  (define i (hash-ref instructions name #f))
  (when i
    (define args (inst-args i))
    (define tidx (sub1 (string->number idx)))
    (when (and (>= tidx 0) (< tidx (length args)))
      (define new-args (list-set args tidx (string-append prefix (list-ref args tidx))))
      (set-inst-args! i new-args))))

(define (process-content content)
  (define merged (regexp-replace* #px"\\\\ *[\r\n]+" content " "))
  (define lines (filter (lambda (l)
                          (let ([t (string-trim l)])
                            (and (> (string-length t) 0)
                                 (not (string-prefix? t "//"))
                                 (not (string-prefix? t "#")))))
                        (string-split merged "\n")))
  (for ([line (string-split merged "\n")])
    (define l (string-trim line))
    (cond
      [(string-prefix? l "#define DEFINE_JUMP")
       (let ([m (regexp-match #px"^#define ([A-Za-z0-9_]+) *\\(([^)]*)\\) *(.*)$" l)])
         (when m (hash-set! jump-macros (list-ref m 1) (list-ref m 3))))]
      [(string-prefix? l "DEFINE_OPCODE_")
       (let-values ([(m a) (parse-c-macro l)])
         (when a (let ([args (parse-args-str a)]) (register-inst! (first args) (rest args)))))]
      [(string-prefix? l "DEFINE_JUMP_")
       (let-values ([(m a) (parse-c-macro l)])
         (define base (first (string-split m "_")))
         (define tmpl (hash-ref jump-macros m (hash-ref jump-macros base #f)))
         (when tmpl
           (define name (first (parse-args-str a)))
           (define expanded (string-replace (string-replace tmpl "name" name) "##" ""))
           (let loop ([cur expanded])
             (let ([m (regexp-match #px"DEFINE_OPCODE_[0-9]+ *\\(([^)]*)\\)" cur)]
                   [p (regexp-match-positions #px"DEFINE_OPCODE_[0-9]+ *\\(([^)]*)\\)" cur)])
               (when m
                 (define args (parse-args-str (list-ref m 1)))
                 (register-inst! (first args) (rest args))
                 (loop (substring cur (cdr (first p)))))))))]
      [(or (string-prefix? l "OPERAND_STRING_ID")
           (string-prefix? l "OPERAND_FUNCTION_ID")
           (string-prefix? l "OPERAND_BIGINT_ID"))
       (let-values ([(m a) (parse-c-macro l)])
         (define pref (cond [(string-contains? m "STRING") "StringID"]
                            [(string-contains? m "FUNCTION") "FunctionID"]
                            [(string-contains? m "BIGINT") "BigIntID"]
                            [else ""]))
         (define args (parse-args-str a))
         (when (>= (length args) 2) (patch-op! (first args) (second args) pref)))])))

(define (gen-code)
  (printf "#lang racket\n(require \"../common/isa.rkt\")\n(provide (all-defined-out))\n")
  (printf "(define-hbc-isa\n")
  (for ([name (reverse instruction-order)] [idx (in-naturals)])
    (define i (hash-ref instructions name))
    (printf "  [#x~a ~a (~a)]\n" (string-upcase (number->string idx 16)) name (string-join (inst-args i) " ")))
  (printf ")\n"))

(module+ main
  (define args (current-command-line-arguments))
  (when (= (vector-length args) 0) (error "Usage: gen-isa.rkt <def-file>"))
  (process-content (file->string (vector-ref args 0)))
  (gen-code))