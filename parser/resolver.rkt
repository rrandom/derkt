#lang racket

(require racket/runtime-path)
(require "../common/isa-manager.rkt")
(require "../model/hbc.rkt")
(require "tables.rkt")

(provide resolve-instruction
         instruction->hash
         print-instruction
         get-instruction-metadata)

;; Pre-calculated strings for registers and small constants (0-255)
(define num-string-cache
  (let ([v (make-vector 256)])
    (for ([i (in-range 256)])
      (vector-set! v i (string->immutable-string (number->string i))))
    v))

(define reg-string-cache
  (let ([v (make-vector 256)])
    (for ([i (in-range 256)])
      (vector-set! v i (string->immutable-string (format "r~a" i))))
    v))

;; Cache for instruction metadata to avoid repeated dynamic-require
(define metadata-cache (make-hash))

(define (get-instruction-metadata ver)
  (hash-ref! metadata-cache ver (lambda () (get-metadata ver))))

;; Optimized resolution for S-expressions
(define (resolve-instruction hbc paired-inst opcode metadata [resolve-args? #t])
  (define inst (cdr paired-inst)) ;; Extract raw instruction from (offset . instr)
  (define info (if (vector? metadata) (vector-ref metadata opcode) (hash-ref metadata opcode #f)))
  (unless info (error "No metadata for opcode:" opcode))

  (define mnemonic (first info))
  (define arg-types (second info))
  (define raw-args (cddr inst))

  (define resolved-args
    (if resolve-args?
        (for/list ([arg raw-args] [type arg-types])
          (if (string-prefix? (symbol->string type) "StringID")
              (get-hbc-string hbc arg)
              arg))
        raw-args))

  (values mnemonic resolved-args))

;; High-speed printer for S-expressions
(define (print-instruction hbc paired-inst out metadata)
  (define inst (cdr paired-inst)) ;; Extract raw instruction
  (define opcode (second inst))
  (define info (if (vector? metadata) (vector-ref metadata opcode) (hash-ref metadata opcode #f)))
  (if info
      (let ([mnemonic (first info)]
            [arg-types (second info)]
            [raw-args (cddr inst)])
        (display mnemonic out)
        (unless (null? arg-types)
          (display "  " out)
          (let loop ([args raw-args] [types arg-types] [first? #t])
            (unless (null? args)
              (unless first? (display ", " out))
              (let ([arg (car args)] [type (car types)])
                (cond [(string-prefix? (symbol->string type) "StringID")
                       (display (get-hbc-string hbc arg) out)]
                      [(and (exact-integer? arg) (>= arg 0) (< arg 256))
                       (if (memq type '(Reg8 Reg32))
                           (display (vector-ref reg-string-cache arg) out)
                           (display (vector-ref num-string-cache arg) out))]
                      [else (display arg out)]))
              (loop (cdr args) (cdr types) #f))))
        (newline out))
      (fprintf out "Unknown Instruction (Opcode: ~a)\n" opcode)))

;; Converts an S-expression instruction to a JSON-compatible hash.
(define (instruction->hash hbc paired-inst opcode metadata index _unused_offset)
  (define offset (car paired-inst)) ;; Use the real physical offset from derkt
  (let-values ([(name args) (resolve-instruction hbc paired-inst opcode metadata)])
    (define info (if (vector? metadata) (vector-ref metadata opcode) (hash-ref metadata opcode)))
    (define types (second info))

    (hash 'index index
          'offset (format "~x" offset)
          'offset_decimal offset
          'opcode opcode
          'name (symbol->string name)
          'display (format "~a  ~a" name (string-join (map (lambda (a) (format "~a" a)) args) ", "))
          'operands args
          'operand_types (map symbol->string types))))
