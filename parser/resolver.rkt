#lang racket

(require racket/runtime-path)
(require "../common/isa-manager.rkt")
(require "../model/hbc.rkt")
(require "tables.rkt")

(provide resolve-instruction
         instruction->hash
         print-instruction)

;; Cache for instruction metadata to avoid repeated dynamic-require
(define metadata-cache (make-hash))

(define (get-instruction-metadata ver)
  (hash-ref! metadata-cache ver (lambda () (get-metadata ver))))

;; Optimized resolution for S-expressions: '(Mnemonic Opcode Arg1 Arg2 ...)
(define (resolve-instruction hbc inst opcode ver)
  (define metadata (get-instruction-metadata ver))
  (define info (hash-ref metadata opcode #f))
  (unless info (error "No metadata for opcode:" opcode))

  (define mnemonic (first info))
  (define arg-types (second info))

  ;; Simply drop the first 2 elements: Mnemonic and Opcode
  (define raw-args (drop inst 2))

  (define resolved-args
    (for/list ([arg raw-args] [type arg-types])
      (if (string-prefix? (symbol->string type) "StringID")
          (get-hbc-string hbc arg)
          arg)))

  (values mnemonic resolved-args))

;; High-speed printer for S-expressions
(define (print-instruction hbc inst out ver)
  (define mnemonic (first inst))
  (define opcode (second inst))
  (define metadata (get-instruction-metadata ver))
  (define info (hash-ref metadata opcode #f))
  (if info
      (let ([arg-types (second info)]
            [raw-args (drop inst 2)])
        (fprintf out "~a" mnemonic)
        (unless (empty? arg-types) (display "  " out))
        (for ([arg raw-args] [type arg-types] [idx (in-naturals)])
          (unless (= idx 0) (display ", " out))
          (if (string-prefix? (symbol->string type) "StringID")
              (display (get-hbc-string hbc arg) out)
              (display arg out)))
        (newline out))
      (fprintf out "Unknown Instruction (Opcode: ~a)\n" opcode)))

;; Converts an S-expression instruction to a JSON-compatible hash.
(define (instruction->hash hbc inst opcode ver index offset)
  (let-values ([(name args) (resolve-instruction hbc inst opcode ver)])
    (define metadata (get-instruction-metadata ver))
    (define info (hash-ref metadata opcode))
    (define types (second info))

    (hash 'index index
          'offset (format "~x" offset)
          'offset_decimal offset
          'opcode opcode
          'name (symbol->string name)
          'display (format "~a  ~a" name (string-join (map (lambda (a) (format "~a" a)) args) ", "))
          'operands args
          'operand_types (map symbol->string types))))
