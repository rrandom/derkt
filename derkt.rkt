#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "parser/functions.rkt")
(require "parser/parallel.rkt")
(require "model/hbc.rkt")
(require "model/header.rkt")

(provide (all-from-out "parser/core.rkt")
         (all-from-out "parser/resolver.rkt")
         (all-from-out "parser/functions.rkt")
         (all-from-out "parser/parallel.rkt")
         (all-from-out "model/hbc.rkt")
         (all-from-out "model/header.rkt")
         hbc->json-serializable
         hbc->json-serializable-parallel
         get-function-hasm)

;; Converts the entire HBC file disassembly into a JSON-serializable structure.
(define (hbc->json-serializable hbc)
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (define metadata (get-instruction-metadata ver))
  (for/list ([f-idx (in-range (vector-length (HBCFile-function-headers hbc)))])
    (define insts (get-instructions-for-function hbc f-idx))
    (process-function-insts hbc insts metadata f-idx ver)))

;; High-performance parallel version of JSON serialization.
(define (hbc->json-serializable-parallel filename)
  (define hbc (parse-hbc-file filename))
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (define metadata (get-instruction-metadata ver))
  ;; Use 'data mode to get raw instruction lists from workers
  (define all-insts (parallel-disassemble-ordered filename 'data))
  (for/list ([insts all-insts] [f-idx (in-naturals)])
    (process-function-insts hbc insts metadata f-idx ver)))

(define (process-function-insts hbc insts metadata f-idx ver)
  (hash 'function_index f-idx
        'instructions
        (for/list ([paired-inst insts] [idx (in-naturals)])
          (define inst (cdr paired-inst))
          (instruction->hash hbc paired-inst (second inst) metadata idx 0))))

;; High-level API to get "HASM" (Hermes Assembly) format for a single function.
;; Returns: (list (offset . (Mnemonic Opcode Op1 Op2 ...)) ...)
(define (get-function-hasm hbc f-idx)
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (define metadata (get-instruction-metadata ver))
  (define insts (get-instructions-for-function hbc f-idx))
  (for/list ([paired-inst insts])
    (define offset (car paired-inst))
    (define inst (cdr paired-inst))
    (define-values (mnemonic resolved-args)
      (resolve-instruction hbc paired-inst (second inst) metadata))
    (cons offset (cons mnemonic resolved-args))))
