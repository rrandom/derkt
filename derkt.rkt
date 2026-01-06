#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "parser/functions.rkt")
(require "model/hbc.rkt")
(require "model/header.rkt")

(provide (all-from-out "parser/core.rkt")
         (all-from-out "parser/resolver.rkt")
         (all-from-out "parser/functions.rkt")
         (all-from-out "model/hbc.rkt")
         (all-from-out "model/header.rkt")
         hbc->json-serializable)

;; Converts the entire HBC file disassembly into a JSON-serializable structure.
(define (hbc->json-serializable hbc)
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (for/list ([f-idx (in-range (vector-length (HBCFile-function-headers hbc)))])
    (define insts (get-instructions-for-function hbc f-idx))
    (hash 'function_index f-idx
          'instructions
          (let loop ([is insts] [idx 0] [offset 0] [acc '()])
            (if (null? is)
                (reverse acc)
                (let* ([inst (first is)]
                       [opcode (vector-ref (struct->vector inst) 1)]
                       [h (instruction->hash hbc inst opcode ver idx offset)])
                  (loop (rest is) (+ idx 1) (+ offset 1) (cons h acc))))))))
