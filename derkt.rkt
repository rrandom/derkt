#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "model/hbc.rkt")
(require "model/header.rkt")

(provide (all-from-out "parser/core.rkt")
         (all-from-out "parser/resolver.rkt")
         (all-from-out "model/hbc.rkt")
         (all-from-out "model/header.rkt")
         hbc->json-serializable)

;; Converts the entire HBC file disassembly into a JSON-serializable structure.
(define (hbc->json-serializable hbc)
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (for/list ([fh (HBCFile-function-headers hbc)]
             [insts (HBCFile-disassembled-functions hbc)]
             [f-idx (in-naturals)])
    (hash 'function_index f-idx
          'offset (HbcHeader-function-count (HBCFile-header hbc)) ; Just a placeholder
          'instructions
          (let loop ([is insts] [offset 0] [acc '()])
            (if (null? is)
                (reverse acc)
                (let* ([inst (first is)]
                       [opcode (vector-ref (struct->vector inst) 1)]
                       [h (instruction->hash hbc inst opcode ver offset offset)]) ; Using offset as index for now
                  (loop (rest is) (+ offset 1) (cons h acc))))))))
