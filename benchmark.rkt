#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "common/isa-manager.rkt")
(require "model/hbc.rkt")
(require "model/header.rkt")
(require racket/port)

(define (run-benchmark filename)
  (printf "Starting benchmark for ~a\n" filename)

  (define start-total (current-milliseconds))

  ;; Phase 1: Parsing and Disassembly (Raw)
  (define t1-start (current-milliseconds))
  (define hbc (parse-hbc-file filename))
  (define t1-end (current-milliseconds))
  (printf "Phase 1 (Parse & Raw Disassembly): ~a ms\n" (- t1-end t1-start))

  ;; Phase 2: Resolving Instructions (No Output)
  (define t2-start (current-milliseconds))
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (for ([insts (HBCFile-disassembled-functions hbc)])
    (for ([inst insts])
      (let ([opcode (vector-ref (struct->vector inst) 1)])
        (resolve-instruction hbc inst opcode ver))))
  (define t2-end (current-milliseconds))
  (printf "Phase 2 (Resolution - No I/O): ~a ms\n" (- t2-end t2-start))

  ;; Phase 3: Optimized Resolution + Printing (Centralized Printer)
  (define t3-start (current-milliseconds))
  (define out (open-output-nowhere))
  (for ([insts (HBCFile-disassembled-functions hbc)])
    (for ([inst insts])
      (print-instruction hbc inst out ver)))
  (define t3-end (current-milliseconds))
  (printf "Phase 3 (Centralized Printer): ~a ms\n" (- t3-end t3-start))

  (printf "Total benchmark time: ~a ms\n" (- (current-milliseconds) start-total)))

(module+ main
  (if (> (vector-length (current-command-line-arguments)) 0)
      (run-benchmark (vector-ref (current-command-line-arguments) 0))
      (printf "Usage: racket benchmark.rkt <hbc_file>\n")))
