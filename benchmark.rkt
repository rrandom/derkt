#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "common/isa-manager.rkt")
(require "model/hbc.rkt")
(require "model/header.rkt")
(require "parser/functions.rkt")
(require racket/port)

(define (run-benchmark filename)
  (printf "Starting benchmark for ~a\n" filename)

  (define start-total (current-milliseconds))

  ;; Phase 1: Parsing (Metadata Only)
  (define t1-start (current-milliseconds))
  (define hbc (parse-hbc-file filename))
  (define t1-end (current-milliseconds))
  (printf "Phase 1 (Parse Metadata): ~a ms, Memory: ~a MB\n"
          (- t1-end t1-start)
          (quotient (current-memory-use) (* 1024 1024)))

  ;; Phase 2: Resolving Instructions (Lazy + Port Reuse + No Output)
  (define t2-start (current-milliseconds))
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (call-with-input-file (HBCFile-source-path hbc) #:mode 'binary
    (lambda (in)
      (for ([f-idx (in-range (vector-length (HBCFile-function-headers hbc)))])
        (define insts (get-instructions-for-function hbc f-idx in))
        (for ([inst insts])
          (let ([opcode (second inst)])
            (resolve-instruction hbc inst opcode ver))))))
  (define t2-end (current-milliseconds))
  (printf "Phase 2 (Lazy + S-Exp + Port Reuse - No I/O): ~a ms, Memory: ~a MB\n"
          (- t2-end t2-start)
          (quotient (current-memory-use) (* 1024 1024)))

  ;; Phase 3: Optimized Resolution + Printing (Lazy + S-Exp + Port Reuse + Centralized Printer)
  (define t3-start (current-milliseconds))
  (define out (open-output-nowhere))
  (call-with-input-file (HBCFile-source-path hbc) #:mode 'binary
    (lambda (in)
      (for ([f-idx (in-range (vector-length (HBCFile-function-headers hbc)))])
        (define insts (get-instructions-for-function hbc f-idx in))
        (for ([inst insts])
          (print-instruction hbc inst out ver)))))
  (define t3-end (current-milliseconds))
  (printf "Phase 3 (Lazy + Port Reuse + Centralized Printer): ~a ms, Memory: ~a MB\n"
          (- t3-end t3-start)
          (quotient (current-memory-use) (* 1024 1024)))

  (printf "Total benchmark time: ~a ms, Final Memory: ~a MB\n"
          (- (current-milliseconds) start-total)
          (quotient (current-memory-use) (* 1024 1024))))

(module+ main
  (if (> (vector-length (current-command-line-arguments)) 0)
      (run-benchmark (vector-ref (current-command-line-arguments) 0))
      (printf "Usage: racket benchmark.rkt <hbc_file>\n")))
