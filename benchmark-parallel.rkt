#lang racket

(require racket/place
         racket/port
         "parser/core.rkt"
         "parser/functions.rkt"
         "parser/resolver.rkt"
         "model/hbc.rkt"
         "model/header.rkt")

(provide run-parallel-benchmark)

;; Worker Place Logic
(define (disassemble-worker p)
  (define args (place-channel-get p))
  (match-define (list hbc-path start-idx end-idx ver metadata-hash) args)

  ;; Create a direct vector for faster resolution
  (define metadata-vec (make-vector 256 #f))
  (for ([(op info) metadata-hash]) (vector-set! metadata-vec op info))

  ;; Parse enough to get necessary tables (minimal reload)
  (define hbc (parse-hbc-file hbc-path))

  (define out (open-output-nowhere))
  (call-with-input-file hbc-path #:mode 'binary
    (lambda (in)
      (for ([f-idx (in-range start-idx end-idx)])
        (define insts (get-instructions-for-function hbc f-idx in))
        (for ([paired-inst insts])
          (print-instruction hbc paired-inst out metadata-vec)))))

  (place-channel-put p 'done))

(define (run-parallel-benchmark filename)
  (printf "Starting Parallel Benchmark for ~a\n" filename)
  (define start-total (current-milliseconds))

  ;; Phase 1: Main process parses header and metadata
  (define hbc (parse-hbc-file filename))
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (define metadata-hash (get-instruction-metadata ver))
  (define func-count (HbcHeader-function-count (HBCFile-header hbc)))

  (define num-workers (processor-count))
  (printf "Detected ~a logical cores. Spawning workers...\n" num-workers)

  (define chunk-size (quotient func-count num-workers))

  (define workers
    (for/list ([i (in-range num-workers)])
      (define start-idx (* i chunk-size))
      (define end-idx (if (= i (sub1 num-workers)) func-count (+ start-idx chunk-size)))

      (define p (place p (disassemble-worker p)))
      ;; Send tasks to worker
      (place-channel-put p (list filename start-idx end-idx ver metadata-hash))
      p))

  ;; Wait for all workers
  (for ([p workers]) (place-channel-get p))

  (define total-time (- (current-milliseconds) start-total))
  (printf "\nParallel Execution Complete!\n")
  (printf "Total benchmark time: ~a ms (~a seconds)\n" total-time (/ total-time 1000.0)))

(module+ main
  (if (> (vector-length (current-command-line-arguments)) 0)
      (run-parallel-benchmark (vector-ref (current-command-line-arguments) 0))
      (printf "Usage: racket benchmark-parallel.rkt <hbc_file>\n")))
