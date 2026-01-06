#lang racket

(require racket/place
         racket/port
         "core.rkt"
         "functions.rkt"
         "resolver.rkt"
         "../model/hbc.rkt"
         "../model/header.rkt")

(provide disassemble-range-worker
         parallel-disassemble-ordered)

;; Worker logic for a range of functions
(define (disassemble-range-worker p)
  (define args (place-channel-get p))
  (match-define (list hbc-path start-idx end-idx ver metadata-hash mode) args)

  (define metadata-vec (make-vector 256 #f))
  (for ([(op info) metadata-hash]) (vector-set! metadata-vec op info))

  (define hbc (parse-hbc-file hbc-path))

  (call-with-input-file hbc-path #:mode 'binary
    (lambda (in)
      (for ([f-idx (in-range start-idx end-idx)])
        (define insts (get-instructions-for-function hbc f-idx in))
        (match mode
          ['text
           (define buf (open-output-string))
           (for ([inst insts]) (print-instruction hbc inst buf metadata-vec))
           (place-channel-put p (list 'result f-idx (get-output-string buf)))]
          ['data
           (place-channel-put p (list 'result f-idx insts))]))))

  (place-channel-put p 'done))

;; Generic parallel dispatcher that maintains function order
(define (parallel-disassemble-ordered filename mode)
  (define hbc (parse-hbc-file filename))
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (define metadata-hash (get-instruction-metadata ver))
  (define func-count (HbcHeader-function-count (HBCFile-header hbc)))

  (define num-workers (processor-count))
  (define chunk-size (quotient func-count num-workers))

  (define workers
    (for/list ([i (in-range num-workers)])
      (define start-idx (* i chunk-size))
      (define end-idx (if (= i (sub1 num-workers)) func-count (+ start-idx chunk-size)))

      (define p (place p (disassemble-range-worker p)))
      (place-channel-put p (list filename start-idx end-idx ver metadata-hash mode))
      p))

  ;; Collect results and store them in a vector to maintain order
  (define final-results (make-vector func-count #f))
  (define active-workers num-workers)

  (let loop ()
    (unless (zero? active-workers)
      (apply sync
             (for/list ([w workers])
               (handle-evt w
                           (lambda (msg)
                             (match msg
                               [(list 'result idx data)
                                (vector-set! final-results idx data)
                                (loop)]
                               ['done
                                (set! active-workers (sub1 active-workers))
                                (loop)]))))) ))

  final-results)
