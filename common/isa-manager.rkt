#lang racket

(provide get-decoder)

(require racket/runtime-path)
(define-runtime-path isa-dir "../isa")

(define (get-decoder ver)
  (define filename (format "hermes-instructions-v~a.rkt" ver))
  (define path (build-path isa-dir filename))
  (if (file-exists? path)
      (dynamic-require path 'decode-instruction)
      (begin
        (printf "Warning: No decoder for v~a, fallback to v96\n" ver)
        (dynamic-require (build-path isa-dir "hermes-instructions-v96.rkt") 'decode-instruction))))
