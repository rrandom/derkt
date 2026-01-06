#lang racket

(provide get-decoder get-metadata)

(require racket/runtime-path)
(define-runtime-path isa-dir "../isa")

(define (get-isa-path ver)
  (define base-path (build-path isa-dir (format "hermes-instructions-v~a.rkt" ver)))
  (if (file-exists? base-path)
      base-path
      (build-path isa-dir "hermes-instructions-v96.rkt")))

(define (get-decoder ver)
  (dynamic-require (get-isa-path ver) 'decode-instruction))

(define (get-metadata ver)
  (dynamic-require (get-isa-path ver) 'instruction-metadata))
