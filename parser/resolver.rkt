#lang racket

(require racket/runtime-path)
(require "../common/isa-manager.rkt")
(require "../model/hbc.rkt")
(require "tables.rkt")

(define-runtime-path isa-dir "../isa")

(provide resolve-instruction
         instruction->hash)

;; Resolves a raw instruction into a more human-readable format or structured data.
(define (resolve-instruction hbc inst opcode ver)
  (define metadata (get-instruction-metadata ver))
  (define info (hash-ref metadata opcode #f))
  (unless info (error "No metadata for opcode:" opcode))

  (define mnemonic (first info))
  (define arg-types (second info))

  (define resolved-args
    (for/list ([arg (drop (vector->list (struct->vector inst)) 2)]
               [type arg-types])
      (cond
        [(string-prefix? (symbol->string type) "StringID")
         (get-hbc-string hbc arg)]
        [else arg])))

  (values mnemonic resolved-args))

;; Converts an instruction to a JSON-compatible hash.
(define (instruction->hash hbc inst opcode ver index offset)
  (let-values ([(name args) (resolve-instruction hbc inst opcode ver)])
    (define metadata (get-instruction-metadata ver))
    (define types (second (hash-ref metadata opcode)))

    (hash 'index index
          'offset (format "~x" offset)
          'offset_decimal offset
          'opcode opcode
          'name (symbol->string name)
          'display (format "~a  ~a" name (string-join (map (lambda (a) (format "~a" a)) args) ", "))
          'operands args
          'operand_types (map symbol->string types))))

;; Private helper to get instruction metadata for a specific version.
(define (get-instruction-metadata ver)
  (dynamic-require (build-path isa-dir (format "hermes-instructions-v~a.rkt" ver))
                   'instruction-metadata))
