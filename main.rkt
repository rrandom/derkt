#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "parser/tables.rkt")
(require "common/isa-manager.rkt")
(require "model/header.rkt")
(require "model/hbc.rkt")
(require "model/function-header.rkt")
(require "parser/functions.rkt")
(require "parser/parallel.rkt")

;; =============================================================================
;; derkt - Hermes Bytecode Disassembler
;; =============================================================================

(module+ main
  (define filename
    (if (> (vector-length (current-command-line-arguments)) 0)
        (vector-ref (current-command-line-arguments) 0)
        "./tests/sample.hbc"))

  (if (file-exists? filename)
      (let ([hbc (parse-hbc-file filename)])
        (printf "\nSuccessfully parsed HBC file: ~a\n" filename)
        (printf "HBC Version: ~a\n" (HbcHeader-version (HBCFile-header hbc)))
        (printf "Number of functions: ~a\n\n" (HbcHeader-function-count (HBCFile-header hbc)))

        (printf "Disassembling ~a functions in parallel...\n"
                (HbcHeader-function-count (HBCFile-header hbc)))

        (define results (parallel-disassemble-ordered filename 'text))

        (for ([text results]
              [fh (HBCFile-function-headers hbc)]
              [idx (in-naturals)])
          (printf "Function #~a:\n" idx)
          (printf "  Offset: 0x~x\n" (function-header-offset fh))
          (display text)
          (newline)))
      (error "File not found:" filename)))
