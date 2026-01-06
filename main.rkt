#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "parser/tables.rkt")
(require "common/isa-manager.rkt")
(require "model/header.rkt")
(require "model/hbc.rkt")
(require "model/function-header.rkt")
(require "parser/functions.rkt")

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

        (define ver (HbcHeader-version (HBCFile-header hbc)))
        (for ([fh (HBCFile-function-headers hbc)]
              [idx (in-naturals)])
          (printf "Function #~a:\n" idx)
          (printf "  Offset: 0x~x\n" (function-header-offset fh))
          (define insts (get-instructions-for-function hbc idx))
          (for ([inst insts])
            (display "    ")
            (print-instruction hbc inst (current-output-port) ver))
          (newline)))
      (error "File not found:" filename)))
