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
(require "derkt.rkt")
(require racket/cmdline)

;; =============================================================================
;; derkt - Hermes Bytecode Disassembler
;; =============================================================================

(module+ main
  (define hasm-func-id (make-parameter #f))

  (define filename
    (command-line
     #:program "derkt"
     #:once-each
     [("--hasm") id "Dump HASM format (S-exp) for specific function ID"
                 (hasm-func-id (string->number id))]
     #:args (hbc-file)
     hbc-file))

  (if (file-exists? filename)
      (let ([hbc (parse-hbc-file filename)])
        (cond
          [(hasm-func-id)
           (write (get-function-hasm hbc (hasm-func-id)))
           (newline)]
          [else
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
             (newline))]))
      (error "File not found:" filename)))
