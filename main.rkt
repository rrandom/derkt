#lang racket

(require "parser/core.rkt")
(require "parser/resolver.rkt")
(require "parser/tables.rkt")
(require "model/header.rkt")
(require "model/hbc.rkt")
(require "model/function-header.rkt")

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
              [insts (HBCFile-disassembled-functions hbc)]
              [idx (in-naturals)])
          (printf "Function #~a:\n" idx)
          (printf "  Offset: 0x~x\n" (function-header-offset fh))
          (printf "  Instructions:\n")
          (for ([inst insts])
            (let-values ([(name args) (resolve-instruction hbc inst (vector-ref (struct->vector inst) 1) ver)])
              (printf "    ~a  ~a\n" name (string-join (map ~a args) ", "))))
          (newline)))
      (error "File not found:" filename)))
