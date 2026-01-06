#lang racket

(require "../common/decode.rkt")
(require "../common/isa-manager.rkt")
(require "../model/header.rkt")
(require "../model/hbc.rkt")
(require "tables.rkt")
(require "functions.rkt")
(require "debug-info.rkt")

(provide parse-hbc-file)

(define HBC_MAGIC #x1f1903c103bc1fc6)

(define (parse-hbc-file filename)
  (define port (open-input-file filename #:mode 'binary))
  (define header (read-HbcHeader port))

  (unless (= (HbcHeader-magic header) HBC_MAGIC)
    (error "Invalid Hermes Magic Number"))

  (define ver (HbcHeader-version header))
  (define f-headers (for/list ([i (in-range (HbcHeader-function-count header))])
                      (read-function-header port ver)))

  (define s-kinds (read-string-kinds port header))
  (define id-hashes (read-identifier-hashes port header))
  (define s-table (read-small-string-table port header))
  (define os-table (read-overflow-string-table port header))
  (define s-storage (read-string-storage port header))
  (define a-buffer (read-array-buffer port header))
  (define ok-buffer (read-object-key-buffer port header))
  (define ov-buffer (read-object-value-buffer port header))

  (define bi-table (read-big-int-table port header))
  (define bi-storage (read-big-int-storage port header))
  (define re-table (read-reg-exp-table port header))
  (define re-storage (read-reg-exp-storage port header))
  (define cjs-mods (read-cjs-module-table port header))
  (define fs-entries (read-function-source-table port header))

  (define d-info (read-debug-info port header))

  (define decoder (get-decoder ver))
  (define d-functions (for/list ([fh f-headers]) (disassemble-function port fh decoder)))

  (define footer (decode-bytes port 20))

  (close-input-port port)

  (define s-cache (make-vector (vector-length s-table) #f))

  (HBCFile header f-headers d-functions s-kinds id-hashes s-table s-storage
           os-table a-buffer ok-buffer ov-buffer bi-table bi-storage re-table re-storage
           cjs-mods fs-entries d-info s-cache footer))
