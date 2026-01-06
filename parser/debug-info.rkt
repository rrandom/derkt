#lang racket

(require "../common/decode.rkt")
(require "../model/header.rkt")
(require "../model/hbc.rkt")

(provide (all-defined-out))

(define (read-DebugInfoHeader port ver)
  (define f-count (decode-u32 port))
  (define fs-size (decode-u32 port))
  (define fr-count (decode-u32 port))
  (define sd-off (decode-u32 port))
  (define tc-off (if (>= ver 91) (decode-u32 port) #f))
  (define st-off (if (>= ver 91) (decode-u32 port) #f))
  (define d-size (decode-u32 port))
  (DebugInfoHeader f-count fs-size fr-count sd-off tc-off st-off d-size))

(define (read-debug-info port header)
  (define off (HbcHeader-debug-info-offset header))
  (if (> off 0)
      (begin
        (file-position port off)
        (let* ([ver (HbcHeader-version header)]
               [dh (read-DebugInfoHeader port ver)]
               [st (for/list ([i (in-range (DebugInfoHeader-filename-count dh))]) (read-DebugStringTable port))]
               [ss (decode-bytes port (DebugInfoHeader-filename-storage-size dh))]
               [fr (for/list ([i (in-range (DebugInfoHeader-file-region-count dh))]) (read-DebugFileRegion port))]
               [sda-size (DebugInfoHeader-scope-desc-data-offset dh)]
               [sda-storage (decode-bytes port sda-size)])
          (let-values ([(sdd-storage tc-storage st-storage)
                        (if (and (>= ver 91) (DebugInfoHeader-textified-callee-offset dh) (DebugInfoHeader-string-table-offset dh))
                            (let* ([to (DebugInfoHeader-textified-callee-offset dh)]
                                   [so (DebugInfoHeader-string-table-offset dh)]
                                   [sdd-s (- to (DebugInfoHeader-scope-desc-data-offset dh))]
                                   [tc-s (- so to)]
                                   [st-s (- (DebugInfoHeader-debug-data-size dh) so)])
                              (values (decode-bytes port sdd-s) (decode-bytes port tc-s) (decode-bytes port st-s)))
                            (let ([sdd-s (- (DebugInfoHeader-debug-data-size dh) (DebugInfoHeader-scope-desc-data-offset dh))])
                              (values (decode-bytes port sdd-s) #"" #"")))])
            (DebugInfo dh st ss fr sda-storage sdd-storage tc-storage st-storage))))
      #f))
