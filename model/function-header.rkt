#lang racket

(require "../common/decode.rkt")
(require "header.rkt")

(provide (all-defined-out))

;; =============================================================================
;; Function Header Flags
;; =============================================================================

(struct FunctionHeaderFlag (prohibit-invoke
                            strict-mode
                            has-exception-handler
                            has-debug-info
                            overflowed)
  #:transparent)

;; =============================================================================
;; Small Function Header
;; =============================================================================

(define-bitfield-struct SmallFunctionHeaderRaw 16
  [offset                      0  25]
  [param-count                25  7]
  [byte-size                  32  15]
  [func-name                  47  17]
  [info-offset                64  25]
  [frame-size                 89  7]
  [env-size                   96  8]
  [highest-read-cache-index   104 8]
  [highest-write-cache-index  112 8]
  [prohibit-invoke-val        120 2]
  [strict-mode-val            122 1]
  [has-exception-handler-val  123 1]
  [has-debug-info-val         124 1]
  [overflowed-val             125 1])

(struct SmallFunctionHeader (offset
                             param-count
                             byte-size
                             func-name
                             info-offset
                             frame-size
                             env-size
                             highest-read-cache-index
                             highest-write-cache-index
                             flags
                             exception-handlers
                             debug-info)
  #:transparent)

(define (read-SmallFunctionHeader port version)
  (define raw (read-SmallFunctionHeaderRaw port version))
  (define pv (SmallFunctionHeaderRaw-prohibit-invoke-val raw))
  (define flags (FunctionHeaderFlag
                 (case pv
                   [(0) 'prohibit-call]
                   [(1) 'prohibit-construct]
                   [(2) 'prohibit-none]
                   [else 
                    (printf "Warning: Unknown prohibit invoke ~a in SmallFunctionHeader\n" pv)
                    'prohibit-none])
                 (= (SmallFunctionHeaderRaw-strict-mode-val raw) 1)
                 (= (SmallFunctionHeaderRaw-has-exception-handler-val raw) 1)
                 (= (SmallFunctionHeaderRaw-has-debug-info-val raw) 1)
                 (= (SmallFunctionHeaderRaw-overflowed-val raw) 1)))

  (SmallFunctionHeader
   (SmallFunctionHeaderRaw-offset raw)
   (SmallFunctionHeaderRaw-param-count raw)
   (SmallFunctionHeaderRaw-byte-size raw)
   (SmallFunctionHeaderRaw-func-name raw)
   (SmallFunctionHeaderRaw-info-offset raw)
   (SmallFunctionHeaderRaw-frame-size raw)
   (SmallFunctionHeaderRaw-env-size raw)
   (SmallFunctionHeaderRaw-highest-read-cache-index raw)
   (SmallFunctionHeaderRaw-highest-write-cache-index raw)
   flags '() #f))

;; =============================================================================
;; Large Function Header
;; =============================================================================

(define-binary-struct LargeFunctionHeaderRaw
  [offset          UInt32]
  [param-count     UInt32]
  [byte-size       UInt32]
  [func-name       UInt32]
  [info-offset     UInt32]
  [frame-size      UInt32]
  [env-size        UInt32]
  [highest-read-cache-index  UInt8]
  [highest-write-cache-index UInt8]
  [flags-byte      UInt8]
  [padding         UInt8])

(struct LargeFunctionHeader (offset
                             param-count
                             byte-size
                             func-name
                             info-offset
                             frame-size
                             env-size
                             highest-read-cache-index
                             highest-write-cache-index
                             flags
                             exception-handlers
                             debug-info)
  #:transparent)

(define (read-LargeFunctionHeader port version)
  (define raw (read-LargeFunctionHeaderRaw port))
  (define fb (LargeFunctionHeaderRaw-flags-byte raw))
  (define pv (bitwise-bit-field fb 0 2))
  (define flags (FunctionHeaderFlag
                 (case pv
                   [(0) 'prohibit-call]
                   [(1) 'prohibit-construct]
                   [(2) 'prohibit-none]
                   [else
                    (printf "Warning: Unknown prohibit invoke ~a in LargeFunctionHeader\n" pv)
                    'prohibit-none])
                 (= (bitwise-bit-field fb 2 3) 1)
                 (= (bitwise-bit-field fb 3 4) 1)
                 (= (bitwise-bit-field fb 4 5) 1)
                 (= (bitwise-bit-field fb 5 6) 1)))

  (LargeFunctionHeader
   (LargeFunctionHeaderRaw-offset raw)
   (LargeFunctionHeaderRaw-param-count raw)
   (LargeFunctionHeaderRaw-byte-size raw)
   (LargeFunctionHeaderRaw-func-name raw)
   (LargeFunctionHeaderRaw-info-offset raw)
   (LargeFunctionHeaderRaw-frame-size raw)
   (LargeFunctionHeaderRaw-env-size raw)
   (LargeFunctionHeaderRaw-highest-read-cache-index raw)
   (LargeFunctionHeaderRaw-highest-write-cache-index raw)
   flags '() #f))

;; =============================================================================
;; Common Function Header Interface
;; =============================================================================

(define (FunctionHeader? x)
  (or (SmallFunctionHeader? x) (LargeFunctionHeader? x)))

(define (function-header-offset fh)
  (if (SmallFunctionHeader? fh) (SmallFunctionHeader-offset fh) (LargeFunctionHeader-offset fh)))

(define (function-header-byte-size fh)
  (if (SmallFunctionHeader? fh) (SmallFunctionHeader-byte-size fh) (LargeFunctionHeader-byte-size fh)))

(define (function-header-param-count fh)
  (if (SmallFunctionHeader? fh) (SmallFunctionHeader-param-count fh) (LargeFunctionHeader-param-count fh)))

(define (function-header-func-name fh)
  (if (SmallFunctionHeader? fh) (SmallFunctionHeader-func-name fh) (LargeFunctionHeader-func-name fh)))
