#lang racket

(require "../common/decode.rkt")

(provide (all-defined-out))

;; =============================================================================
;; String Table Entries
;; =============================================================================

(define-bitfield-struct StringKindEntryNew 4 [count 0 31] [kind 31 1])
(define-bitfield-struct StringKindEntryOld 4 [count 0 30] [kind 30 2])
(define-bitfield-struct SmallStringTableEntry 4 [is-utf16 0 1] [offset 1 23] [length 24 8])
(define-binary-struct OverflowStringTableEntry [offset UInt32] [length UInt32])

;; =============================================================================
;; Miscellaneous Tables
;; =============================================================================

(define-binary-struct BigIntTableEntry [offset UInt32] [length UInt32])
(define-binary-struct RegExpTableEntry [offset UInt32] [length UInt32])
(define-binary-struct CJSModuleEntry [symbol-id UInt32] [offset UInt32])
(define-binary-struct CJSModuleInt [value UInt32])
(define-binary-struct FunctionSourceEntry [function-id UInt32] [string-id UInt32])
(define-binary-struct ExceptionHandlerInfo [start UInt32] [end UInt32] [target UInt32])

;; =============================================================================
;; Debug Information Structures
;; =============================================================================

(define-binary-struct DebugInfoOffsetsOld [src UInt32] [scope-desc UInt32])
(define-binary-struct DebugInfoOffsetsNew [src UInt32] [scope-desc UInt32] [callee UInt32])
(define-binary-struct DebugStringTable [offset UInt32] [length UInt32])
(define-binary-struct DebugFileRegion [from-address UInt32] [filename-id UInt32] [source-mapping-url-id UInt32])

(struct DebugInfoHeader (filename-count
                         filename-storage-size
                         file-region-count
                         scope-desc-data-offset
                         textified-callee-offset
                         string-table-offset
                         debug-data-size)
  #:transparent)

(struct DebugInfo (header
                   string-table
                   string-storage
                   file-regions
                   sources-data-storage
                   scope-desc-data-storage
                   textified-callee-storage
                   string-table-storage)
  #:transparent)

;; =============================================================================
;; Main HBC File Structure
;; =============================================================================

(struct HBCFile (header
                 function-headers
                 disassembled-functions
                 string-kinds
                 identifier-hashes
                 small-string-table
                 string-storage
                 overflow-string-table
                 array-buffer-storage
                 object-key-buffer
                 object-value-buffer
                 big-int-table
                 big-int-storage
                 reg-exp-table
                 reg-exp-storage
                 cjs-modules
                 function-source-entries
                 debug-info
                 string-cache
                 footer)
  #:transparent)
