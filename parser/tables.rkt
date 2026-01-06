#lang racket

(require "../common/decode.rkt")
(require "../model/header.rkt")
(require "../model/hbc.rkt")
(require "../model/function-header.rkt")

(provide (all-defined-out))

;; =============================================================================
;; Macros for repetitive table reading
;; =============================================================================

(define-syntax-rule (define-buffer-readers [name size-attr] ...)
  (begin
    (define (name port header)
      (align-reader port 4)
      (decode-bytes port (size-attr header)))
    ...))

(define-syntax-rule (define-list-readers [name count-attr reader] ...)
  (begin
    (define (name port header)
      (align-reader port 4)
      (for/list ([i (in-range (count-attr header))]) (reader port)))
    ...))

;; =============================================================================
;; String and Identifier Tables
;; =============================================================================

(define (read-string-kinds port header)
  (align-reader port 4)
  (define ver (HbcHeader-version header))
  (for/list ([i (in-range (HbcHeader-string-kind-count header))])
    (if (>= ver 72) (read-StringKindEntryNew port ver) (read-StringKindEntryOld port ver))))

(define (read-small-string-table port header)
  (for/list ([i (in-range (HbcHeader-string-count header))])
    (align-reader port 4)
    (read-SmallStringTableEntry port (HbcHeader-version header))))

(define-list-readers
  [read-identifier-hashes HbcHeader-identifier-count decode-u32]
  [read-overflow-string-table HbcHeader-overflow-string-count read-OverflowStringTableEntry]
  [read-big-int-table HbcHeader-big-int-count read-BigIntTableEntry]
  [read-reg-exp-table HbcHeader-reg-exp-count read-RegExpTableEntry]
  [read-function-source-table HbcHeader-function-source-count read-FunctionSourceEntry])

;; =============================================================================
;; Storage and Buffers
;; =============================================================================

(define-buffer-readers
  [read-string-storage HbcHeader-string-storage-size]
  [read-array-buffer HbcHeader-array-buffer-size]
  [read-object-key-buffer HbcHeader-obj-key-buffer-size]
  [read-object-value-buffer HbcHeader-obj-value-buffer-size]
  [read-reg-exp-storage HbcHeader-reg-exp-storage-size]
  [read-big-int-storage HbcHeader-big-int-storage-size])

;; =============================================================================
;; Specialty Tables (Exceptions)
;; =============================================================================

(define (read-cjs-module-table port header)
  (align-reader port 4)
  (define ver (HbcHeader-version header))
  (when (> (HbcHeader-cjs-module-count header) 0)
    (for/list ([i (in-range (HbcHeader-cjs-module-count header))])
      (if (and (BytecodeOptions-cjs-modules-statically-resolved (HbcHeader-options header)) (< ver 77))
          (read-CJSModuleInt port)
          (read-CJSModuleEntry port)))))
