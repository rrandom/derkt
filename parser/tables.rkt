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
      (for/vector ([i (in-range (count-attr header))]) (reader port)))
    ...))

;; =============================================================================
;; String and Identifier Tables
;; =============================================================================

(define (read-string-kinds port header)
  (align-reader port 4)
  (define ver (HbcHeader-version header))
  (for/vector ([i (in-range (HbcHeader-string-kind-count header))])
    (if (>= ver 72) (read-StringKindEntryNew port ver) (read-StringKindEntryOld port ver))))

(define (read-small-string-table port header)
  (for/vector ([i (in-range (HbcHeader-string-count header))])
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

;; =============================================================================
;; String Lookup (Optimized with Vectors and Caching)
;; =============================================================================

(define (decode-utf16 bs)
  (with-handlers ([exn:fail? (lambda (e) (bytes->string/utf-8 bs #\?))])
    ;; to-encoding is #f (system default), from-encoding is UTF-16LE
    (let ([p (reencode-input-port (open-input-bytes bs) #f "UTF-16LE" #f #t)])
      (port->string p))))

(define (get-hbc-string hbc id)
  (define cache (HBCFile-string-cache hbc))
  (define cached (vector-ref cache id))
  (if cached
      cached
      (let* ([s-table (HBCFile-small-string-table hbc)]
             [os-table (HBCFile-overflow-string-table hbc)]
             [storage (HBCFile-string-storage hbc)]
             [entry (vector-ref s-table id)]
             [is-utf16? (= (SmallStringTableEntry-is-utf16 entry) 1)]
             [off (SmallStringTableEntry-offset entry)]
             [len (SmallStringTableEntry-length entry)]
             [result (if (= len #xff) ; Overflow
                         (let* ([o-entry (vector-ref os-table off)]
                                [o-off (OverflowStringTableEntry-offset o-entry)]
                                [o-len (OverflowStringTableEntry-length o-entry)]
                                [raw (subbytes storage o-off (+ o-off o-len))])
                           (if is-utf16? (decode-utf16 raw) (bytes->string/utf-8 raw)))
                         (let ([raw (subbytes storage off (+ off len))])
                           (if is-utf16? (decode-utf16 raw) (bytes->string/utf-8 raw))))])
        (vector-set! cache id result)
        result)))
