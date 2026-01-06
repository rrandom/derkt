#lang racket

(require "../common/decode.rkt")

(provide (all-defined-out))

;; =============================================================================
;; Bytecode Options (1 byte bitfield)
;; =============================================================================

(struct BytecodeOptions (static-builtins
                         cjs-modules-statically-resolved
                         has-async
                         flags)
  #:transparent)

(define (read-BytecodeOptions port)
  (define b (decode-u8 port))
  (BytecodeOptions
   (bitwise-bit-set? b 0)
   (bitwise-bit-set? b 1)
   (bitwise-bit-set? b 2)
   (bitwise-bit-set? b 3)))

;; =============================================================================
;; HBC Header
;; =============================================================================

(struct HbcHeader (magic
                   version
                   sha1
                   file-length
                   global-code-index
                   function-count
                   string-kind-count
                   identifier-count
                   string-count
                   overflow-string-count
                   string-storage-size
                   big-int-count
                   big-int-storage-size
                   reg-exp-count
                   reg-exp-storage-size
                   array-buffer-size
                   obj-key-buffer-size
                   obj-value-buffer-size
                   segment-id
                   cjs-module-count
                   cjs-module-offset
                   function-source-count
                   debug-info-offset
                   options)
  #:transparent)

(define (align-reader port alignment)
  (define pos (file-position port))
  (define rem (modulo pos alignment))
  (when (and (> rem 0) (< rem alignment))
    (read-bytes (- alignment rem) port)))

(define (read-HbcHeader port)
  (define magic (decode-u64 port))
  (define ver (decode-u32 port))
  (define sha1 (decode-bytes port 20))
  (define file-len (decode-u32 port))
  (define g-idx (decode-u32 port))
  (define f-count (decode-u32 port))
  (define sk-count (decode-u32 port))
  (define id-count (decode-u32 port))
  (define s-count (decode-u32 port))
  (define os-count (decode-u32 port))
  (define ss-size (decode-u32 port))
  
  (define bi-count (if (>= ver 87) (decode-u32 port) 0))
  (define bis-size (if (>= ver 87) (decode-u32 port) 0))
  
  (define re-count (decode-u32 port))
  (define res-size (decode-u32 port))
  (define ab-size (decode-u32 port))
  (define okb-size (decode-u32 port))
  (define ovb-size (decode-u32 port))
  
  (define-values (cjs-m-off seg-id)
    (if (< ver 78) (values (decode-u32 port) 0) (values 0 (decode-u32 port))))
    
  (define cjs-m-count (decode-u32 port))
  (define fs-count (if (>= ver 84) (decode-u32 port) 0))
  (define di-off (decode-u32 port))
  (define opts (read-BytecodeOptions port))

  (align-reader port 32)

  (HbcHeader magic ver sha1 file-len g-idx f-count sk-count id-count s-count os-count ss-size 
             bi-count bis-size re-count res-size ab-size okb-size ovb-size seg-id 
             cjs-m-count cjs-m-off fs-count di-off opts))
