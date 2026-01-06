#lang racket

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(provide (all-defined-out)
         (for-syntax get-reader-id))

;; =============================================================================
;; Basic Binary Decoding Functions
;; =============================================================================

(define-syntax-rule (define-uint-decoders [name n] ...)
  (begin
    (define (name port)
      (for/fold ([res 0]) ([i (in-range n)])
        (let ([b (read-byte port)])
          (if (eof-object? b) (error "Unexpected EOF")
              (bitwise-ior res (arithmetic-shift b (* i 8)))))))
    ...))

(define-uint-decoders [decode-u8 1] [decode-u16 2] [decode-u32 4] [decode-u64 8])

(define (decode-double port)
  (define bs (read-bytes 8 port))
  (if (or (eof-object? bs) (< (bytes-length bs) 8))
      (error "Unexpected EOF while reading double")
      (floating-point-bytes->real bs #f)))

(define (decode-bytes port count)
  (define bs (read-bytes count port))
  (if (or (eof-object? bs) (< (bytes-length bs) count))
      (error (format "Unexpected EOF while reading ~a bytes" count))
      bs))

(define (read-bitfield bs start-bit bit-count)
  (let loop ([value 0] [written-bits 0] [bit-idx start-bit])
    (if (>= written-bits bit-count)
        value
        (let* ([byte-idx (quotient bit-idx 8)]
               [bits-in-current-byte (- 8 (modulo bit-idx 8))]
               [bits-to-read (min bits-in-current-byte (- bit-count written-bits))]
               [mask (sub1 (arithmetic-shift 1 bits-to-read))]
               [shift (modulo bit-idx 8)]
               [byte-val (bytes-ref bs byte-idx)]
               [shifted-byte (arithmetic-shift byte-val (- shift))]
               [extracted (bitwise-and shifted-byte mask)]
               [new-val (bitwise-ior value (arithmetic-shift extracted written-bits))])
          (loop new-val (+ written-bits bits-to-read) (+ bit-idx bits-to-read))))))

;; =============================================================================
;; Compile-time Helpers
;; =============================================================================

(begin-for-syntax
  (define (get-reader-id stx type-sym)
    (case type-sym
      ['Reg8  #'decode-u8]
      ['Reg32 #'decode-u32]
      ['UInt8 #'decode-u8]
      ['UInt16 #'decode-u16]
      ['UInt32 #'decode-u32]
      ['UInt64 #'decode-u64]
      ['Addr8  #'decode-u8]
      ['Addr32 #'decode-u32]
      ['Double #'decode-double]
      ['StringIDUInt8    #'decode-u8]
      ['StringIDUInt16   #'decode-u16]
      ['StringIDUInt32   #'decode-u32]
      ['FunctionIDUInt8  #'decode-u8]
      ['FunctionIDUInt16 #'decode-u16]
      ['FunctionIDUInt32 #'decode-u32]
      ['BigIntIDUInt16   #'decode-u16]
      ['BigIntIDUInt32   #'decode-u32]
      ['Imm32 #'decode-u32]
      [else (raise-syntax-error 'get-reader-id "Unknown operand type" stx type-sym)])))

;; =============================================================================
;; Meta-programming Macros
;; =============================================================================

(define-syntax (define-binary-struct stx)
  (syntax-parse stx
    [(_ name:id [field:id type:id] ...)
     (define readers
       (for/list ([t (in-list (syntax->list #'(type ...)))])
         (with-syntax ([reader (get-reader-id stx (syntax-e t))])
           #'(reader port))))
     #`(begin
         (struct name (field ...) #:transparent)
         (define (#,(format-id stx "read-~a" #'name) port)
           (name #,@readers))
         (provide (struct-out name)
                  #,(format-id stx "read-~a" #'name)))]))

(define-syntax (define-bitfield-struct stx)
  (syntax-parse stx
    [(_ name:id size-in-bytes:expr [field:id start-bit:expr bit-count:expr] ...)
     #`(begin
         (struct name (field ...) #:transparent)
         (define (#,(format-id stx "read-~a" #'name) port version)
           (define bs (decode-bytes port size-in-bytes))
           (name
            #,@(for/list ([field-id (syntax->list #'(field ...))]
                          [start (syntax->list #'(start-bit ...))]
                          [count (syntax->list #'(bit-count ...))])
                 #`(read-bitfield bs #,start #,count))))
         (provide (struct-out name)
                  #,(format-id stx "read-~a" #'name)))]))
