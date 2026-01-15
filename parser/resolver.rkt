#lang racket

(require racket/runtime-path)
(require "../common/isa-manager.rkt")
(require "../model/hbc.rkt")
(require "tables.rkt")
(require "../common/decode.rkt")

(provide resolve-instruction
         instruction->hash
         print-instruction
         get-instruction-metadata)

;; Pre-calculated strings for registers and small constants (0-255)
(define num-string-cache
  (let ([v (make-vector 256)])
    (for ([i (in-range 256)])
      (vector-set! v i (string->immutable-string (number->string i))))
    v))

(define reg-string-cache
  (let ([v (make-vector 256)])
    (for ([i (in-range 256)])
      (vector-set! v i (string->immutable-string (format "r~a" i))))
    v))

;; Cache for instruction metadata to avoid repeated dynamic-require
(define metadata-cache (make-hash))

(define (get-instruction-metadata ver)
  (hash-ref! metadata-cache ver (lambda () (get-metadata ver))))

;; Optimized resolution for S-expressions
(define (resolve-instruction hbc paired-inst opcode metadata [resolve-args? #t])
  (define inst (cdr paired-inst)) ;; Extract raw instruction from (offset . instr)
  (define info (if (vector? metadata) (vector-ref metadata opcode) (hash-ref metadata opcode #f)))
  (unless info (error "No metadata for opcode:" opcode))

  (define mnemonic (first info))
  (define arg-types (second info))
  (define raw-args (cddr inst))

  (define resolved-args
    (if resolve-args?
        (cond
          [(or (eq? mnemonic 'NewArrayWithBuffer) (eq? mnemonic 'NewArrayWithBufferLong))
           (let ([dest (first raw-args)]
                 [index (second raw-args)]
                 [count (third raw-args)]
                 [offset (fourth raw-args)])
             ;; (printf "Debug: NewArrayWithBuffer offset=~a count=~a\n" offset count)
             (list dest index count (read-buffer-elements hbc (HBCFile-array-buffer-storage hbc) offset count)))]
          [(or (eq? mnemonic 'NewObjectWithBuffer) (eq? mnemonic 'NewObjectWithBufferLong))
           (let ([dest (first raw-args)]
                 [size-hint (second raw-args)]
                 [count (third raw-args)]
                 [key-offset (fourth raw-args)]
                 [val-offset (fifth raw-args)])
             (define keys (read-buffer-elements hbc (HBCFile-object-key-buffer hbc) key-offset count))
             (define vals (read-buffer-elements hbc (HBCFile-object-value-buffer hbc) val-offset count))
             (list dest size-hint count (map list keys vals)))]
          [else
           (for/list ([arg raw-args] [type arg-types])
             (cond
               [(string-prefix? (symbol->string type) "StringID")
                (get-hbc-string hbc arg)]
               [(string-prefix? (symbol->string type) "BigIntID")
                (get-hbc-bigint hbc arg)]
               [else arg]))])
        raw-args))

  (values mnemonic resolved-args))

(define (read-buffer-elements hbc buffer offset count)
  (with-handlers ([exn:fail? (lambda (e) (list (format "Error decoding buffer at ~a: ~a" offset (exn-message e))))])
    (define port (open-input-bytes buffer))
    (file-position port offset)

    (let loop ([elements '()])
      (if (>= (length elements) count)
          (take elements count)
          (let* ([tag-byte (read-byte port)]
                 [tag (bitwise-and (arithmetic-shift tag-byte -4) #b111)]
                 [extended? (not (zero? (bitwise-and tag-byte #b10000000)))]
                 [len (if extended?
                          (bitwise-ior (arithmetic-shift (bitwise-and tag-byte #b1111) 8) (read-byte port))
                          (bitwise-and tag-byte #b1111))]
                 [new-elements
                  (for/list ([i (in-range len)])
                    (case tag
                      [(0) 'null]
                      [(1) #t]
                      [(2) #f]
                      [(3) (decode-double port)] ; Number
                      [(4) (get-hbc-string hbc (decode-u32 port))] ; LongString
                      [(5) (get-hbc-string hbc (decode-u16 port))] ; ShortString
                      [(6) (get-hbc-string hbc (read-byte port))]   ; ByteString
                      [(7) (decode-s32 port)] ; Integer
                      [else (format "UnknownTag:~a" tag)]))])
            (loop (append elements new-elements)))))))

;; High-speed printer for S-expressions
(define (print-instruction hbc paired-inst out metadata)
  (define inst (cdr paired-inst)) ;; Extract raw instruction
  (define opcode (second inst))
  (define info (if (vector? metadata) (vector-ref metadata opcode) (hash-ref metadata opcode #f)))
  (if info
      (let ([mnemonic (first info)]
            [arg-types (second info)]
            [raw-args (cddr inst)])
        (display mnemonic out)
        (unless (null? arg-types)
          (display "  " out)
          (let loop ([args raw-args] [types arg-types] [first? #t])
            (unless (null? args)
              (unless first? (display ", " out))
              (let ([arg (car args)] [type (car types)])
                (cond [(string-prefix? (symbol->string type) "StringID")
                       (display (get-hbc-string hbc arg) out)]
                      [(and (exact-integer? arg) (>= arg 0) (< arg 256))
                       (if (memq type '(Reg8 Reg32))
                           (display (vector-ref reg-string-cache arg) out)
                           (display (vector-ref num-string-cache arg) out))]
                      [else (display arg out)]))
              (loop (cdr args) (cdr types) #f))))
        (newline out))
      (fprintf out "Unknown Instruction (Opcode: ~a)\n" opcode)))

;; Converts an S-expression instruction to a JSON-compatible hash.
(define (instruction->hash hbc paired-inst opcode metadata index _unused_offset)
  (define offset (car paired-inst)) ;; Use the real physical offset from derkt
  (let-values ([(name args) (resolve-instruction hbc paired-inst opcode metadata)])
    (define info (if (vector? metadata) (vector-ref metadata opcode) (hash-ref metadata opcode)))
    (define types (second info))

    (hash 'index index
          'offset (format "~x" offset)
          'offset_decimal offset
          'opcode opcode
          'name (symbol->string name)
          'display (format "~a  ~a" name (string-join (map (lambda (a) (format "~a" a)) args) ", "))
          'operands args
          'operand_types (map symbol->string types))))
