#lang racket

(require (for-syntax racket/base syntax/parse racket/syntax "decode.rkt"))
(require "decode.rkt")

(provide (all-defined-out))

(define-syntax (define-hbc-isa stx)
  (syntax-parse stx
    [(_ [opcode:integer mnemonic:id (op-type:id ...)] ...)
     (define decode-id (datum->syntax stx 'decode-instruction))
     (define struct-defs
       (for/list ([name (syntax-e #'(mnemonic ...))] [types (syntax-e #'((op-type ...) ...))])
         (define s-name (format-id stx "Inst-~a" name))
         (define fields (for/list ([i (in-range (length (syntax->list types)))]) (format-id stx "op~a" i)))
         #`(struct #,s-name #,fields #:transparent)))
     (define cases
       (for/list ([code (syntax-e #'(opcode ...))] [name (syntax-e #'(mnemonic ...))] [types (syntax-e #'((op-type ...) ...))])
         (define s-name (format-id stx "Inst-~a" name))
         (define readers (for/list ([t (syntax->list types)])
                           (with-syntax ([reader (get-reader-id stx (syntax-e t))]) #'(reader port))))
         #`[(#,code) (#,s-name #,@readers)]))
     #`(begin
         #,@struct-defs
         (define (#,decode-id port)
           (define op (read-byte port))
           (cond [(eof-object? op) eof]
                 [else (case op #,@cases [else (error 'decode "Unknown Opcode: 0x~x" op)])])))]))
