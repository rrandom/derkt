#lang racket

(require (for-syntax racket/base syntax/parse racket/syntax "decode.rkt" racket/string))
(require "decode.rkt" "../model/hbc.rkt")

(provide (all-defined-out))

(define-syntax (define-hbc-isa stx)
  (syntax-parse stx
    [(_ [opcode:integer mnemonic:id (op-type:id ...)] ...)
     (define decode-id (datum->syntax stx 'decode-instruction))
     (define metadata-id (datum->syntax stx 'instruction-metadata))

     (define struct-defs
       (for/list ([name (syntax->list #'(mnemonic ...))]
                  [types-stx (syntax->list #'((op-type ...) ...))])
         (define s-name (format-id stx "Inst-~a" name))
         (define fields (for/list ([i (in-range (length (syntax->list types-stx)))]) (format-id stx "op~a" i)))
         #`(struct #,s-name (op-code #,@fields) #:transparent)))

     (define cases
       (for/list ([code (syntax->list #'(opcode ...))]
                  [name (syntax->list #'(mnemonic ...))]
                  [types-stx (syntax->list #'((op-type ...) ...))])
         (define s-name (format-id stx "Inst-~a" name))
         (define readers (for/list ([t (syntax->list types-stx)])
                           (with-syntax ([reader (get-reader-id stx (syntax-e t))]) #'(reader port))))
         #`[(#,code) (#,s-name #,code #,@readers)]))

     (define metadata-entries
       (for/list ([code (syntax->list #'(opcode ...))]
                  [name (syntax->list #'(mnemonic ...))]
                  [types-stx (syntax->list #'((op-type ...) ...))])
         (with-syntax ([c code] [n name] [(t ...) types-stx])
           #'(cons c (list 'n (list 't ...))))))

     #`(begin
         #,@struct-defs
         (define #,metadata-id
           (make-immutable-hash (list #,@metadata-entries)))

         (define (#,decode-id port)
           (define op (read-byte port))
           (cond [(eof-object? op) eof]
                 [else (case op #,@cases [else (error 'decode "Unknown Opcode: 0x~x" op)])])))]))
