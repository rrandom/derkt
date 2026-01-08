#lang racket

(require "../common/decode.rkt")
(require "../model/header.rkt")
(require "../model/hbc.rkt")
(require "../model/function-header.rkt")
(require "../common/isa-manager.rkt")

(provide (all-defined-out))

(define (get-instructions-for-function hbc func-id [existing-port #f])
  (define fh (vector-ref (HBCFile-function-headers hbc) func-id))
  (define ver (HbcHeader-version (HBCFile-header hbc)))
  (define decoder (get-decoder ver))
  (if existing-port
      (disassemble-function existing-port fh decoder)
      (call-with-input-file (HBCFile-source-path hbc) #:mode 'binary
        (lambda (port) (disassemble-function port fh decoder)))))

(define (read-DebugInfoOffsets port ver)
  (align-reader port 4)
  (if (>= ver 91) (read-DebugInfoOffsetsNew port) (read-DebugInfoOffsetsOld port)))

(define (read-exception-handlers port ver count)
  (for/list ([i (in-range count)]) (read-ExceptionHandlerInfo port)))

(define (read-function-header port ver)
  (define init-pos (file-position port))
  (define sfh (read-SmallFunctionHeader port ver))
  (define anchor-pos (+ init-pos 16))

  (if (not (FunctionHeaderFlag-overflowed (SmallFunctionHeader-flags sfh)))
      (let* ([flags (SmallFunctionHeader-flags sfh)]
             [fh (if (or (FunctionHeaderFlag-has-exception-handler flags) (FunctionHeaderFlag-has-debug-info flags))
                     (begin
                       (file-position port (SmallFunctionHeader-info-offset sfh))
                       (let* ([handlers (if (FunctionHeaderFlag-has-exception-handler flags)
                                            (begin (align-reader port 4) (read-exception-handlers port ver (decode-u32 port)))
                                            '())]
                              [dbg-off (if (FunctionHeaderFlag-has-debug-info flags) (read-DebugInfoOffsets port ver) #f)])
                         (struct-copy SmallFunctionHeader sfh [exception-handlers handlers] [debug-info dbg-off])))
                     sfh)])
        (file-position port anchor-pos)
        fh)
      (let* ([info-off (SmallFunctionHeader-info-offset sfh)]
             [hdr-off (SmallFunctionHeader-offset sfh)]
             [new-off (bitwise-ior (arithmetic-shift info-off 16) (bitwise-and hdr-off #xffff))])
        (file-position port new-off)
        (let* ([lfh (read-LargeFunctionHeader port ver)]
               [flags (LargeFunctionHeader-flags lfh)]
               [fh (let* ([handlers (if (FunctionHeaderFlag-has-exception-handler flags)
                                        (begin (align-reader port 4) (read-exception-handlers port ver (decode-u32 port)))
                                        '())]
                          [dbg-off (if (FunctionHeaderFlag-has-debug-info flags) (read-DebugInfoOffsets port ver) #f)])
                     (struct-copy LargeFunctionHeader lfh [exception-handlers handlers] [debug-info dbg-off]))])
          (file-position port anchor-pos)
          fh))))

(define (disassemble-bytecode port size decoder)
  (define start-pos (file-position port))
  (define end-pos (+ start-pos size))
  (let loop ([insts '()])
    (define curr-pos (file-position port))
    (if (< curr-pos end-pos)
        (let ([inst (decoder port)])
          (if (eof-object? inst)
              (reverse insts)
              (loop (cons (cons (- curr-pos start-pos) inst) insts))))
        (reverse insts))))

(define (disassemble-function port fh decoder)
  (file-position port (function-header-offset fh))
  (disassemble-bytecode port (function-header-byte-size fh) decoder))
