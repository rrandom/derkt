#lang racket

(provide (all-defined-out))

(struct function-info
  (fid              ; Function ID (index)
   name             ; Function Name (string)
   param-count      ; Parameter count
   register-count   ; Register count (derived from frame-size/env)
   frame-size)      ; Frame size
  #:transparent)

(struct fundef-with-info
  (info               ; function-info struct
   ir                 ; List of instructions (HASM format)
   exception-handlers) ; List of ExceptionHandlerInfo (start, end, target)
  #:transparent)

