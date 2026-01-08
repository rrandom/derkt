#lang info

(define collection "derkt")
(define deps '("base"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Hermes Bytecode Disassembler")
(define version "0.1")
(define pkg-authors '("rrandom"))

(define raco-commands
  '(("derkt" derkt/main "disassemble Hermes bytecode (text or hasm)" #f)))
