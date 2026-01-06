#lang racket

(require rackunit
         "../common/decode.rkt")

(test-case "decode-u8"
  (check-equal? (decode-u8 (open-input-bytes (bytes #x42))) #x42))

(test-case "decode-u16"
  (check-equal? (decode-u16 (open-input-bytes (bytes #x01 #x02))) #x0201))

(test-case "decode-u32"
  (check-equal? (decode-u32 (open-input-bytes (bytes #x01 #x02 #x03 #x04))) #x04030201))

(test-case "decode-u64"
  (check-equal? (decode-u64 (open-input-bytes (bytes #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))) 
                #x0807060504030201))

(test-case "read-bitfield"
  (define bs (bytes #b10110011)) ; #xB3
  (check-equal? (read-bitfield bs 0 2) #b11)
  (check-equal? (read-bitfield bs 2 3) #b100)
  (check-equal? (read-bitfield bs 5 3) #b101))
