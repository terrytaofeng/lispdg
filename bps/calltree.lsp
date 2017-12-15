

(define opt-calltree-last-index 1)
;nil 0 -> (1)
;(1) 0 -> (2)
;(1) 1 -> (1 1)
;(1 1) 1 -> (1 2)
;(1 1) 2 -> (1 1 1)
(define (transt-idx-array idx_array new_idx start_idx)
  (if (nil? idx_array)
      (list (if start_idx start_idx opt-calltree-last-index))
      (if (>= new_idx (length idx_array))
        (append idx_array '(1))
        (slice 
          (map (fn (x) 
                  (if (= $idx new_idx) 
                    (+ x 1) x)) idx_array)
          0 (+ new_idx 1))
      )
  )
)

(define (calltree-parse msg start_idx)
  (letn 
      (
       start_idx (if start_idx start_idx opt-calltree-last-index)
       msg1 (parse msg "\n")
       msg1 (clean empty? msg1)
       msg1 (clean (fn (x)(or (= (length (trim x)) 0) (= (first x) "#"))) msg1)
       c nil
       idx_array (map (fn (x) (find (trim x) x)) msg1)
 ;      x (println "idx_array:" idx_array)
       t1 (unique (sort (copy idx_array)))
       t11 (t1 0)
       t12 (if (< (length t1) 2) 1 (- (t1 1) (t1 0)))
       idx_array (map (fn (x) (- x t11)) idx_array)
       idx_array (map (fn (x) (/ x t12)) idx_array)
       a1   (map 
              (fn (x) 
                (letn
                  (t (if (nil? c) nil (last c))
                   t1 (transt-idx-array t x start_idx))
                  (extend c (list t1))
                    t1
                  )
                )
              idx_array)
       a2 (map (fn (x) (join (map string x) ".")) a1)
       ;merge idx_array and conent
       a3 (transpose (list a2 (map trim msg1)))
      )
      (define opt-calltree-last-index (+ start_idx (length (filter zero? idx_array))))
      ;(println "opt:" opt-calltree-last-index)

      a3
     ))

;line :  A->B:text
;call_format  '(":" "|")
(define (callline-parse line call_format)
  (letn
    (match_arrays (map (fn (x) (list (find x line) x)) call_format)
     match_arrays (clean (fn (x) (nil? (x 0))) match_arrays)
     match_first (if match_arrays (first (sort match_arrays)) nil)
     match_idx (if match_first (match_first 0))
     match_char (if match_first (match_first 1))
     header (if match_first (slice line 0 match_idx) line)
     left (if match_first (slice line (+ (length match_char) match_idx)) "")
      )
      (list (trim header) (trim left))))

(define (callline-format sn item attrs)
    (letn
      (a (callline-parse (trim item) '("|" "->" " ->" "-> " "\t"))
       client (a 0)
       left (a 1)
       a1 (callline-parse left '("|" " " "\t"))
       server (a1 0)
       left (a1 1)
        )
      (format {%s -> %s [ label="%s %s" %s ] } client server sn left (if attrs attrs ""))
      )
    )

(define (calltree content attrs start_idx )
  (letn
    ( a (calltree-parse content start_idx)
      b (join (map (fn (x) (callline-format (x 0) (x 1) attrs)) a) "\n")
      )
    b
    )
  )
