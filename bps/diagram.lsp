

(define (classtree content)
  (letn (
      lines (parse content "\n")
      lines (clean (fn (x)(or (empty? x) (= (length (trim x)) 0) (= (first x) "#"))) lines) 
      header nil
      parse-line 
        (fn (x)
          (letn 
            (
              a (parse x)
              is-member (= 0 (find "-" (trim x)))
              h (if (> (length a) 0) (first a))
              )
              (list x a is-member h)))
      newlines 
        (map 
          (fn (y) 
            (letn
              (b (parse-line y)
               l (b 0)
               a (b 1)
               is-member (b 2) 
               h (b 3)
               is-extends (if (catch (a 1) 't) t)
              )
              (begin
                (if (not (or is-member (nil? h))) 
                  (set 'header h))
                (if is-member
                    (format {%s -> %s [ headlabel="%s" ]} header (a 1) (a 2))
                    (if is-extends
                      (join 
                        (map (fn (t) (format {%s -> %s [ arrowtail="empty" dir="back"]} t (a 0)))
                          (rest (rest a)))
                        "\n")
                      l
                      )
                  ))
              ))
          lines)

    )
    (join newlines "\n")
  )
)

(define-macro (umlet-classtree token content)
  (letex ((token token)
          (content content))
    (letn (lines (parse content "\n")
           lines (clean (fn (x)(or (empty? x) (= (length (trim x)) 0) (= (first x) "#"))) lines) 
           uml_lines 
            (map 
              (fn (x) 
                (if (find "=>" x)
                  (slice x 0 $it)
                  x
                  )
              ) lines)
            uml_lines (clean (fn (x) (= "-" (get-line-arg x 0))) uml_lines)
            uml_lines
             (if (= "--" (get-line-arg (last uml_lines) 0))
              (slice uml_lines 0 (dec (length uml_lines)))
              uml_lines)
            uml_lines (join uml_lines "\n")
            lines (append (list (first lines)) (filter (fn (x) (= "-" (get-line-arg x 0))) lines))
            lines (join lines "\n")
          )
        (join
          (list  (umlet token uml_lines) (classtree lines) "")
          "\n")
      )))




(define-macro (GRAPH_SHOW_DEFINE fun title content)
  (letex ((s fun)
          (title title)
          (content content))
   (define (s) 
      (fp (REF-DEFINE title))
      (println (N:dot content))
      (delete 'myData)
      )))

(define-macro (GRAPH_SHOW title content)
  (letex ((title title)
          (content content))
   (begin
      (fp (REF-DEFINE title))
      (println (N:dot content))
      (delete 'myData)
      )))

  
(define GRAPH_DEFT_LAYOUT  
  {
    rankdir="LR";
    node [shape=record margin=0.1 width=0.1 height=0.1];
    nodesep=0;
    ranksep="0"
    edge [ arrowhead="vee"]
  }
)

(define (html-table-parse title text) 
        (letn
          (msg (parse text "\n")
           msg (clean (fn (x)(or (empty? x) (= (length (trim x)) 0) (= (first x) "#"))) msg) 
           msg-array (map (fn(x) (parse x)) msg))
          (fp (REF-DEFINE title))
          (println {<table border=1>})
          (map (fn(x)
                   (println {<tr>})
                   (map (fn(y) (println (format {<td>%s</td>} y))) x)
                   (println {</tr>})
                   ) msg-array)
          (println {</table>})
        ))

(define (html-text-file title text)
    (begin 
      (fp (REF-DEFINE title))
      (println (pre text))))


(define (get-line-arg line idx)
  (letn (result nil
         a (if line (parse line))
        )
      (if (catch (a idx) 'result)
          result)
    )
  )




;; TODO move to calltree
(define-macro (gv-attr-color c)
  (letex ((c c))
   (format { color="%s" fontcolor="%s" } (string c) (string c))))

(define (gv-attr-color-rand)
  (letn ((r 200)
        (c (apply + (map (fn (x) (* (+ (- 256 r) x) (pow 256 $idx))) (rand r 3)))))
    (format { color="#%06x" fontcolor="#%06x" } c c)))



