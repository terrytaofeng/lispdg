#!/usr/bin/env newlisp
;; @module node.lsp
;; @description node data description system.

(context 'N)

(constant 'NODE_PREFIX "N")
(constant 'NODE_SN    "_sn")
(constant 'NODE_TYPE  "_type")
(constant 'NODE_NID   "_nid")
(constant 'NODE_PID   "_pid")
(constant 'NODE_LEVEL "_level")
(constant 'NODE_BODY  "_body")

(constant 'NODE_ID   "id")
(constant 'NODE_CHILDREN "children")
(constant 'NODE_TAG      "tag")
(constant 'NODE_LABEL    "label")
(constant 'NODE_DESC     "desc")
(constant 'NODE_TIP      "tip")

(constant 'NODE_TYPE_DEFAULT "node")
(constant 'NODE_TYPE_RELATION "relation")
(constant 'NODE_TYPE_DIGRAPH "gv-digraph")
(constant 'NODE_TYPE_GRAPH   "gv-graph")
(constant 'NODE_TYPE_NODE    "gv-node")

(define NODE_PARAM_IGNORE (list NODE_SN NODE_TYPE NODE_NID NODE_PID NODE_LEVEL NODE_BODY 
                                NODE_CHILDREN NODE_TAG))
(define NODE_SINGLE (list NODE_SN NODE_TYPE NODE_NID NODE_PID NODE_LEVEL 
                          NODE_ID))



(define IDX_SN       0)
(define IDX_TYPE     1)
(define IDX_NODEID   2)
(define IDX_PID      3)
(define IDX_LEVEL    4)
(define IDX_ID       5)
(define IDX_LABEL    6)
(define IDX_DESC     7)
(define IDX_TIP_CNT  8)
(define IDX_CATEGORY 9)



(define HTML_TMP_DIR "/var/www/html/tmp")

(setq node-opt-silence nil)
(setq opt-print-beauty nil)
(setq opt-generate-params-level1 " ")
(setq opt-generate-params-level2 ";")

(define myData:myData)
(define rootid:rootid nil)
(define last_nodeid nil)
(define autoid:autoid 0)

(define (is-single-key? x)
  (find x NODE_SINGLE)
  )



(define (setx-rootid nodeid)
  (if (not rootid:rootid)
      (setq rootid:rootid nodeid)))

;; @syntax (N:has-data)
(define (has-data)
        rootid:rootid)

(define (is-root-gv-node?)
        (letn (type (getx-type rootid:rootid))
              (or (= type NODE_TYPE_GRAPH) (= type NODE_TYPE_DIGRAPH))))

(define (get-rootid)
        rootid:rootid)
        
(define (setx nodeid key value , single)
  (begin
    (setq last_nodeid nodeid)
    (setx-rootid nodeid)
    (setq single (find nodeid '()))
    (setq key (if (symbol? key)
                  (string (term key))
                  (string key)))
    (setq value (if (symbol? value)
                  (string (term value))
                  value))
    (if (not (context 'myData nodeid))
        (context 'myData nodeid '()))
    (if (is-single-key? key)
        (if (assoc key (context 'myData nodeid))
            (setf (lookup key (context 'myData nodeid)) value)
            (push (list key value) (context 'myData nodeid) -1)
            )
        (if (assoc key (context 'myData nodeid))
            (extend (lookup key (context 'myData nodeid)) (list value))
            (push (list key (list value)) (context 'myData nodeid) -1)
            )))
    ; (if (and (= NODE_TYPE_RELATION (getx-type nodeid)) (= key NODE_CHILDREN))
    ;     (fixRelation nodeid value))
    ; (if (= key NODE_TAG)
    ;     (fixTag nodeid value))
  )

(define (getx nodeid key)
  (if (context 'myData nodeid)
      (lookup key (context 'myData nodeid))
      ))

(define (getx-uarray nodeid key)
  (if (getx nodeid key)
      (unique (map string (flat $it)))))

(define (getx-string nodeid key ln)
        (letn ((t (getx nodeid key))
               (ln (if ln ln "")))
              (if t
                  (join (map string (flat t)) ln))))

(define (getx-type nodeid dft)
   (if (getx nodeid NODE_TYPE)
       $it
       (if dft
           NODE_TYPE_DEFAULT)))
       

(define (getx-type-title nodeid dft)
  (replace "^gv-" (getx-type nodeid dft) "" 1))


(define (getx-id nodeid dft)
  (if (getx nodeid NODE_ID)
      $it
      (if dft
          (getx nodeid NODE_NID))))
      
(define (getx-sn nodeid)
  (if (getx nodeid NODE_SN)
      (int $it)
      10000))

      
(define (getx-body nodeid)
  (getx-string nodeid NODE_BODY))

(define (getx-label nodeid dft)
  (if (getx-string nodeid NODE_LABEL)
      $it
      (if dft
          (getx nodeid NODE_NID))))

(define (getx-desc nodeid)
  (getx-string nodeid NODE_DESC "\n"))

(define (getx-children nodeid)
  (getx-uarray nodeid NODE_CHILDREN))

(define (getx-tag nodeid)
  (getx-uarray nodeid NODE_TAG))

(define (getx-tip-count nodeid)
  (if (getx nodeid NODE_TIP)
      (length $it)
      0))

(define (getx-params nodeid)
  (if (context 'myData nodeid)
      (begin
        (letn ((ps (clean (fn (x) (find (first x) NODE_PARAM_IGNORE)) (context 'myData nodeid))))
              (generate-params ps 0)
              ))
      ""))

(define (getx-category nodeid)
  (local (cs)
         (while nodeid
           (setq nodeid (getx nodeid NODE_PID))
           (if nodeid
               (push nodeid cs))
           )
         (if cs
             (join cs "/"))
         ))

(define (printx nodeid)
        (letn (ns (if nodeid
                      (list nodeid)
                      (clean (curry = "myData") (map term (symbols (context 'myData))))))
              (dolist (x ns)
                (println x ":" (replace "\\)" (string (context 'myData x)) "\)\n" 1)))))


(define (is-attr-map? l)
  (or
    (and (list? l)
         (for-all true? (map (fn (x) (and (list? x) (= (length x) 2))) l)))
    (and (quote? l)
         (is-attr-map? (eval l)))))

(define (generate-params var level)
  (cond
    ((or (null? var)
         (> level 2)) "")
    ((is-attr-map? var)
     (if (= 0 level)
         (join
           (map (fn (x) (append (string (x 0)) "=\"" (generate-params (x 1)  (+ level 1)) "\""))
                var)
           opt-generate-params-level1)
           (join (map string (flat var)) opt-generate-params-level2)
         ))
     ((list? var) (join (map string var) opt-generate-params-level2))
     (true (string var))))


(define (auto-id name)
  (format "%s_%d" name (inc autoid:autoid)))

(define (format-node nodeid extra)
        (letn ((type (getx-type nodeid true))
               (level (getx nodeid NODE_LEVEL))
               (body (getx-body nodeid))
               (id (getx-id nodeid true))
               (attr (getx-params nodeid))
               )
              (cond
                ((find type '("gv-digraph" "gv-graph" "gv-subgraph"))
                 (letn (
                        (head (cond ((= type "gv-digraph") "digraph G")
                                    ((= type "gv-graph" "graph G"))
                                    ((= type "gv-subgraph") (format "subgraph cluster_%s" id))
                                    ))
                        (opt-generate-params-level1 "\n")
                        (opt-generate-params-level2 ";")
                        (attr (getx-params nodeid)) 
                        )
                       (if extra
                           (format "%s{\n%s\n%s\n%s\n}\n" head attr body extra)
                           (format "%s{\n%s\n%s\n}\n" head attr body))))
                ((= type "gv-node")
                 (if (null? body)
                     (format "\"%s\" [%s]\n" id attr)
                     (format "\"%s\" [%s label=<\n%s\n>]\n" id attr body)
                     )
                 )
                (true
                  (letn ()
                        (append
                          (if opt-print-beauty (append "\n" (dup " " level)) "")
                          (append "<" (getx-type-title id true) (if (null? attr) ""  " ") attr ">")
                          body
                          (if opt-print-beauty (dup " " level) "")
                          (append "</" (getx-type-title id true) ">")
                          (if opt-print-beauty "\n" "")
                          ))
                  )
                )))


;; @syntax (N:dot {str:dot language})
(define-macro (dot)
              (letn ((result nil)
                     (cmd (format "unset SERVER_NAME;dot -Tsvg <<EOF\n%s\nEOF" 
                            (doargs (arg) (eval arg))))
                     )
                    (if (catch (exec cmd) 'result)
                        (if (list? result)
                            (join result "")
                            (string result))
                        "")))

(define-macro (dot-png)
              (if (not crypto:md5)
                  (module "crypto.lsp"))
              (letn ((result nil)
                     (content (doargs (arg) (eval arg)))
                     (dst (crypto:md5 content))
                     (base HTML_TMP_DIR)
                     (f (format "%s/%s.png" base dst))
                     )
                    (if (not (null? (read-file f 10)))
                        f
                        (letn ((cmd (format "unset SERVER_NAME;dot -Tpng > %s <<EOF\n%s\nEOF" 
                                            f content)))
                              (if (catch (exec cmd) 'result)
                                  f
                                  f)
                              ))))


;; @syntax (N:render nodeid)
(define (render nodeid)
  (letn (nodeid (if nodeid
                    nodeid
                    rootid:rootid))
        (if (and nodeid (context 'myData nodeid))
            (format-node nodeid))))

;; @syntax (N:gv-render nodeid)
(define (gv-render nodeid)
  (letn (nodeid (if nodeid
                    nodeid
                    rootid:rootid))
        (if (and nodeid (context 'myData nodeid))
            (dot (format-node nodeid)))))




;; @syntax (N:N nodeid [attr] [body])
;; @param nodeid : "" nil symbol quote list
;; @param attr : '((attr1 value1)) (list (list attr1 value1) (list attr2 value2)) (list attr value)
;; @return string data
;;
;; @example
;; (N "node" '(("label" "hello")))
(define-macro (N:N _nodeid)
              (letn ((nodeid (eval _nodeid))
                     (ids (cond
                            ((number? nodeid) (list (get-name nodeid)))
                            ((= _nodeid 'true) (list (auto-id NODE_PREFIX)))                            
                            ((null? nodeid) (if (or (symbol? _nodeid))
                                               (letn (i (term _nodeid))
                                                     (if (or (= i "nil") (= i "true"))
                                                         (list (auto-id NODE_PREFIX))
                                                         (list i)))
                                               (list (auto-id NODE_PREFIX))))
                            ((list? nodeid) (list? nodeid) (map string nodeid))
                            (true (list (get-name nodeid)))))
                     (bodys "")
                     (level (if plevel (+ plevel 1) 0))
                     )
                    (dolist (id ids)
                      (setx id NODE_SN (inc autoid:autoid))
                      (setx id NODE_NID id)
                      (setx id NODE_LEVEL level)
                      (setx id NODE_PID pid)
                      (letn ((pid id)
                             (plevel level)
                             (body "")
                             (attr "")
                             )
                            (dolist (arg (args))
                              (letn (arg (eval arg))
                                    (cond
                                      ((and (list? arg) (even? (length arg)) (string? (arg 0))) 
                                       (dolist (kvs (explode arg 2)) (setx id (kvs 0) (kvs 1))))
                                      ((is-attr-map? arg) (dolist (kvs arg) (setx id (kvs 0) (kvs 1))))
                                      (true (extend body (if arg (string arg) ""))))))
                            (if body (setx id NODE_BODY body))
                            (if body
                                (if (not node-opt-silence)
                                    (extend bodys
                                            (format-node id)
                                            )
                                    ))
                            )
                      )
                    bodys
                    ))


;; @syntax (N:N-N symbol|string)
;; @description tag generate.
;; @return string data
;;
;; @example
;; (N-N "gv-digraph" "gv-subgraph" ...)
;; (N-N html body div ...)
;; (N:N-gv-digraph ...)
(define-macro (N-N)
  (dolist (arg (args))
    (letex ((t (string arg))
            (s (sym (append "N-" (string arg)))))
           (define-macro (s)
                         (letn ((r nil)
                                (nodeid (if (catch (first (args)) 'r)
                                            r
                                            ""))
                               (src (if (and (quote? nodeid) (symbol? (eval nodeid)))
                                       (append (list N:N (first (args)) (quote (list (list NODE_TYPE t)))) (rest (args)))
                                       (append (list N:N nil (quote (list (list NODE_TYPE t)))) (args)))))
;                           (println src)
                           (eval src)))))
  )


;; @syntax (N:N-M symbol|string)
;; @description redefine.
;; @example
;; (N:N-M)
;; (N:N-M "html" "Div")
;; (html ...) -> (N:N-html ...)
(define-macro (N-M)
  (if (= 0 (length (args)))
      (N-M-default)
      (dolist (arg (unique (flat (args))))
        (letn ((t   (string (eval arg)))
               (src (sym (append "N-" (lower-case t)) (sym "N")))
               (dst (sym t (sym t))))
              (def-new src dst)))))



(define (get-name x)
  (cond
    ((symbol? x) (term x))
    ((primitive? x) (replace "@[0-9A-F]+$" (string x) "" 1))
    (true (string x))) )

(define (get-token-inner x)
              (cond
                ((null? x) nil)
                ((string? x) x)              
                ((number? x) (string x))              
                ((symbol? x)  (if (eval x)
                                  (get-token-inner $it)
                                  (string (term x))))
                (true (get-token-inner (eval x)))))
                


(define-macro (get-token x)
              (cond
                ((null? x) nil)
                ((string? x) x)              
                ((number? x) (string x))              
                ((symbol? x)  (if (eval x)
                                  (get-token-inner $it)
                                  (string (term x))))
                (true (get-token-inner (eval x)))))


;; @syntax (N:ATTR nodeid attr value)
;; @syntax (N:ATTR nodeid value)
;; @syntax (N:ATTR attr value)
(define-macro (ATTR)
  (letn ((n (length $args)))
        (case n
          (3 (letn ((args0 ($args 0))
                    (args1 ($args 1))
                    (args2 ($args 2))
                    (nodeid (get-token args0))
                    (attr (get-token args1))
                    (value (get-token args2)))
                   (if (and nodeid attr value)
                       (begin
                              (setx nodeid attr value)
                              (setx nodeid NODE_TAG attr)
                         ))))
          (2 (if pid
                 (letn ((args0 ($args 0))
                        (args1 ($args 1))
                        (type (getx-type pid))
                        (tag (get-token args0))
                        (nodeid (if (= type NODE_TYPE_RELATION) tag pid))
                        (attr (if (= type NODE_TYPE_RELATION) pid tag))
                        (value (get-token args1)))
                       (if (and tag value)
                            (if (= type NODE_TYPE_RELATION)
                                (begin
                                  (setx tag pid value)
                                  (setx tag NODE_TAG pid)
                                  )
                                (begin
                                  (setx pid tag value)
                                  (setx pid NODE_TAG tag)     
                                  ))
                            )))))
        nil))

;; @syntax (N:R nodeid node-list1 node-list2...)
(define-macro (R nodeid)
              (letex (nodeid nodeid)
                     (letn (
                            (none (N nodeid (list (list NODE_TYPE NODE_TYPE_RELATION))))
                            (pid nodeid)
                            (plevel (int (getx nodeid NODE_LEVEL)))
                            (args1 (unique (clean null? (map get-name (flat (map eval (args)))))))
                            )
                           (N nodeid (list NODE_TYPE NODE_TYPE_RELATION NODE_CHILDREN args1)))))
                   
;; @syntax (N:T nodeid node-list1 node-list2...)                     
(define-macro (T nodeid)
              (letex (nodeid nodeid)
                     (letn ((nodeid (get-token nodeid))
                            (none (N nodeid))
                            (pid nodeid)
                            (plevel (int (getx nodeid NODE_LEVEL)))
                            (args1 (unique (clean null? (map get-name (flat (map eval (args)))))))
                            )
                           (N nodeid (list NODE_TAG args1)))))



(define-macro (LABEL nodeid)
        (if $args
            (doargs (arg) (setx (eval nodeid) NODE_LABEL (eval arg)))
            (getx nodeid NODE_LABLE)))

(define-macro (DESC nodeid)
        (if $args
            (doargs (arg) (setx (eval nodeid) NODE_DESC (eval arg)))
            (getx nodeid NODE_DESC)))

(define-macro (TIP nodeid)
        (if $args
            (doargs (arg) (setx (eval nodeid) NODE_TIP (eval arg)))
            (getx nodeid NODE_TIP)))

(define TAG_HTML '(a abbr acronym address applet area article aside audio b base basefont bb bdo big blockquote body br button canvas caption center cite code col colgroup command datagrid datalist dd del details dfn dialog dir div dl dt em embed eventsource fieldset figcaption figure font footer form frame frameset h1 head header hgroup hr html i iframe img input ins isindex kbd keygen label legend li link map mark menu meta meter nav noframes noscript object ol optgroup option output p param pre progress q rp rt ruby s samp script section select small source span strike strong style sub sup table tbody td textarea tfoot th thead time title tr track tt u ul var video wbr))
(define TAG_SVG  '(a altGlyph altGlyphDef altGlyphItem animate animateColor animateMotion animateTransform audio canvas circle clipPath color-profile cursor defs desc discard ellipse feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence filter font font-face font-face-format font-face-name font-face-src font-face-uri foreignObject g glyph glyphRef hatch hatchpath hkern iframe image line linearGradient marker mask mesh meshgradient meshpatch meshrow metadata missing-glyph mpath path pattern polygon polyline radialGradient rect script set solidcolor stop style svg switch symbol text textPath title tref tspan unknown use video view vkern))
(define TAG_GV   '("gv-digraph" "gv-graph" "gv-subgraph" "gv-node" "gv-table" "gv-tr" "gv-td" "gv-hr" "gv-vr" "gv-img" "gv-br" "gv-b" "gv-font" "gv-i" "gv-o" "gv-s" "gv-sub" "gv-sup"  "gv-u"))
(eval (cons N-N TAG_HTML))
(eval (cons N-N TAG_SVG))
(eval (cons N-N TAG_GV))
(def-new 'N-gv-digraph 'N-digraph)
(def-new 'N-gv-graph 'N-graph)
(def-new 'N-gv-subgraph 'N-subgraph)
(def-new 'N-gv-node 'N-node)

(define (N-M-default)
  (N-M "html" "head" "meta" "body" "Div" "span" "ul" "li" "pre")
  (N-M "digraph" "subgraph" "graph" "node" "table" "tr" "td" "hr" "vr" "img" "br")
  (N-M "->" "umlet" "textile")
  (N-M "table-array" "td-array")
  )


(define (test-1)
  (println
    (N-gv-digraph '((label "test graph"))
       (N-gv-subgraph '((id "abcd"))
          (N-gv-node (sym "node_1")
             (N-gv-table
               (for (j 1 3)
                    (extend ""
                        (N-gv-tr
                          (for (i 1 3) (extend "" (N-gv-td  "hello" )))
                          ) ) ) ) ) ) ) ) )


(define (test-2)
  (println
   (N-html 'root
           (N-div '((class red) (with 10))
                  1 2 3 4
                  (N-div "node1" '(("label" "apple")) "none1:good day")
                  (N-ul "node2" '(("label" "banana")) "node2:banana"
                        (for (i 0 3)
                             (extend "" (N-span "" (format "node3%d" i)))
                             ) ) ) ) ) )


(define (test-3)
  (begin
    (T "n0"
       "r12"  "r13"
       )
    
    (R "r0"
       "n11" "n12" "n13"
       )
    (println (getx-tag "n0"))
    (println (getx-children "r0"))   
    ))


(define (getx-symbols)
  (clean (curry = "myData") (map term (symbols myData))))


(define (sort-by-sn x y)
        (letn ((a (getx-sn x))
               (b (getx-sn y)))
               (<= a b)
               ))


;; @syntax (N:get-summary)
;; @description getx summary
(define (get-summary , m)
        (letn ((syms (getx-symbols))
               (nodes     (clean  (fn (x) (= NODE_TYPE_RELATION (getx-type x))) syms))
               (relations (filter (fn (x) (= NODE_TYPE_RELATION (getx-type x))) syms))
               (all-nodes (clean nil? (unique (flat (append nodes (map getx-children relations))))))
               (all-relations (clean nil? (unique (flat (append relations (map getx-tag nodes))))))
               (v-nodes     (difference all-nodes nodes))
               (v-relations (difference all-relations relations))
               (all-relations (sort all-relations sort-by-sn))
               (all-nodes (sort all-nodes sort-by-sn))
               (m (map
                    (fn (x)
                        (append
                          (list (getx x NODE_SN) (getx-type x) x (getx x NODE_PID) (getx x NODE_LEVEL)
                                (getx-id x) (getx-label x) (getx-desc x) (getx-tip-count x) 
                                (getx-category x))
                          (map (fn (y)
                                   (if (getx x y)
                                       2
                                      (if (or (if (getx-children y)
                                                (find x $it))
                                              (if (getx-tag x)
                                                (find y $it)))
                                          1
                                          nil)))
                               all-relations)
                          ))
                    all-nodes))
               (m1 (transpose m))
               (sum1 (map (fn (x) (for-all null? x)) m1))
               (sum2 (map (fn (x) (exists (fn (y) (= y 2)) x)) m1))
               (get-idx (fn (x) (int (replace "r" (copy x) ""))))
               (get-rev (fn (x) (find "r" x)))
               (attr-len (- (length (m 0)) (length all-relations)))
               (f1 (fn (x idx)
                       (letn ((rev (get-rev idx))
                              (idx (get-idx idx))
                              (rev (if (or (= idx IDX_TIP_CNT) (>= idx attr-len)) (not rev) rev))
                              (m2 (sort (m1 idx)))
                              (m3 (reverse (copy m2)))
                              (len (length m2))
                              (y (x idx))
                              (d (div (add (find y m2)
                                           (sub len (find y m3))) 2))
                              )
                             (if rev
                                 (sub len d)
                                 d))))
               (width (length (m 0)))
               (l (if (null? opt-node-summary-sort:opt-node-summary-sort)
                      (list "0")
                      opt-node-summary-sort:opt-node-summary-sort))
               (l (clean (fn (x) (<= width (get-idx x))) (unique l)))
               
               (l0 (sequence 1 (length l)))
               (l0 (map (fn (z) (div 1 z)) l0))
               
               (f2 (fn (x)
                       (letn ((l1 (map (fn (idx) (f1 x idx)) l))
                              (l1 (map mul l1 l0)))
                             (apply add l1))))
               )
              (if all-nodes
                  (letn ((width (length (m 0))) 
                         (height (length all-nodes)))
                        (sort m (fn (x y) (<= (f2 x) (f2 y))))
                        (list
                          all-nodes nodes v-nodes all-relations relations v-relations
                          m
                          sum1
                          sum2
                          )))))             


;;; gv extension
;; @syntax (N:N--> list1 list2 list3 ... [attr])
(define (N-->)
        (letn ((f (fn (x)
                      (map (fn (y) (format "\"%s\"" (if (symbol? y) (string (term y)) (string  y)))) (unique (flat (list x))))))
               (attrs (filter N:is-attr-map? $args))
               (ps (clean N:is-attr-map? $args))
               (a (join (map (fn (x) (format "{ %s }" (join (f x) " "))) ps) "->"))
               (b (join (map (fn (x) (N:generate-params x 0)) attrs) " "))
               )
              (if (null? b)
                  a
                  (format "%s [ %s ] \n" a b)
                  )))

(define (render-gv-td td-data td-attr)
        (letn ((x (trim td-data)))
              (if (ends-with x "#") ;;parse xxx#,xxx#port#
                  (letn ((t1 (parse x "#")) 
                         (a1 (t1 0))
                         (a2 (t1 (- (length t1) 2))))
                        (N:N-gv-td td-attr (list (list "PORT" a2)) (escape a1)))
                  (N:N-gv-td td-attr (escape td-data)))))


(define (parse-text-lines text)
        (letn ((s nil)
               (lines (map trim (parse text "\n")))
               (lines (clean (fn (x) 
                                 (if (and (nil? s) (null? x) ) 
                                     true 
                                     (begin (setq s 1) nil))) 
                             lines)))
              lines))


;; @syntax (Web:escape <str>)
;; @param <str> a string to escape
;; @return the escaped string
;; <p>Escapes characters that are part of the (X)HTML and XML syntax to prevent
;; characters from confusing browsers' parsing of markup. Escapes single and
;; double quotes, ampersands, and left and right angle brackets
;; ('&quot;', '&apos;', '&amp;', '&lt;', and '&gt;').</p>
(define (escape str)
  (replace {&} str {&amp;})
  (replace {"} str {&quot;})
  (replace {'} str {&apos;})
  (replace {<} str {&lt;})
  (replace {>} str {&gt;})
  str)

;; @syntax (Web:unescape <str>)
;; @param <str> an entity-escaped string
;; @return the unescaped string
;; <p>Unescapes the basic (X)HTML and XML character entities in a string.</p>
(define (unescape str)
  (replace {&quot;} str {"})
  (replace {&apos;} str {'})
  (replace {&amp;} str {&})
  (replace {&lt;} str {<})
  (replace {&gt;} str {>})
  str)
            
            
;; @syntax (N:N-umlet [nodeid] text)
(define-macro (N-umlet)
              (letn ((nargs (length $args))
                     (nodeid (if (= nargs 2) ($args 0)))
                     (text (if (= nargs 2) ($args 1) (args 0)))
                     )
                    (letex ((nodeid nodeid)
                            (text text))
                           (letn ((s 1)
                                  (lines (parse-text-lines text))
                                  (f1 (fn (x)
                                          (cond
                                            ((= "--" x) (begin (setq s 2) "<HR/>"))
                                            ((= 1 s) (N:N-gv-tr (render-gv-td x (list "ALIGN" "center"))))
                                            ((= 2 s) (N:N-gv-tr (render-gv-td x (list "ALIGN" "left"))))
                                            (true "")
                                            )))
                                  )
                                 (if (= nargs 2)
                                     (N:N-gv-node nodeid (list "shape" "plaintext") 
                                                  (N:N-gv-table '("CELLBORDER" "0") 
                                                                (join (map f1 lines))
                                                                ))
                                     (N:N-gv-table '("CELLBORDER" "0") 
                                                   (join (map f1 lines))
                                                   )
                                     ) ))
                    ) )

;; @syntax (N:N-textile [nodeid] text)
(define-macro (N-textile)
              (letn ((nargs (length $args))
                     (nodeid (if (= nargs 2) ($args 0)))
                     (text (if (= nargs 2) ($args 1) (args 0)))
                     )
                    (letex ((nodeid nodeid)
                            (text text))
                           (letn ((lines (parse-text-lines text))
                                  (is-table? (fn (x)
                                                 (letn (x (trim x))
                                                       (and (= "|" (first x)) (= "|" (last x))))))
                                  (a (map is-table? lines))
                                  (b (map (fn (x) (if (nil? x) (format "#f%d#" $idx) (format "t%d" $idx))) a))
                                  (c (map parse (clean null? (map trim (parse (join b " ") "#")))))
                                  (ftable (fn (x , t1)
                                              (N:N-gv-table
                                                (dolist (tr-data x)
                                                  (setq t1 (parse tr-data "|"))
                                                  (setq t1 (slice t1 1 (- (length t1) 2)))
                                                  (extend ""
                                                          (N:N-gv-tr
                                                            (dolist (td-data t1)
                                                              (extend "" (render-gv-td td-data))
                                                              ))))))) 
                                  (parse-td (fn (x)
                                                (letn ((x (trim x)))
                                                      (if (ends-with x "#")
                                                          (letn ((t1 (parse x "#"))
                                                                 (a1 (t1 0))
                                                                 (a2 (t1 (- (length t1) 2))))
                                                                (list a1 (list (list "PORT" a2))))
                                                          (list x nil)))))
                                  (content "")
                                  (attr1 '(("BORDER" "0")("ALIGN" "left")))
                                  )
                                 
                                 (dolist (l c)
                                   (letn ((type (first (first l)))
                                          (b (int (replace "f|t" (first l) "" 1)))
                                          (data (slice lines b (length l)))
                                          (d1 (if (= "t" type)
                                                  (N:N-gv-tr (N:N-gv-td attr1 (ftable data)))
                                                  (letn ((d11 (first data)))
                                                        (if (= "--" d11)
                                                            "<HR/>"
                                                            (N:N-gv-tr  (render-gv-td d11 attr1))
                                                            )
                                                        ) )) 
                                          )
                                         (extend content d1)
                                         )
                                   )
                                 (if (= nargs 2)
                                     (N:N-gv-node nodeid (list "shape" "plaintext") 
                                                  (N:N-gv-table '("CELLBORDER" "0") content)
                                                  )
                                     (N:N-gv-table '("CELLBORDER" "0") content)
                                     )
                                 )
                           ))
              )


;; @syntax (N:N-table-array (list (list)) table-attr tr-attr td-attr)
;; @example
;; (N:N-table-array '(("td11" "td12")("td21" "td22")))
(define (N-table-array a table-attr tr-attr td-attr)
        (N-gv-table table-attr
          (dolist (tr-data a)
            (extend ""
                    (N-gv-tr tr-attr
                      (dolist (td-data tr-data)
                        (extend "" (N-gv-td td-attr (string td-data)))
                        )
                      )
                    )
            )))


;; @syntax (N:N-tr-array list tr-attr td-attr)
;; @example
;; (N:N-td-array '("td1" "td2"))
(define (N-tr-array a tr-attr td-attr)
        (N-gv-tr tr-attr
          (dolist (td-data a)
            (extend "" (N-gv-td td-attr td-data))
            )
          )
        )

;; @syntax (N:N-td-array list td-attr)
;; @example
;; (N:N-td-array '("td1" "td2"))
(define (N-td-array a td-attr)
        (N-gv-tr
          (dolist (td-data a)
            (extend "" (N-gv-td td-attr td-data))
            )
          )
        )

(define (N-gv-rank nodes size , ns ns-l lines i level level-node level-node-def level-nodes)
        (when (< size (length nodes))
          (setq ns (explode nodes size))
          (setq ns-l (length ns))
          (setq level-nodes '())
          (setq lines '())
          (dotimes (i ns-l)
            (setq level (inc gv-rank-level))
            (setq level-node (format {gv_rank_level_%d} level))
            (push level-node level-nodes -1)
            (setq level-node-def (format {%s [ label="" style="invis" width=0 height=0 ]} level-node))
            (push (append
                    "{ rank=same;\n"
                    level-node-def "\n"
                    (join (map (curry format {"%s"}) (ns i) ) " ")
                    "}"
                    )
                  lines -1))
          (push (format {%s [ style="invis" ] } (join level-nodes " -> ")) lines -1)
          (if lines
              (join
                lines
                "\n")
              "")))

(context 'MAIN)

