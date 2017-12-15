#!/usr/bin/env newlisp
;; @module bps.lsp

(module "bps.lsp")
(module "getopts.lsp")


(load-module "html.lsp")
(load-module "node.lsp")

(N:N-M)

;;; html-matrix

(define base_dir "/var/www/html/lispdg/")
(define gallery_dir (format "%s/gallery/" base_dir))
(define modules_dir (format "%s/bps/" base_dir))
(define lispdg_file (format "%s/lispdg.cgi" base_dir))

;(define gallery_dir "/var/www/html/data/gitmap/")
;(define modules_dir "/usr/lib/bps/")
;(define lispdg_file "/var/www/html/lispdg.cgi")


(define HEAD_SORT_COUNT 5)
(define HEAD_SORT_WITH 28)

(define KEY_ORDER_0 0)
(define KEY_ORDER_1 1)
(define KEY_ORDER_2 2)

(define MODE_SWITH  0)
(define MODE_TOGGLE 1)
(define MODE_LOOP   2)
(define MODE_PRI    3)

(define ATTR_IDX_ATTR_GROUP 2)
(define HTML_MATRIX_ATTR_HEADER_WITH 2)

(define URI_SORT           "sort")
(define URI_SORT_R         "r")
(define URI_SORT_SEP       ",")
(define URI_RELATION_COLOR "rs")

(define lispdg 1)
(global 'lispdg)

;; parse opt=1,2,4r
;; return ("1" "2" "4r")
(define (get-key-order)
        (if (is-cgi?)
            (if (CGI:get URI_SORT)
                (map string (parse $it URI_SORT_SEP))
                (list))
            (list)))

;; return KEY_ORDER_x
(define (get-key-order-status idx)
        (letn (opt (get-key-order)
                   (i (string idx))
                   (ir (append i URI_SORT_R)))
              (cond
                ((find i opt) KEY_ORDER_1)
                ((find ir opt) KEY_ORDER_2)
                (true KEY_ORDER_0))))

;; return idx[r]
(define (get-key-order-idx idx)
        (letn (opt (get-key-order)
                   (i (string idx))
                   (ir (append i URI_SORT_R)))
              (if (find i opt)
                  $it
                  (if (find ir opt)
                      $it))))

(define (gen-key-order-model idx mode)
        (letn ((status (get-key-order-status idx))
               (opt (get-key-order))
               (idx (string idx))
               (ir  (append idx URI_SORT_R))
               (adj-pri (fn (i)
                            (letn ((idx (find i opt))
                                   (idx1 (if idx
                                             (% (+ (- idx 1) (length opt)) (length opt))))
                                   )
                                  (if idx
                                      (swap (opt idx) (opt idx1))))))
               )
              (cond 
                ((= status KEY_ORDER_0) (push idx opt -1))
                ((= status KEY_ORDER_1) (cond
                                          ((= mode MODE_SWITH)  (pop opt (find idx opt)))
                                          ((= mode MODE_TOGGLE) (replace idx opt ir))
                                          ((= mode MODE_LOOP)   (replace idx opt ir)) 
                                          ((= mode MODE_PRI)    (adj-pri idx))) )
                ((= status KEY_ORDER_2) (cond
                                          ((= mode MODE_SWITH)  (pop opt (find ir opt)))
                                          ((= mode MODE_TOGGLE) (replace ir opt idx))
                                          ((= mode MODE_LOOP)   (pop opt (find ir opt)))
                                          ((= mode MODE_PRI)    (adj-pri ir)) )))
              (join opt URI_SORT_SEP)
              ))

(define (gen-pdg-data)
      (if (N:has-data)
          (setq summary_data (N:get-summary)))
      )

(define (gen-attr-key hh idx)
        (letn ((color_key (hh idx))
               (color (get-mycolor color_key))
               (op1 (gen-key-order-model idx MODE_PRI)))
              (format {<a href="?%s"><span title="%s" style="background-color:#%02x">%s</span></a>}
                      (my-link (list (list URI_SORT op1))) 
                      color_key color (first color_key))))



(define (html-widget-key-style2 idx)
        (letn ((status (get-key-order-status idx))
               (s (if (get-key-order-idx idx) (string $it) ""))
               (h (cond
                    ((= status KEY_ORDER_1) "v")
                    ((= status KEY_ORDER_2) "^")
                    (true nil)))
               )
               (if h
                  (format {<a href="?%s">%s</a>} 
                          (my-link (list (list URI_SORT (gen-key-order-model idx MODE_TOGGLE) ))) 
                          (format "%s%s" s h)))))
        



; (define-macro (letb a b)
;               (letn ((src (append '(local) (list (eval a)) 
;                                   (list (list 'bind (list 'transpose (list 'list a b)))) 
;                                   (args))))
;                     (eval src)))

(define (html-matrix-main-table)
        (hp (if (is-toggle-opt? 'OPT_HEAD_TOGGLE) "<div>" {<div style="width:200%;height:80%;overflow:auto">})
            (hp "<table>"
                (letn ((attr_base1 (map (fn (x) (if (sum1 $idx) "" x)) attr_base))
                       (attrs 
                         (append
                           attr_base1
                           all-relations
                           ))
                       (tr1 (map (fn (x)
                                     (letn ((h1 (html-widget-key-style2 $idx))
                                            (h2 (format {<a title="%s" href="?%s">%s</a>} 
                                                        x (my-link (list (list URI_SORT (gen-key-order-model $idx MODE_SWITH))))  
                                                        (if (and (>= $idx l_attr_base1) (not (sum2 $idx)))
                                                            (slice x 0 HTML_MATRIX_ATTR_HEADER_WITH) x))
                                                ))
                                           (if h1
                                               (format "%s</br>%s" h1 h2)
                                               (format "%s" h2))  
                                           )) 
                                 attrs))
                       (show-matrix (fn (nodeid idx)
                                        (letn ((r (attrs idx)))
                                              (if (N:getx-string nodeid r "<br/>")
                                                  $it
                                                  (gen-attr-key attrs idx)))))
                       (f1 nil)
                       (f2 nil)
                       (key-order (map (fn (x) (int (replace URI_SORT_R x "" 1))) (get-key-order)))
                       )
                      
                      (fp 
                        (tr
                          (dolist (td-data (push (html-option-toggle 'OPT_DYN_ATTR  "attr") tr1 3))
                            (extend "" (td (if td-data td-data "")))
                            ))) 
                      
                      (dolist (tr-data matrix)
                        (setq f1 (map (fn (x) 
                                                (if (and 
                                                      (>= $idx l_attr_base) 
                                                      (>= x 1)
                                                      (if (is-toggle-opt? 'OPT_DYN_ATTR)
                                                          (find $idx key-order)
                                                          true)                                                      
                                                      )
                                                    $idx
                                                    nil)) tr-data))
                        
                        (setq f1 (clean null? (slice f1 l_attr_base)))
                        (sort f1 (fn (x y) 
                                     (letn ((x1 (if (find x key-order) $it x))
                                            (y1 (if (find y key-order) $it y))
                                            )
                                           (< x1 y1)
                                           )))
                        (setq f1 (map (fn (x) (gen-attr-key attrs x)) f1))
                        (setq f2 (if f1 (join f1 "")""))
                        
                        (hp "<tr>"
                            (dolist (td1 tr-data)
                              (letn ((td1 (if td1 td1 ""))
                                     ;(td1 (replace "\n" (string td1) "</br>" 1))
                                     (td1 (string td1))
                                     (td1 (if (>= $idx l_attr_base) 
                                              (if (null? td1)
                                                  ""
                                                  (case td1
                                                        ("0" "")
                                                        ("1" (gen-attr-key attrs $idx))
                                                        ("2" (show-matrix (tr-data 2) $idx))))
                                              (if (= "0" td1) "" td1)))
                                     (td1 (if (= $idx N:IDX_TIP_CNT) 
                                              (format {<a href="?%s">%s</a>} 
                                                      (my-link (list (list "fun" "html-show-tip") (list "nid" (tr-data 2))))
                                                      td1 )
                                              td1))
                                     )
                                    (fp {<td style="border-bottom:1px solid black;">%s</td>} td1)
                                    (if (= $idx ATTR_IDX_ATTR_GROUP)
                                        (fp {<td style="border-bottom:1px solid black;">%s</td>} f2))
                                    ))
                            )
                        )
                      )
                )
            ))

(define (html-summary-relation , aa1 aa2 m_node_atts m_attr)
        (setq aa1 (map (fn (x) (- (int (replace URI_SORT_R x "" 1)) l_attr_base)) (get-key-order)))
        (if opt-full-html-summary-relation
            (setq aa1 (unique (append aa1 (series 0 (curry + 1) (length all-relations)))))
            )
           
        (setq aa2 
              (map (fn (x) (list x (format "%05d%s" $idx x)))
                   (select all-relations aa1)))
        (setq m_node_atts
              (map (fn (x)
                       (letn ((nodeid (x N:IDX_NODEID))
                              (f-choice (fn (v k) (if v k)))
                              (rs (slice x l_attr_base))
                              (rs1 (clean nil?
                                          (select (map f-choice rs all-relations) aa1) ))
                              (rs2 (map (fn (x) (lookup x aa2)) rs1))
                              )
                             
                             (list (join rs2 "|")
                                   (join rs1 "|")
                                   nodeid
                                   )
                             )
                       )
                   matrix
                   )
              )
        (setq show-model (fn (nodes)
                             (letn (k (get-uri-opt 'OPT_SHOW_STYLE))
                                   (case k
                                         ("key1" (join nodes ","))
                                         ("key2" (join nodes "<br/>"))
                                         ("key3" (letn ((column 5)
                                                        (m1 (explode nodes column))
                                                        )
                                                       (N:N-table-array m1  nil nil '("style" "padding-left:5px;padding-right:5px;border-left:1px solid black;width:120px"))))
                                         (true (join nodes ","))))))
        (setq m_node_atts (map (fn (x) (select x '(1 2))) (sort m_node_atts)))
        (setq m_attr (unique (map first m_node_atts)))
        (if (and m_attr (null? (first m_attr)))
            (rotate m_attr -1))
        
        
        (hp "<table border=1>"
            (hp "<tr>"
                (fp "<td>%s</td>" (html-option-toggle 'OPT_SUMMARY_RELATION_FULL  "summary relation full"))
                (fp "<td>%s</td>" (html-option-select 'OPT_SHOW_STYLE "show style" '(("key1" ",")
                                                                                     ("key2" "br")
                                                                                     ("key3" "table")
                                                                                     )))
                )
            (dolist (attr m_attr)
              (hp "<tr>"
                  (fp "<td>%s</td>" attr)
                  (fp "<td>%s</td>" (show-model (map last (filter (fn (x) (= attr (first x))) m_node_atts))))
                  )
              )
            )
        )

(define attr_base (list "sn" "type" "nid" "pid" "level" "id" "label" "desc" "tip" "category"))
(define l_attr_base (length attr_base))

(define (html-matrix)
        (html-response-type)
        (letb '(all-nodes nodes v-nodes all-relations relations v-relations matrix sum1 sum2) summary_data
              (letn ()
                    (hp "<html>"
                        (fp
                          {
                           <head>
                           <meta http-equiv="Content-type" content="text/html; charset=utf8" />
                           </head>
                           })
                        (hp {<body>}
                            (fp {<div><a href="?">root</a> <a href="?%s">clear</a> | %s<br/></div>} 
                                (my-link nil nil '("fun" "file"))
                                (join
                                  (list
                                    (html-option-toggle 'OPT_HEAD_SORT "sort attrs")
                                    (html-option-toggle 'OPT_DYN_ATTR  "dyn attr")
                                    (html-option-toggle 'OPT_HEAD_TOGGLE  "head toggle")
                                    (html-option-toggle 'OPT_SUMMARY_RELATION  "summary relation")
                                    )
                                  
                                  " ")
                                )
                            (hp (if (is-toggle-opt? 'OPT_HEAD_TOGGLE) {<table style="display:none">} "<table>")
                                (letn ((rs1 (if (is-toggle-opt? 'OPT_HEAD_SORT)
                                                (sort (copy all-relations))
                                                all-relations))
                                       (width HEAD_SORT_COUNT)
                                       (m (if rs1 (explode rs1 width) (list)))
                                       (height (length m))
                                       )
                                      (dolist (tr-data m)
                                        (hp "<tr>"
                                            (dolist (x tr-data)
                                              (letn ((idx (+ l_attr_base (find x all-relations))))
                                                    (fp {<td>%s</td>} (if (html-widget-key-style2 idx) $it ""))
                                                    (fp
                                                      {<td><a title="%s" href="?%s"><span style="background-color:#%02x">%s</span></a></td>
                                                       <td><a title="%s"  href="?%s"><span style="background-color:#%02x">%s</span></a></td>}
                                                      x (my-link (list (list URI_SORT (gen-key-order-model idx MODE_PRI)))) (get-mycolor x) (first x)
                                                      x (my-link (list (list URI_SORT (gen-key-order-model idx MODE_SWITH)))) (get-mycolor x) (slice x 0 HEAD_SORT_WITH)
                                                      )
                                                    ))))
                                      ))
                            (cond
                              ((is-toggle-opt? 'OPT_SUMMARY_RELATION)
                                (if (is-toggle-opt? 'OPT_SUMMARY_RELATION_FULL)
                                     (let (opt-full-html-summary-relation 1) (html-summary-relation))
                                     (html-summary-relation)))
                              (true (html-matrix-main-table))
                              
                              )
                            ))))
        )


;;; search gitmap


(define (html-noargs)
        (letn (fs (exec (format {find -L %s -name "*.pdg.lsp" |sort} gallery_dir)))
              (hp "<html>"
                  (hp "<body>"
                      
                      (dolist (fun (list "html-design-pdg-node" "html-design-pdg-lispdg" "html-design-bps-module"))
                        (map
                          (fn (x)
                              (letn (fun1 (x 0) title (x 1))
                                    (if fun1
                                        (fp { <a href="?fun=%s&fun1=%s">%s</a>} fun fun1 title)
                                        (fp { <a href="?fun=%s">%s</a>} fun title))))
                          (list
                            (list "html-matrix" "[M]" )
                            (list "html-digraph" "[G]" )
                            (list "html-debug" "[D]" )
                            (list nil (append "design:" fun))
                            ) )
                        (println "<br/>")
                        )
                      
                      (dolist (f fs)
                        (map
                          (fn (x)
                              (letn (fun (x 0) title (x 1))
                                    (if fun
                                        (fp { <a href="?fun=%s&file=%s">%s</a>} fun f title)
                                        (fp { <a href="?file=%s">%s</a>} f title))))
                          (list
                            (list "html-matrix" "[M]" )
                            (list "html-digraph" "[G]" )
                            (list "html-html" "[H]" )
                            (list "html-debug" "[D]" )
                            (list "html-callgraph" "[C]" )
                            (list nil (replace gallery_dir (copy f) "" 1))
                            ) )
                        (println "<br/>")
                        ) ) ) ))
;;;

(define (html-show-tip)
        (html-response-type)
        (letn (
               (nodeid (CGI:get "nid"))
               (label (if (N:getx-label nodeid true) $it ""))
               (desc  (if (N:getx-desc nodeid) $it ""))
               (tips (N:getx nodeid N:NODE_TIP)))
              (hp "<html>"
                  (hp "<body>"
                      (fp {<h3>%s</h3>} (string label))
                      (fp {<p>%s</p>} (string desc))
                      (dolist (tip tips)
                        (fp {<p>%stip %d%s</p>} (dup "=" 20) (+ 1 $idx) (dup "=" 20) )
                        (fp {<pre>%s</pre>} tip)
                        ) ) ) )) 

;;;
(define (html-digraph , get-rs-color make-rs-color rs-lable-list gv-extend gv-graph)
        (html-response-type)
        (letb '(all-nodes nodes v-nodes all-relations relations v-relations matrix sum1 sum2) summary_data
              (setq get-rs-color (fn (rs)
                                     (letn ((rs (map (fn (x y) (if x y)) rs all-relations))
                                            (color_key (join (clean null? rs) "|"))
                                            (color (get-mycolor color_key))
                                            )
                                           (list color_key color))))
              (setq make-rs-color
                    (map (fn (row)
                             (letn ((nodeid (row N:IDX_NODEID))
                                    (rs (slice row l_attr_base)))
                                   (letb '(color_key color) (get-rs-color rs)
                                         (if (and (not (null? color_key)) (!= N:NODE_TYPE_RELATION (N:getx-type nodeid)))
                                             (format "\"%s\" [ tooltip=\"%s\" style=filled fillcolor=\"#%02x\" href=\"?%s\"] \n"  nodeid color_key color  (my-link (list (list URI_RELATION_COLOR (string color)))))
                                             ) ) ))
                         matrix
                         ) )
              
              (extend gv-extend (join (clean null? make-rs-color) "\n"))
              
              ;; make cluster by color
              (if (and (is-cgi?) (CGI:get URI_RELATION_COLOR))
                  (letn ((l_color_nodeid (map (fn (row)
                                                  (letn ((nodeid (row N:IDX_NODEID))
                                                         (rs (slice row l_attr_base)))
                                                        (letb '(color_key color) (get-rs-color rs)
                                                              (list (string color) color_key nodeid))))
                                              matrix
                                              ))
                         (l_color_key (map (fn (x) (slice x 0 2)) (unique (clean nil? l_color_nodeid))))
                         (get_nodes (fn (r)
                                        (letn ((m1 (map (fn (x) (if (find r x) (x 2))) l_color_nodeid))
                                               (m1 (clean nil? m1))
                                               (m1 (map (fn (x) (format "\"%s\"" x)) m1)))
                                              (if (and m1 (> (length m1) 0))
                                                  (join m1 " ")))))
                         (_rs (CGI:get URI_RELATION_COLOR))
                         (_rss (if (= _rs "ALL")
                                   (unique (map (curry nth 0) (clean nil? l_color_nodeid)))
                                   (list _rs)))
                         )
                        (dolist (r _rss)
                          (if (get_nodes r)
                              (extend gv-extend (N:N-subgraph (list "label" (lookup r l_color_key)) '(("style" "filled") ("fillcolor" "lightgray")) (get_nodes r))); (format "subgraph cluster_%s{ style=filled;\nfillcolor=lightgray;\n%s } \n" r $it))
                              ))))
              
              (if (N:is-root-gv-node?)
                  (letn (n (N:get-rootid))
                        (setq gv-graph (N:format-node n gv-extend))
                        )
                  (setq gv-graph (format "digraph G{\n%s\n}\n" gv-extend))
                  )
              (setq rs-lable-list
                    (join
                      (map (fn (n)
                               (letn ((rs (map (fn (x y) (if x y)) n all-relations)))
                                     (letb '(color_key color) (get-rs-color rs)
                                           (format {<a href="?%s"><span style="background-color:#%02x">%s</span></a> } 
                                                   (my-link (list (list URI_RELATION_COLOR (string color))))
                                                   color color_key))))
                           (unique (clean (fn (y) (for-all nil? y)) (map (fn (x) (slice x l_attr_base) ) matrix)))
                           )
                      ""
                      )
                    )
              (hp "<html>"
                  (fp
                    {
                     <head>
                     <meta http-equiv="Content-type" content="text/html; charset=utf8" />
                     </head>
                     }
                    )
                  (hp {<body>}
                      (when (not (null? rs-lable-list))
                        (hp {<div style=""><a href="?">root</a></div>})
                        ;(hp {<div style="position:fixed;top:2px;left:40px;">}
                        (hp {<div style="">}
                            (fp rs-lable-list)
                            (fp {<a href="?%s"><span>ALL</span></a> } (my-link (list (list URI_RELATION_COLOR "ALL")))))
                        )
                      (when gv-graph
                          (hp {<div style="">};{<div style="height:80%;overflow:auto">}
                             (println (N:dot gv-graph))    
                          )
                          
                          )
                      ))
              )
        )


(define (html-html)
        (html-response-type)
        (letb '(all-nodes nodes v-nodes all-relations relations v-relations matrix sum1 sum2) summary_data
              (begin
                (println (N:render))
              ;  (let (opt-full-html-summary-relation 1) (html-summary-relation))
                )))


(define (html-debug)
        (html-response-type "text/plain")
        (hp "printx:")
        (N:printx)
        (hp "data:")
        (replace "\\)" (string summary_data) "\)\n" 1)
        (hp "render:" )
        (println (N:render))
        )



(define (html-callgraph, s1 in-tag-html? clean1 sym-list sym-matrix)
        (html-response-type)
        (setq old_syms (map string (symbols 'MAIN)))
        (if pdg-file
            (load pdg-file)
            )
        (setq clean1 (fn (x)
                         (or
                           (and (find (term x) old_syms) (!= (term x) "html-auto"))
                           (find "test" (term x))
                           (find (term x) BPS_FUN)
                           )))
        (map set '(sym-list sym-matrix gv-body) (sym-depend-matrix 'MAIN nil clean1))
        (begin
          (N:N-gv-digraph 
            {
             node [ shape="record"]
             }
            gv-body
            )
          )
        (gen-pdg-data)
        (exec-fun1)
        )


(define (sym-dependof? x y)
        (find x (flat (eval y))))

(define (user-fun? x)
        (letn (t (type (eval x)))
              (or (= "lambda" t)
                  (= "fexpr" t))))

(define (get-sym-name x) 
        (if (= (prefix x) 'MAIN)
            (term x)
            (format "%s:%s" (string (prefix x)) (term x))))

(define (sym-depend-matrix cxt filter-sym clean-sym, ss f1 gv-body )
        (if (list? cxt)
            (setq ss cxt)
            (begin
              (setq cxt (if cxt (context (sym cxt)) 'MAIN))
              (setq ss  (symbols cxt))
              ) )
        
        (setq ss (filter user-fun? ss))
        (if filter-sym
            (setq ss (filter filter-sym ss))
            )
        (if clean-sym
            (setq ss (clean clean-sym ss))
            )
        (map (fn (x) (extend gv-body (format "\"%s\" ;\n" (get-sym-name x)))) ss)
        (list
          ss
          (map
            (fn (x)
                (map (fn (y)
                         (letn (d (sym-dependof? x y))
                               (if d
                                   (extend gv-body (format "\"%s\" -> \"%s\" ;\n" (get-sym-name y) (get-sym-name x))))
                               d
                               )
                         )
                     ss)
                )
            ss
            )
          gv-body
          )
        )
(define (exec-fun1)
        (if (and (is-cgi?) (CGI:get "fun1"))
            (setq fun1 (CGI:get "fun1")))
        (if fun1
            (eval-string (format "(%s)" fun1))
            (html-digraph)    
            )
        )

(define (html-design-pdg-node , s1 in-tag-html? clean1 sym-list sym-matrix fun1)
        (html-response-type)
        (setq s1 (symbols 'N))
        (setq s1 (filter user-fun? s1))
        (setq in-tag-html? (fn (x)
                               (letn (t (replace "N-" (term x) "" 1) )
                                     (find t
                                           (append
                                             (map term N:TAG_HTML)
                                             (map term N:TAG_SVG)
                                             (map string N:TAG_GV)
                                             )))))
        (setq clean1 (fn (x)
                      (or
                        (find "test" (term x))
                        (and
                          (find "N-[a-z]" (term x) 1)
                          (in-tag-html? x)
                          (for-all nil? (map (curry sym-dependof? x) s1))
                          )
                        )
                      ))
        (map set '(sym-list sym-matrix gv-body) (sym-depend-matrix 'N nil clean1))
        
        (begin
          (N:N-gv-digraph 
            {
             node [ shape="record"]
             "M_extension" -> "N:N-M"
            }
            
            
            (N:N-umlet 'M_extension [text]
                     N:N-M M_extension
                     --
                     html head meta body Div span ul li
                     --
                     digraph subgraph graph node 
                     table tr td hr vr img br
                     --
                     -> umlet textile
                     table-array td-array
                     [/text])
            
            gv-body
            
            
            
            )
          (N:R "gv-extension" 
               '(
                 "N:N-digraph"
                 "N:N-graph"
                 "N:N-subgraph"
                 "N:N-node"
                 "N:N-table-array"
                 "N:N-td-array"
                 "N:N-umlet"
                 "N:N-textile"
                 "N:N-->"
                 ))
          (N:R "redefine tools" 
               '(
                 "N:N-N"
                 "N:N-M"
                 "N:N-M-default"
                 ))
          (N:R "N:N-N(html xxx)"
               '(
                 "N:N-html"
                 "N:N-span"
                 "N:N-ul"
                 "N:N-div"
                 ))
          (N:R "N:N-N(gv xxx)" 
               '(
                 "N:N-gv-digraph"
                 "N:N-gv-subgraph"
                 "N:N-gv-td"
                 "N:N-gv-tr"
                 "N:N-gv-node"
                 "N:N-gv-table"
                 ))
          (N:R "N:N-M(xxx:xxx)" 
               '(
                 "M_extension"
                 ))          
          (N:R "base" 
               '(
                 "N:N"
                 "N:T"
                 "N:R"
                 "N:ATTR"
                 "N:DESC"
                 "N:LABEL"
                 "N:TIP"
                 ))
          (N:R "render tools" 
               '(
                 "N:render"
                 "N:gv-render"
                 "N:dot"
                 "N:dot-png"
                 "N:format-node"
                 ))          
          (N:R "help tools" 
               '(
                 "N:get-summary"
                 "N:has-data"
                 "N:get-rootid"
                 ))
          (N:R "debug tools" 
               '(
                 "N:printx"
                 ))
          
          (N:R "N-getx" 
               (filter (curry find "getx") (map get-sym-name s1))
               )
          )
        (gen-pdg-data)
        (exec-fun1)
        )

(define (parse-bps-module f , lines lines-f lines-m title)
        (setq 
          title (last (parse f "/"))
          lines (parse (read-file f) "\n")
          lines (map (fn (l)
                         (when (regex "^\\((define|define-macro)\\s+\\(([a-z0-9A-Z_-]+)" l)
                           (list $1 $2)))
                     lines)
          lines (sort (clean nil? lines))
          lines-f (map last (filter (fn (x) (= "define" (first x))) lines))
          lines-m (map last (filter (fn (x) (= "define-macro" (first x))) lines))
          umlet-text (join
                       (append
                         (list title "--")
                         lines-m
                         (if lines-m (list "--"))
                         lines-f
                         )
                       "\n" true)
          )
        (list 
           title
           lines
           lines-f
           lines-m
           umlet-text))


(define (html-design-bps-module , fs)
        (html-response-type)
        
        (setq 
          fs (exec (format {find -L %s -name "*.lsp" |sort} modules_dir))
          fs (append fs (list lispdg_file))
          )

        (begin
          (N:N-gv-digraph 
            {
             node [ shape="record"]
            }
            
            (let (result "")
               (dolist (f fs)
                 (when-let (r (parse-bps-module f))
                         (extend result (node '("shape" "plaintext")
                                              (N:N-umlet (last r))))))
               result)
            
            ))
                    
        (gen-pdg-data)
        (exec-fun1)
        )

(define BPS_FUN '("load-module" "letb" "fp" "hp" "html-response-type" "is-cgi?" "cgi-init" "cgi-index" "cgi-main" "main" "type"))

(define (html-design-pdg-lispdg , s1 in-tag-html? clean1 sym-list sym-matrix)
        (html-response-type)
        (setq clean1 (fn (x)
                         (or
                           (find "test" (term x))
                           (find (term x) BPS_FUN)
                           )))
        (map set '(sym-list sym-matrix gv-body) (sym-depend-matrix 'MAIN nil clean1))
        (begin
          (N:N-gv-digraph 
            {
             node [ shape="record"]
             }
            gv-body
            )
          (N:R "cgi_functions" cgi_functions)
          )
        (gen-pdg-data)
        (exec-fun1)
        )

(define (init-parse-opt)
  (shortopt "?" (getopts:usage) nil "help")
  (shortopt "h" (getopts:usage) nil "help")
  (shortopt "f" (setq pdg-file getopts:arg) "pdg" "Set pdg file")
  (getopts (2 (main-args)))
  )


;;;

(define (html-test)
        (html-response-type)
        (println
          (html-option-select 'OPT_XXX1 "title1" '(("k1" "v1")
                                                   ("k2" "v2")))
          )
        )

;;; main process
(define pdg-file nil)
(cgi-init)
(init-parse-opt)

(if (is-cgi?)
    (begin
      (if (CGI:get "file")
          (define pdg-file $it))
      (if (and
            (not pdg-file)
            (CGI:get "OPT_REPO")
            (CGI:get "OPT_FILE"))
          (define pdg-file (append (CGI:get "OPT_REPO") "/" (CGI:get "OPT_FILE"))))
      (if (CGI:get URI_SORT)
          (define opt-node-summary-sort:opt-node-summary-sort (get-key-order)))
      (if (CGI:get "fun")
          (define fun $it))
      ))


(if (and pdg-file (!= fun "html-callgraph"))
    (begin
      (load pdg-file)
      (gen-pdg-data)))

    
(define cgi_functions
        (list         
          "html-matrix"
          "html-digraph"
          "html-html"
          "html-debug"
          "html-callgraph"
          
          "html-show-tip"
          
          "html-design-pdg-node"
          "html-design-pdg-lispdg"
          "html-design-bps-module"
          
           "html-test"
          ))

(define (cgi-index)
        (if (not html-auto)
            (html-response-type))
        (cond
          (html-auto (apply html-auto))
          ((N:has-data)
            (if (N:is-root-gv-node?) 
                (html-digraph)
                (html-matrix)))
          (true (html-noargs)))
        )


(define (mydebug)
        (N:printx)
        (println (dup "=" 80))
        (println (N:get-summary))
        (println (dup "=" 80))
        (println (N:render))
        )

(if (is-cgi?)
    (cgi-main)
    (main))

(exit)