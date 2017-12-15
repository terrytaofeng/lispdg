#!/usr/bin/env newlisp

(define (REF-DEFINE title level , idx)
        (setq idx (if ref_define_data
                      (length ref_define_data)
                      0))
        (if (not level) 
            (setq level 0))
        (push (list idx title level) ref_define_data -1)
        (format {%s<a id="ref_%d"></a><h2>%s</h2>} (dup "&nbsp;" level) idx title))

(define (REF-GENERATE)
        (when ref_define_data
          (letn (links (join
                         (map
                           (fn (x)
                               (format {%d . <a href="#ref_%d">%s</a>} (+ 1 $idx) (x 0) (x 1))
                               )
                           ref_define_data
                           )
                         "<br/>"
                         ))
                (format {<div id="ref-index">
                         %s
                         </div>
                         <script>
                         (function() {
                                      var e = document.getElementById("ref-index");
                                      var body = e.parentNode;
                                      body.removeChild(e);
                                      body.insertBefore(e,body.firstChild);
                                      
                                      })();                         
                         </script>
                         }
                        links)
                )
          ))

(define (REF-PRINT)
        (println (REF-GENERATE)))


(define (my-link a a-exclude a-include)
  (letn ((params (if (is-cgi?) CGI:params (list)))
         (a (if a $it (list)))
         (p1 (clean (fn (x)  
                   (lookup (first x) a)
                   ) params))
         (p2 (append a p1))
         )
        (if a-exclude
            (setq p2 (clean (fn (x) (lookup (first x) a-exclude)) p2)))
        (if a-include
            (setq p2 (filter (fn (x) (find (first x) a-include)) p2)))
        (join (map (fn (x) (append (x 0) "=" (x 1))) p2) "&")))

(define (get-uri-opt opt-str)
        (letn ((opt-str (string opt-str))
               (opt-sys (if (eval (sym opt-str)) $it "")))
              (if (CGI:get opt-str) 
                  $it 
                  opt-sys)))

(define (html-option-select name title options , opt-html js)
        (setq cur (get-uri-opt name))
        (setq name (string name))
        (setq opt-html (join
                         (map (fn (x)
                                  (letn (attr (if (= cur (string (x 0)))
                                                  {selected="selected"}
                                                  ""))
                                        (format {<option %s value="%s">%s</option>} attr (string (x 0)) (string (x 1)))    
                                        ))
                              options)
                         ) )
        (setq js (format [text]
              l=window.location.href;
              l=l.replace(/[&]*%s=[a-z0-9A-Z_-]*/,"");
              e=this;
              v=e.options[e.selectedIndex].value;
              l=l  + "&" + e.name + "=" + v;
              window.location.href =l;
              [/text] name));
        (format
          {%s<select name="%s" onchange='%s'>
           <option value="">--</option>
           %s
           </select>} title name js opt-html))

(define (is-toggle-opt? opt-str)
        (letn ((opt-str (string opt-str))
               (opt-sys (if (eval (sym opt-str)) 1 0))
               (opt (if (CGI:get opt-str) (int $it) opt-sys)))
              (if (= 0 opt)
                  nil
                  true)))

(define (html-option-toggle opt-str title)
        (letn ((opt-str (string opt-str))
               (opt (if (is-toggle-opt? opt-str) "1" "0"))
               (opt-next (if (is-toggle-opt? opt-str) "0" "1")))
               (format {<a href="?%s">%s:%s</a>}
                       (my-link (list (list opt-str opt-next)))
                       title
                       opt)))
