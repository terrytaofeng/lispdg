;; @module bps.lsp


(define-macro (load-module:load-module)
              (dolist (arg (args))
                (let ((r nil)
                      (dirname (if (> (length (main-args)) 1)
                                   (if (find "(.*/)" (real-path ((main-args) 1)) 1) $1 nil))))
                  (or 
                    (catch (load arg) 'r) 
                    (catch (load (append dirname arg)) 'r)
                    (catch (module arg) 'r)
                    (catch (load (append "/usr/lib/bps/" arg)) 'r)
                    (catch (load (append "/usr/local/lib/bps/" arg)) 'r)          
                    ))))

(define-macro (def-static)
     (let (temp (append (lambda) (list (1 (args 0)) (args 1))))
         (def-new 'temp (sym (args 0 0) (args 0 0)))))


(define (expr2xml expr (level 0))
        (cond 
          ((or (atom? expr) (quote? expr))
           (print (dup "  " level))
           (println expr))
          ((list? (first expr))
           (expr2xml (first expr) (+ level 1))
           (dolist (s (rest expr)) (expr2xml s (+ level 1))))
          ((symbol? (first expr))
           (print (dup "  " level))
           (println "<" (first expr) ">")
           (dolist (s (rest expr)) (expr2xml s (+ level 1)))
           (print (dup "  " level))
           (println "</" (first expr) ">"))
          (true
            (print (dup "  " level) 
                   (println "<error>" (string expr) "<error>")))
          ))


(define-macro (letb _binds _body)
              (letn ((_src (append '(local) (list (eval _binds)) 
                                  (list (list 'bind (list 'transpose (list 'list _binds _body)))) 
                                  (args))))
                    (eval _src)))

(define-macro (if-let _body)
              (eval
                (list
                  'let
                  _body
                  (append
                    (list
                      'if
                      (_body 0)
                      )
                    $args)
                  )
                ))


(define-macro (when-let _body)
              (eval
                (list
                  'let
                  _body
                  (append
                    (list
                      'when
                      (_body 0)
                      )
                    $args)
                  )
                ))

(define-macro (catch-default _expr _dft)
              (letex (_expr _expr)
                     (local (result)
                            (if (catch _expr 'result)
                                result
                                (eval _dft)))))

(define (joinx str-joint bool-trail-joint)
        (if (not (string? str-joint))
            (setq str-joint ""))
        (join (map string (clean nil? (flat $args)))
              str-joint
              bool-trail-joint))

;;; depend of mydevice
(define-macro (fp)
              (let (_body (eval (cons format $args)))
                (if (string? mydevice)
                    (extend mydevice _body "\n")
                    (println _body))))


;;; depend of mydevice
(define-macro (hp _head)
              (letn ((_head (eval _head))
                     (_tail ((parse (replace "[<>]" (copy _head) " " 1)) 0))
                     
                     (_tail (join (map (curry format "</%s>") 
                                     (reverse 
                                       (map rest  
                                            (filter (fn (x) (starts-with x "<")) 
                                                    (parse (replace ">" (copy _head) " " 1))))))))
                     )
                    (if (string? mydevice)
                        (extend mydevice (string _head) "\n")
                        (println _head))
                    (dolist (arg (args))
                      (eval arg)
                      )
                    (if (string? mydevice)
                        (extend mydevice (string _tail) "\n")
                        (println _tail))
                    
                    
                    ))


(define (html-response-type type)
        (if (not type)
            (setq type "text/html"))
        (if (is-cgi?)
            (if (not html-response-type-is-print)
                (begin
                  (print (format "Content-Type: %s\r\n\r\n" type))
                  (setq html-response-type-is-print 1)))))

;;; cgi functions

(define (is-cgi?)
        (env "QUERY_STRING"))


(define (cgi-init)
        (if (is-cgi?)
            (module "cgi.lsp")))

(define (cgi-index)
        (html-response-type)
        (hp "<html><body>"
            (if cgi_functions
                (map (fn (x)
                         (fp
                           {<div><a href="?fun=%s">%s</a></div>} (string x) (string x)
                           )
                         )
                     cgi_functions
                     )
                )
            )
        )

(define (cgi-main)
        (cgi-init)
        (if (is-cgi?)
            (letn ((fun (CGI:get "fun"))
                   (funs (if cgi_functions (map string cgi_functions)))
                   )
                  (if (and funs (find fun funs))
                      (eval-string (format "(%s)" fun))
                      (cgi-index)
                      )
                  ) ) )

;;; main

(define (main)
        (letn ((funs (filter (fn (x) (if (lambda? (eval x)))) (symbols)))
               (fun (if (>= (length (main-args)) 3) ((main-args) 2)))
               (params (slice (main-args) 3))
               (cmd (if fun (append (list (sym fun)) params)))
               )
              (if (and fun (find (sym fun) funs))
                  (begin
                    (println cmd)
                    (let (c (eval cmd))
                      (println (dup "=" 120) "\n" c)))
                  (map (fn (x)
                           (println (format "%02d. %s" $idx (string x))))
                       funs)
                  )
              )
        )
;;; tools

(define (mktemp type)
        (letn (
               f (join (map char (randomize (sequence (char "a") (char "z")))))
               f (if type (append f "." type) f)
               check-file (fn (path)
                              (if (write-file path "") path))
               )
              (or 
                (check-file (append "/run/shm/" f))
                (check-file (append "/tmp/" f)))))


;;;debug tools

(define (type x)
  (let (types 
         '("bool" "bool" "integer" "float" 
           "string" "symbol" "context" "primitive" 
           "import-simple" "import-libffi" "quote" "list" "lambda" 
           "fexpr" "array"))
    (types (& 0xf ((dump x) 1)))))

(global 'type)


;;; aux tools

(define (get-mycolor color_key range)
        (if (not get-mycolor-once)
            (begin
              (seed 12345)
              (new Tree 'MyColor)
              (setq get-mycolor-once 1)))
        (if (not range)
            (setq range '(120 256)))
        (letn ((my-rand (fn (range cnt)
                            (if (number? cnt)
                                (dotimes (i cnt)
                                  (extend '() (list (my-rand range))))
                                (+ (first range) (rand (- (last range) (first range)))))))
               (a (my-rand range 3))
               (color (apply + (map (fn (x) (* x (pow 256 $idx))) a)))
               (color (if (MyColor color_key)
                          (MyColor color_key)
                          (MyColor color_key color))))
              color))

(define (node-main)
        (when (not lispdg)
          (main)
          (exit)
          )
        )