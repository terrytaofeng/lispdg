#!/usr/bin/env newlisp

(if (not load-module)
    (module "bps.lsp"))

(define (gnuplot-file m)
        (when-let (f (mktemp))
                  (let (text (cond
                               ((or (list? m) (array? m)) 
                                (join (map (fn (l)
                                               (if (list? l)
                                                   (join (map string l) " ")
                                                   (string l)))
                                           m)
                                      "\n" 
                                      true))
                               (true (string m))))
                    (write-file f text)
                    f)))

(define-macro (gnuplot _text)
              (letn ((_text (eval _text))
                     (_result nil)
                     (_cmd (format "gnuplot <<EOF\n%s\nEOF" 
                                   (if (find "set term " _text 1)
                                       _text
                                       (extend "set term svg;\n" _text))
                                   ))
                     )
                    (if (catch (exec _cmd) '_result)
                        (if (list? _result)
                            (join _result "")
                            (string _result))
                        "")))



(define-macro (matlab _text)
              (letn ((_text (eval _text))
                     (_result ""))
                    (when-let (_f (mktemp "svg"))
                              (let (_cmd (format "octave --no-gui --no-window-system  <<EOF\nhold on;\n%s\nprint -dsvg %s;\nEOF" _text _f))
                                (if (catch (exec _cmd) '_result)
                                    (setq _result (read-file _f))))
                              (delete-file _f)
                              )
                    _result))