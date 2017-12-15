#!/usr/bin/env newlisp

;; debug functions
(define-macro (xdebug)
  (dolist (x (args))
    (println (string x) "[" (type (eval x)) "] : " (eval x))))


(define-macro (MAKE_DEBUG fun)
  (letex ((s fun)
          (name (string fun)))
    (if DEBUG
      (if DEBUG_LOG
        (define (s) (println name (args)))
        (define (s) nil)))))
