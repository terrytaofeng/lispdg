#!/usr/bin/env newlisp
(module "bps.lsp")

(load-module "node.lsp")
(load-module "calltree.lsp")
(load-module "html.lsp")

(N:N-M)

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
  }
)



;;;;;;;;;;;;;;; main
(define (html-auto)
        (html-response-type)
        (hp "<html><body>"

            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GRAPH_SHOW "test1"
 (digraph GRAPH_DEFT_LAYOUT 
          
  (subgraph    { A B C} )

  (umlet 'A {
    A
    --
    hello#
    --
    data#

    })  
  (umlet 'B {
    B
    --
    op1()#op1#
    --
    op2()#op2#
    })  


  (calltree {#test
A -> B  a call b
 B -> C
 B -> D
  }{color="blue" fontcolor="blue"})

  (calltree {#test
A:hello -> B:op1
 B:op2 -> A:data
  }{color="red" fontcolor="red"} 3)




));;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ) (REF-PRINT))



(node-main)
