
<!-- MarkdownTOC autolink="true" autoanchor="false" bracket="round" -->

- [install newlisp and bps modules](#install-newlisp-and-bps-modules)
- [install graphviz](#install-graphviz)
- [create gallery](#create-gallery)
- [generate html](#generate-html)
- [install apache and enable cgi](#install-apache-and-enable-cgi)
- [lispdg.cgi config](#lispdgcgi-config)

<!-- /MarkdownTOC -->


## install newlisp and bps modules
- wget http://www.newlisp.org/downloads/newlisp_10.7.1-utf8_amd64.deb
- sudo dpkg -i newlisp_10.7.1-utf8_amd64.deb
- sudo ln ${this_project_dir}/bps.lsp /usr/local/share/newlisp/modules/bps.lsp -s
- sudo ln ${this_project_dir}/bps/    /usr/lib/bps -s



## install graphviz

sudo apt-get install graphviz
sudo apt-get install gnuplot


## create gallery
```
gallery/demo.pdg.lsp

....
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


```


 more pdg.lsp file can link to gallery/output/



## generate html

```
newlisp gallery/demo.pdg.lsp html-auto
```

## install apache and enable cgi


```
sudo apt-get install apache2

diff --git a/apache2/apache2.conf b/apache2/apache2.conf
index baf6d8a..020a428 100644
--- a/apache2/apache2.conf
+++ b/apache2/apache2.conf
@@ -161,10 +161,13 @@ Include ports.conf
        Require all granted
</Directory>
+
+AddHandler cgi-script .cgi
<Directory /var/www/>
-       Options Indexes FollowSymLinks
-       AllowOverride None
-       Require all granted
+        Options Indexes FollowSymLinks ExecCGI
+        AddHandler cgi-script .cgi .pl
+        AllowOverride None
+        Require all granted
</Directory>


sudo a2enmod cgi
sudo a2dismod mpm_event
sudo a2enmod mpm_prefork
sudo service apache2 restart

use browser open http://websit/lispdg.cgi
```

## lispdg.cgi config
```
ln lispdg.lsp lispdg.cgi -s


edit lispdg.lsp

;(define gallery_dir "/var/www/html/data/gitmap/"))
;(define modules_dir "/usr/lib/bps/")
;(define lispdg_file "/var/www/html/lispdg.cgi")

```