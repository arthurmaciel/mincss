(use test mincss sxml-transforms)

(define rules (append `((literal *preorder* . ,(lambda (t b) b)))
                      mincss-rules
                      alist-conv-rules*))
  
(test-group "mincss bultin tags"
  (test '(a (@ (href "#") (class "btn")) "Testing")  
        (pre-post-order* '(btn (@ (href "#")) "Testing") rules))
  
  (test '(a (@ (href "#") (class "btn btn-a")) "Testing")
        (pre-post-order* '(btn-a (@ (href "#")) "Testing") rules))
  
  (test '(a (@ (href "#") (class "btn btn-a btn-sm")) "Testing")
        (pre-post-order* '(btn-a-sm (@ (href "#")) "Testing") rules))
  
  (test '(a (@ (href "#") (class "btn btn-b")) "Testing")
        (pre-post-order* '(btn-b (@ (href "#")) "Testing") rules))
  
  (test '(a (@ (href "#") (class "btn btn-b btn-sm")) "Testing")
        (pre-post-order* '(btn-b-sm (@ (href "#")) "Testing") rules))
  
  (test '(a (@ (href "#") (class "btn btn-c")) "Testing")
        (pre-post-order* '(btn-c (@ (href "#")) "Testing") rules))
  
  (test '(a (@ (href "#") (class "btn btn-c btn-sm")) "Testing")
        (pre-post-order* '(btn-c-sm (@ (href "#")) "Testing") rules))
  
  (test '(span (@ (class "addon")) "Testing")
        (pre-post-order* '(addon "Testing") rules))
  
  (test '(table (@ (class "table")) "Testing")
        (pre-post-order* '(table "Testing") rules))
  
  (test '(i (@ (class "ico")) "Testing")
        (pre-post-order* '(ico "Testing") rules))
  
  (test '(div (@ (class "container")) "Testing")
        (pre-post-order* '(container "Testing") rules))
  
  (test '(div (@ (class "row")) "Testing")
        (pre-post-order* '(row "Testing") rules))
  
  (test '(div (@ (class "col c1")) "Testing")
        (pre-post-order* '(c1 "Testing") rules))
  
  (test '(div (@ (class "col c2")) "Testing")
        (pre-post-order* '(c2 "Testing") rules))
  
  (test '(div (@ (class "col c3")) "Testing")
        (pre-post-order* '(c3 "Testing") rules))
  
  (test '(div (@ (class "col c4")) "Testing")
        (pre-post-order* '(c4 "Testing") rules))
  
  (test '(div (@ (class "col c5")) "Testing")
        (pre-post-order* '(c5 "Testing") rules))
  
  (test '(div (@ (class "col c6")) "Testing")
        (pre-post-order* '(c6 "Testing") rules))
  
  (test '(div (@ (class "col c7")) "Testing")
        (pre-post-order* '(c7 "Testing") rules))
  
  (test '(div (@ (class "col c8")) "Testing")
        (pre-post-order* '(c8 "Testing") rules))
  
  (test '(div (@ (class "col c9")) "Testing")
        (pre-post-order* '(c9 "Testing") rules))
  
  (test '(div (@ (class "col c10")) "Testing")
        (pre-post-order* '(c10 "Testing") rules))
  
  (test '(div (@ (class "col c11")) "Testing")
        (pre-post-order* '(c11 "Testing") rules))
  
  (test '(div (@ (class "col c12")) "Testing")
        (pre-post-order* '(c12 "Testing") rules))
  
  (test '(div (@ (class "msg")) "Testing")
        (pre-post-order* '(msg "Testing") rules))
  
  (test '((nav (@ (class "nav") (tabindex "-1") (onclick "this.focus()")) ((a (@ (href "1") (class "pagename current")) "1") (a (@ (href "2")) "2") (a (@ (href "3")) "3"))) (button (@ (class "btn-close btn btn-sm")) "x"))
        (pre-post-order* '(nav ((a (@ (href "1") (class "pagename current")) "1") (a (@ (href "2")) "2") (a (@ (href "3")) "3"))) rules)))


(test-group "egg defined (pseudo)tags and procedures"
  (test '(meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
        (pre-post-order* '(meta-viewport) rules))
  
  (test `(link (@ (rel "stylesheet") (href ,(mincss-cdn-url)) (type "text/css")))
        (pre-post-order* '(link-cdn-mincss) rules))

  (test "<a href=\"#\" class=\"btn b\">Testing</a>"
        ((sxml->html) (pre-post-order* '(btn (@ (href "#") (class "b")) "Testing") rules))))

(test-exit)

