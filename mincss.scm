;; mincss - a set of SXML rules for the mincss styling library (http://mincss.com)
;;
;; Copyright (c) 2017, Arthur Maciel
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the author nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY ERESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(module mincss

(mincss-rules
 sxml->html
 mincss-cdn-url)

(import chicken scheme ports)

(use srfi-1
     sxml-transforms
     sxpath-lolevel)

(define mincss-cdn-url
  (make-parameter "https://cdn.jsdelivr.net/min/1.5/min.min.css"))

;; Help procedures
(define (merge-attribute-lists lst1 lst2)
  (let* ((lst1-keys (map car lst1))
         (lst2-keys (map car lst2))
         (diff-keys (lset-xor eq? lst1-keys lst2-keys))
         (same-keys (lset-intersection eq? lst1-keys lst2-keys)))
    (append 
     (map (lambda (attr)
            (or (assoc attr lst1) (assoc attr lst2)))
          diff-keys)
     (map (lambda (attr)
            (list attr (string-append (cadr (assoc attr lst1))
                                      " "
                                      (cadr (assoc attr lst2)))))
          same-keys))))

(define (merge-attributes new-attribute-list element)
  (let* ((current-attribute-list (sxml:attr-list element))
         (attribute-list (if (null? current-attribute-list) 
                             new-attribute-list
                             (merge-attribute-lists current-attribute-list
                                                    new-attribute-list)))
         (body (if (null? (cddr element))
                   '()
                   (caddr element))))
    (list (car element)
          (append '(@) attribute-list)
          body)))

;; A SXML rule is of the form `(sxml-tag . (lambda (tag body) ...))
(define (make-rule/merged-attributes prototype)
  (lambda (tag body)
    (let ((new-attribute-list (sxml:attr-list (cons tag body))))
      (if (null? new-attribute-list)
          (if (null? body)
              (prototype)
              (prototype (car body)))
          (merge-attributes new-attribute-list
                            (if (null? (cdr body))
                                (prototype)
                                (prototype (cadr body))))))))

;; Prototypes should have the following form:
;; '(sxml-tag . (lambda body `(html-tag (@ (attributes ...)) ,@body)))
(define (description->prototype description)
  (let ((sxml-tag (car description))
        (html-tag (cadr description))
        (classes (caddr description)))
    (cons sxml-tag
          (lambda body
            `(,html-tag (@ (class ,classes))
                        ,@body)))))

(define (prototype->rule prototype)
  `(,(car prototype) . ,(make-rule/merged-attributes (cdr prototype))))

(define mincss-tag-descriptions
  `((btn          a             "btn")
    (btn-a        a             "btn btn-a")
    (btn-a-sm     a             "btn btn-a btn-sm")
    (btn-b        a             "btn btn-b")
    (btn-b-sm     a             "btn btn-b btn-sm")
    (btn-c        a             "btn btn-c")
    (btn-c-sm     a             "btn btn-c btn-sm")
    (addon        span          "addon")
    (table        table         "table")
    (ico          i             "ico")
    (container    div           "container")
    (row          div           "row")
    (c1           div           "col c1")
    (c2           div           "col c2")
    (c3           div           "col c3")
    (c4           div           "col c4")
    (c5           div           "col c5")
    (c6           div           "col c6")
    (c7           div           "col c7")
    (c8           div           "col c8")
    (c9           div           "col c9")
    (c10          div           "col c10")
    (c11          div           "col c11")
    (c12          div           "col c12")
    (msg          div           "msg")))

(define mincss-misc-prototypes
  `((nav . ,(lambda body `((nav (@ (class "nav")
                              (tabindex "-1")
                              (onclick "this.focus()"))
                           ,@body)
                      (button (@ (class "btn-close btn btn-sm"))
                              "x"))))
    (link-cdn-mincss . ,(lambda body
                          `(link (@ (rel "stylesheet")
                                    (href ,(mincss-cdn-url))
                                    (type "text/css")))))
    (meta-viewport . ,(lambda body
                        '(meta (@ (name "viewport")
                                  (content "width=device-width, initial-scale=1")))))))

(define mincss-all-prototypes
  (append (map description->prototype mincss-tag-descriptions)
          mincss-misc-prototypes))

(define mincss-rules
  (map (lambda (prot)
         (prototype->rule prot))
       mincss-all-prototypes))

(define rules (append `((literal *preorder* . ,(lambda (t b) b)))
                      mincss-rules
                      alist-conv-rules*))

(define sxml->html
  (make-parameter
   (lambda (sxml)
    (with-output-to-string
      (lambda ()
        (SRV:send-reply (pre-post-order*
                         (pre-post-order* sxml rules)
                         universal-conversion-rules*))))))))
