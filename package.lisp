;;;; package.lisp

(defpackage #:lru-cache
  (:use #:cl)
  (:export #:lru-cache
           #:make-lru-cache
           #:put
           #:get
           #:cache-size
           #:cache-max-size))
