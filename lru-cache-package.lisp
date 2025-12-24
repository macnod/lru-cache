;;;; package.lisp

(defpackage #:lru-cache
  (:use :cl)
  (:local-nicknames
    (:dl :dc-dlist))
  (:export
    :lru-cache
    :cache-put
    :cache-get
    :cache-size
    :cache-max-size
    ))
