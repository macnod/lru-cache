;;;; package.lisp

(defpackage #:lru-cache
  (:use :cl)
  (:local-nicknames
    (:dl :dc-dlist))
  (:export
    :cache-get
    :cache-max-size
    :cache-put
    :cache-remove
    :cache-size
    :cache-usage
    :lru-cache
    ))
