;;;; package.lisp

(defpackage #:lru-cache
  (:use :cl)
  (:local-nicknames
    (:dl :dc-dlist))
  (:export
    :cache-evictions
    :cache-get
    :cache-hits
    :cache-insertions
    :cache-max-size
    :cache-misses
    :cache-put
    :cache-removals
    :cache-remove
    :cache-requests
    :cache-size
    :cache-updates
    :cache-usage
    :lru-cache
    ))
