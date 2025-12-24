;;;; lru-cache.asd

(asdf:defsystem :lru-cache
  :description "LRU Cache library for Common Lisp"
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (:dc-dlist)
  :components ((:file "lru-cache-package")
               (:file "lru-cache"))
  :in-order-to ((test-op (test-op :lru-cache/tests))))
