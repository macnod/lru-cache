;;;; lru-cache.asd

(asdf:defsystem #:lru-cache
  :description "LRU Cache library for Common Lisp"
  :author "macnod"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:dc-dlist)
  :components ((:file "package")
               (:file "lru-cache"))
  :in-order-to ((test-op (test-op :lru-cache/tests))))

(asdf:defsystem #:lru-cache/tests
  :description "Tests for lru-cache"
  :author "macnod"
  :license "MIT"
  :depends-on (#:lru-cache #:fiveam)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam :run! :lru-cache-tests)))
