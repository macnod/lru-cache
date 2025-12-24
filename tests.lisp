;;;; tests.lisp

(defpackage #:lru-cache/tests
  (:use #:cl #:lru-cache #:fiveam))

(in-package #:lru-cache/tests)

(def-suite lru-cache-tests
  :description "Test suite for lru-cache")

(in-suite lru-cache-tests)

(test create-cache
  "Test creating an LRU cache"
  (let ((cache (make-lru-cache :max-size 3)))
    (is (not (null cache)))
    (is (= 0 (cache-size cache)))
    (is (= 3 (cache-max-size cache)))))

(test put-and-get
  "Test basic put and get operations"
  (let ((cache (make-lru-cache :max-size 3)))
    ;; Put some values
    (put "key1" "value1" cache)
    (put "key2" "value2" cache)
    (put "key3" "value3" cache)
    
    ;; Check size
    (is (= 3 (cache-size cache)))
    
    ;; Get values
    (multiple-value-bind (val found) (get "key1" cache)
      (is (equal "value1" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (get "key2" cache)
      (is (equal "value2" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (get "key3" cache)
      (is (equal "value3" val))
      (is (eq t found)))))

(test get-nonexistent
  "Test getting a nonexistent key"
  (let ((cache (make-lru-cache :max-size 3)))
    (multiple-value-bind (val found) (get "nonexistent" cache)
      (is (null val))
      (is (null found)))))

(test update-existing
  "Test updating an existing key"
  (let ((cache (make-lru-cache :max-size 3)))
    (put "key1" "value1" cache)
    (put "key1" "updated-value" cache)
    
    ;; Size should still be 1
    (is (= 1 (cache-size cache)))
    
    ;; Value should be updated
    (multiple-value-bind (val found) (get "key1" cache)
      (is (equal "updated-value" val))
      (is (eq t found)))))

(test eviction
  "Test that least recently used items are evicted"
  (let ((cache (make-lru-cache :max-size 3)))
    ;; Fill the cache
    (put "key1" "value1" cache)
    (put "key2" "value2" cache)
    (put "key3" "value3" cache)
    
    ;; Add a 4th item, should evict key1 (least recent)
    (put "key4" "value4" cache)
    
    ;; Check size is still 3
    (is (= 3 (cache-size cache)))
    
    ;; key1 should be evicted
    (multiple-value-bind (val found) (get "key1" cache)
      (is (null found)))
    
    ;; Others should still exist
    (multiple-value-bind (val found) (get "key2" cache)
      (is (equal "value2" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (get "key4" cache)
      (is (equal "value4" val))
      (is (eq t found)))))

(test lru-order-on-get
  "Test that accessing an item makes it most recent"
  (let ((cache (make-lru-cache :max-size 3)))
    ;; Fill the cache
    (put "key1" "value1" cache)
    (put "key2" "value2" cache)
    (put "key3" "value3" cache)
    
    ;; Access key1 to make it most recent
    (get "key1" cache)
    
    ;; Add a 4th item, should evict key2 (now least recent)
    (put "key4" "value4" cache)
    
    ;; key2 should be evicted
    (multiple-value-bind (val found) (get "key2" cache)
      (is (null found)))
    
    ;; key1 should still exist (was accessed)
    (multiple-value-bind (val found) (get "key1" cache)
      (is (equal "value1" val))
      (is (eq t found)))))

(test lru-order-on-put
  "Test that updating an item makes it most recent"
  (let ((cache (make-lru-cache :max-size 3)))
    ;; Fill the cache
    (put "key1" "value1" cache)
    (put "key2" "value2" cache)
    (put "key3" "value3" cache)
    
    ;; Update key1 to make it most recent
    (put "key1" "updated1" cache)
    
    ;; Add a 4th item, should evict key2 (now least recent)
    (put "key4" "value4" cache)
    
    ;; key2 should be evicted
    (multiple-value-bind (val found) (get "key2" cache)
      (is (null found)))
    
    ;; key1 should still exist with updated value
    (multiple-value-bind (val found) (get "key1" cache)
      (is (equal "updated1" val))
      (is (eq t found)))))

(test different-value-types
  "Test storing different types of values"
  (let ((cache (make-lru-cache :max-size 3)))
    ;; Store different types
    (put "string-key" "string-value" cache)
    (put "number-key" 42 cache)
    (put "list-key" '(1 2 3) cache)
    
    ;; Retrieve and check
    (multiple-value-bind (val found) (get "string-key" cache)
      (is (equal "string-value" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (get "number-key" cache)
      (is (= 42 val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (get "list-key" cache)
      (is (equal '(1 2 3) val))
      (is (eq t found)))))
