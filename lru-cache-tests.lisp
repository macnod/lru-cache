;; Tests for the lru-cache package

(require :dc-dlist)
(require :fiveam)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :lru-cache)

(defpackage :lru-cache-tests (:use :cl :fiveam :dc-dlist :lru-cache))

(in-package :lru-cache-tests)

(def-suite lru-cache-tests
  :description "Test suite for lru-cache")

(in-suite lru-cache-tests)

(test create-cache
  "Test creating an LRU cache"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    (is (not (null cache)))
    (is (= 0 (cache-size cache)))
    (is (= 3 (cache-max-size cache)))))

(test put-and-get
  "Test basic cache-put and cache-get operations"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    ;; Put some values
    (cache-put "key-1" "value-1" cache)
    (cache-put "key-2" "value-2" cache)
    (cache-put "key-3" "value-3" cache)
    
    ;; Check size
    (is (= 3 (cache-size cache)))
    
    ;; Get values
    (multiple-value-bind (val found)
      (cache-get "key-1" cache)
      (is (equal "value-1" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (cache-get "key-2" cache)
      (is (equal "value-2" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (cache-get "key-3" cache)
      (is (equal "value-3" val))
      (is (eq t found)))))

(test get-nonexistent
  "Test getting a nonexistent key"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    (multiple-value-bind (val found) (cache-get "nonexistent" cache)
      (is (null val))
      (is (null found)))))

(test update-existing
  "Test updating an existing key"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    (cache-put "key-1" "value-1" cache)
    (cache-put "key-1" "updated-1" cache)
    
    ;; Size should still be 1
    (is (= 1 (cache-size cache)))
    
    ;; Value should be updated
    (multiple-value-bind (val found) (cache-get "key-1" cache)
      (is (equal "updated-1" val))
      (is (eq t found)))))

(test eviction
  "Test that least recently used items are evicted"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    ;; Fill the cache
    (cache-put "key-1" "value-1" cache)
    (cache-put "key-2" "value-2" cache)
    (cache-put "key-3" "value-3" cache)
    
    ;; Add a 4th item, should evict key-1 (least recent)
    (cache-put "key-4" "value-4" cache)
    
    ;; Check size is still 3
    (is (= 3 (cache-size cache)))
    
    ;; key-1 should be evicted
    (multiple-value-bind (val found) (cache-get "key-1" cache)
      (declare (ignore val))
      (is (null found)))
    
    ;; Others should still exist
    (multiple-value-bind (val found) (cache-get "key-2" cache)
      (is (equal "value-2" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (cache-get "key-4" cache)
      (is (equal "value-4" val))
      (is (eq t found)))))

(test large-scale-eviction
  "Test that least-recently-used items are evicted, on a large scale"
  (loop
    with size = 100 and elements = 100000
    with cache = (make-instance 'lru-cache :max-size size)
    for a from 1 to elements
    for key = (format nil "key-~6,'0d" a)
    for value = (format nil "value-~6,'0d" a)
    collect key into keys
    do (cache-put key value cache)
    finally
    (is-true (equal size (cache-size cache))
      "Expected cache size to be " size)
    (is-true (equal size (dc-dlist:len (lru-cache::cache-list cache)))
      "Expected cache list length to be " size)
    (is-true (equal size (hash-table-count (lru-cache::cache-table cache)))
      "Expected cache table entry-count to be " size)
    (is-true (loop for a from 1 to (- elements size)
               for key = (pop keys)
               never (multiple-value-bind (node found)
                       (cache-get key cache)
                       (declare (ignore node))
                       found))
      "The oldest ~d elements were evicted" (- elements size))
    (is-true (loop for a from (1+ (- elements size)) to elements
               for key = (pop keys)
               always (multiple-value-bind (node found)
                        (cache-get key cache)
                        (declare (ignore node))
                        found))
    "The newest ~d elements are remain in the cache" size)))

(test lru-order-on-get
  "Test that accessing an item makes it most recent"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    ;; Fill the cache
    (cache-put "key-1" "value-1" cache)
    (cache-put "key-2" "value-2" cache)
    (cache-put "key-3" "value-3" cache)
    
    ;; Access key-1 to make it most recent
    (cache-get "key-1" cache)
    
    ;; Add a 4th item, should evict key-2 (now least recent)
    (cache-put "key-4" "value-4" cache)
    
    ;; key-2 should be evicted
    (multiple-value-bind (val found) (cache-get "key-2" cache)
      (declare (ignore val))
      (is (null found)))
    
    ;; key-1 should still exist (was accessed)
    (multiple-value-bind (val found) (cache-get "key-1" cache)
      (is (equal "value-1" val))
      (is (eq t found)))))

(test lru-order-on-put
  "Test that updating an item makes it most recent"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    ;; Fill the cache
    (cache-put "key-1" "value-1" cache)
    (cache-put "key-2" "value-2" cache)
    (cache-put "key-3" "value-3" cache)
    
    ;; Update key-1 to make it most recent
    (cache-put "key-1" "updated-1" cache)
    
    ;; Add a 4th item, should evict key-2 (now least recent)
    (cache-put "key-4" "value-4" cache)
    
    ;; key-2 should be evicted
    (multiple-value-bind (val found) (cache-get "key-2" cache)
      (declare (ignore val))
      (is (null found)))
    
    ;; key-1 should still exist with updated value
    (multiple-value-bind (val found) (cache-get "key-1" cache)
      (is (equal "updated-1" val))
      (is (eq t found)))))

(test different-value-types
  "Test storing different types of values"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    ;; Store different types
    (cache-put "string-key" "string-value" cache)
    (cache-put "number-key" 42 cache)
    (cache-put "list-key" '(1 2 3) cache)
    
    ;; Retrieve and check
    (multiple-value-bind (val found) (cache-get "string-key" cache)
      (is (equal "string-value" val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (cache-get "number-key" cache)
      (is (= 42 val))
      (is (eq t found)))
    
    (multiple-value-bind (val found) (cache-get "list-key" cache)
      (is (equal '(1 2 3) val))
      (is (eq t found)))))

(test test-function
  "Test using a different test function"
  (let ((cache (make-instance 'lru-cache :max-size 3 :test-function #'eql)))

    ;; Fill the cache
    (cache-put :key-1 "value-1" cache)
    (cache-put :key-2 "value-2" cache)
    (cache-put :key-3 "value-3" cache)
    (cache-put :key-4 "value-4" cache)
    (cache-put :key-4 "value-4a" cache)
    (cache-put :key-4 "value-4b" cache)

    ;; Ensure that :key-1 was evicted
    (multiple-value-bind (val found) (cache-get :key-1 cache)
      (declare (ignore val))
      (is-false found ":key-1 should have been evicted"))

    ;; The rest of the keys should be there
    (loop for key in (list :key-2 :key-3 :key-4)
      for val in (list "value-2" "value-3" "value-4b")
      always (multiple-value-bind (v found) (cache-get key cache)
               (is-true found)
               (is (equal v val)))))

  (let ((cache (make-instance 'lru-cache :max-size 3 :test-function #'eql)))

    ;; Fill the cache
    (cache-put "key-1" "value-1" cache)
    (cache-put "key-2" "value-2" cache)
    (cache-put "key-3" "value-3" cache)
    (cache-put "key-4" "value-4" cache)
    (cache-put "key-4" "value-4a" cache)
    (cache-put "key-4" "value-4b" cache)

    ;; Keys 1-3 should have been evicted, because key-4 does not match itself
    (loop for key in (list "key-1" "key-2" "key-3")
      for value in (list "value-1" "value-2" "value-3")
      always (multiple-value-bind (v found) (cache-get key cache)
               (declare (ignore v))
               (is-false found)))

    ;; key-4 will never be found
    (multiple-value-bind (v found) (cache-get "key-4" cache)
      (is-false v)
      (is-false found))))

(test cache-size
  "Test functions for monitoring the size of the cache"
  (let ((cache (make-instance 'lru-cache :max-size 4)))
    (is (= 4 (cache-max-size cache)))
    (is (= 0 (cache-size cache)))
    (is (= 0.0 (cache-usage cache)))
    (cache-put "key-1" "value-1" cache)
    (is (= 1 (cache-size cache)))
    (is (= 0.25 (cache-usage cache)))
    (cache-put "key-2" "value-2" cache)
    (is (= 2 (cache-size cache)))
    (is (= 0.50 (cache-usage cache)))
    (cache-put "key-3" "value-3" cache)
    (is (= 3 (cache-size cache)))
    (is (= 0.75 (cache-usage cache)))
    (cache-put "key-4" "value-4" cache)
    (is (= 4.00 (cache-size cache)))
    (is (= 1.00 (cache-usage cache)))
    (cache-put "key-5" "value-5" cache)
    (is (= 4 (cache-size cache)))
    (is (= 1.00 (cache-usage cache)))
    (is (= 4 (cache-max-size cache)))))

(test cache-remove
  "Test removing items from the cache"
  (let* ((size 10)
          (current-size 0)
          (cache (make-instance 'lru-cache :max-size size))
          (first-key nil)
          (middle-key nil)
          (last-key nil))
    (loop for a from 1 to (1- size)
      for key = (format nil "key-~a" a)
      for key-first = key then key-first
      for key-middle = (if (= a (floor size 2)) key key-middle)
      for key-last = key
      do
      (cache-put (format nil "key-~a" a) (format nil "value-~a" a) cache)
      (incf current-size)
      finally
      (setf
        first-key key-first
        middle-key key-middle
        last-key key-last))
    (is (= size (cache-max-size cache)))
    (is (= current-size (cache-size cache)))
    ;; Remove a key from the middle
    (cache-remove middle-key cache)
    (decf current-size)
    (is (= current-size (cache-size cache)))
    (multiple-value-bind (val found) (cache-get middle-key cache)
      (is-false val)
      (is-false found))
    ;; Remove first key
    (cache-remove first-key cache)
    (decf current-size)
    (is (= current-size (cache-size cache)))
    (multiple-value-bind (val found) (cache-get first-key cache)
      (is-false val)
      (is-false found))
    ;; Remove last key
    (cache-remove last-key cache)
    (decf current-size)
    (is (= current-size (cache-size cache)))
    (multiple-value-bind (val found) (cache-get last-key cache)
      (is-false val)
      (is-false found))
    ;; Ensure other keys are still present
    (loop for a from 1 to (1- size)
      for key = (format nil "key-~a" a)
      unless (member key (list first-key middle-key last-key) :test #'equal)
      do (multiple-value-bind (val found) (cache-get key cache)
           (is-true found)
           (is (equal val (format nil "value-~a" a)))))
    ;; Reinsert the middle key
    (cache-put middle-key "value-middle" cache)
    (incf current-size)
    (is (= current-size (cache-size cache)))
    (multiple-value-bind (val found) (cache-get middle-key cache)
      (is-true found)
      (is (equal val "value-middle")))
    ;; Fill beyond capacity
    (loop for a from current-size to (+ size 3)
      for b = (+ size a)
      for key = (format nil "key-~a" b)
      for value = (format nil "value-~a" b)
      do
      (cache-put key value cache)
      when (< current-size size) do (incf current-size))
    (is (= current-size (cache-size cache)))
    (is (= (cache-max-size cache) (cache-size cache)))
    ;; Check return value of remove the middle-key again
    (is-true (cache-remove middle-key cache))
    (decf current-size)
    (is (= current-size (cache-size cache)))
    (multiple-value-bind (val found) (cache-get middle-key cache)
      (is-false val)
      (is-false found))
    ;; Check return value of removing a nonexistent key
    (is-false (cache-remove "nonexisting-key" cache))))

(test cache-op-counts
  "Test that cache operation counts are tracked correctly"
  (let ((cache (make-instance 'lru-cache :max-size 3)))
    (is-true (zerop (cache-hits cache)))
    (is-true (zerop (cache-misses cache)))
    (is-true (zerop (cache-requests cache)))
    (is-true (zerop (cache-insertions cache)))
    (is-true (zerop (cache-updates cache)))
    (is-true (zerop (cache-evictions cache)))
    (is-true (zerop (cache-removals cache)))
    ;; Insert a new item
    (cache-put "key-1" "value-1" cache)
    (is-true (zerop (cache-hits cache)))
    (is-true (zerop (cache-misses cache)))
    (is-true (zerop (cache-requests cache)))
    (is (= 1 (cache-insertions cache)))
    (is-true (zerop (cache-updates cache)))
    (is-true (zerop (cache-evictions cache)))
    (is-true (zerop (cache-removals cache)))
    ;; Update existing item
    (cache-put "key-1" "value-1-updated" cache)
    (is-true (zerop (cache-hits cache)))
    (is-true (zerop (cache-misses cache)))
    (is-true (zerop (cache-requests cache)))
    (is (= 1 (cache-insertions cache)))
    (is (= 1 (cache-updates cache)))
    (is-true (zerop (cache-evictions cache)))
    (is-true (zerop (cache-removals cache)))
    ;; Get existing item
    (cache-get "key-1" cache)
    (is (= 1 (cache-hits cache)))
    (is (= 1 (cache-requests cache)))
    ;; Get nonexistent item
    (cache-get "key-2" cache)
    (is (= 1 (cache-hits cache)))
    (is (= 1 (cache-misses cache)))
    (is (= 2 (cache-requests cache)))
    ;; Fill cache to trigger eviction
    (cache-put "key-2" "value-2" cache)
    (cache-put "key-3" "value-3" cache)
    (is (= 3 (cache-insertions cache)))
    (cache-put "key-4" "value-4" cache)
    (is (= 4 (cache-insertions cache)))
    (is (= 1 (cache-evictions cache)))
    ;; Remove an item
    (cache-remove "key-4" cache)
    (is (= 1 (cache-removals cache)))
    ;; Remove nonexistent item
    (cache-remove "nonexistent" cache)
    (is (= 1 (cache-removals cache)))))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
