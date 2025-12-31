(in-package #:lru-cache)

;;; LRU Cache Implementation
;;;
;;; Uses a hash table for O(1) lookups and a doubly-linked list
;;; (via dc-dlist) to maintain LRU order.
;;;
;;; The hash table maps keys to dlist nodes. Each node stores the key
;;; and value and.
(defclass lru-cache ()
  ((cache-table
    :accessor cache-table
    :documentation "Hash table mapping keys to dlist nodes")
    (cache-list
      :initform (make-instance 'dl:dlist)
      :reader cache-list
      :documentation "Doubly-linked list maintaining LRU order")
    (cache-max-size
      :initarg :max-size
      :initform 100
      :reader cache-max-size
      :documentation
      "Maximum number of entries before eviction. Defaults to 100.")
    (test-function
      :initarg :test-function
      :initform #'equal
      :reader test-function
      :documentation
      "Test function for the cache-table hash table. Defaults to EQUAL."))
  (:documentation "LRU Cache with bounded size and automatic eviction."))

(defmethod initialize-instance :after ((cache lru-cache) &key)
  "Initialize the cache-table hash table with the specified test function and size."
  (setf
    (cache-table cache) (make-hash-table
                          :test (test-function cache)
                          :size (1+ (cache-max-size cache)))))

(defmethod cache-size ((cache lru-cache))
  "Return the current number of entries in the cache."
  (dl:len (cache-list cache)))

(defmethod cache-usage ((cache lru-cache))
  "Return the usage ratio of the cache (current size / max size)."
  (if (zerop (cache-max-size cache))
    0.0
    (/ (float (cache-size cache)) (cache-max-size cache))))

(defmethod cache-put (key value (cache lru-cache))
  "Insert or update a key-value pair in the cache. If the key exists, update its
value and move it to the front (most recent). If the key is new and cache is at
max-size, evict the least recently used entry."
  (let ((cache-table (cache-table cache))
         (cache-list (cache-list cache)))
    (multiple-value-bind (node found)
      (gethash key cache-table)
      (if found
        (progn
          (dl:delete-node cache-list node)
          (dl:push-head cache-list (list :key key :value value))
          (setf (gethash key cache-table) (dl:head cache-list)))
        (progn
          (when (>= (dl:len cache-list) (cache-max-size cache))
            (let* ((evicted-payload (dl:pop-tail cache-list))
                    (evicted-key (getf evicted-payload :key)))
              (remhash evicted-key cache-table)))
          (dl:push-head cache-list (list :key key :value value))
          (setf (gethash key cache-table) (dl:head cache-list))))))
  value)

(defmethod cache-get (key (cache lru-cache))
  "Retrieve a value from the cache by key. Returns two values: the cached value (or
NIL if not found) and T/NIL indicating whether the key was found.  When found,
moves the entry to the front (most recent)."
  (let ((cache-table (cache-table cache))
         (cache-list (cache-list cache)))
    (multiple-value-bind (node found)
      (gethash key cache-table)
      (if found
        (let* ((payload (dl:value node))
                (key (getf payload :key))
                (value (getf payload :value)))
          (dl:delete-node cache-list node)
          (dl:push-head cache-list (list :key key :value value))
          (setf (gethash key cache-table) (dl:head cache-list))
          (values value t))
        (values nil nil)))))

(defmethod cache-remove (key (cache lru-cache))
  "Remove a key-value pair from the cache by key. Returns T if the key was found
and removed, NIL otherwise."
  (let ((cache-table (cache-table cache))
         (cache-list (cache-list cache)))
    (multiple-value-bind (node found)
      (gethash key cache-table)
      (when found
        (dl:delete-node cache-list node)
        (remhash key cache-table)
        t))))
