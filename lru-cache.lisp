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
    :initform (make-hash-table :test 'equal)
    :accessor cache-table
    :documentation "Hash table mapping keys to dlist nodes")
   (cache-list
    :initform (make-instance 'dl:dlist)
    :accessor cache-list
    :documentation "Doubly-linked list maintaining LRU order")
   (max-size
    :initarg :max-size
    :initform 100
    :accessor cache-max-size
    :documentation "Maximum number of entries before eviction"))
  (:documentation "LRU Cache with bounded size and automatic eviction"))

(defun cache-size (cache)
  "Return the current number of entries in the cache."
  (dl:len (cache-list cache)))

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
