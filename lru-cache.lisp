;;;; lru-cache.lisp

(in-package #:lru-cache)

;;; LRU Cache Implementation
;;;
;;; Uses a hash table for O(1) lookups and a doubly-linked list
;;; (via dc-dlist) to maintain LRU order.
;;;
;;; The hash table maps keys to dlist nodes. Each node stores the key
;;; and has a value accessor for the cached value.

(defclass lru-cache ()
  ((cache-table
    :initform (make-hash-table :test 'equal)
    :accessor cache-table
    :documentation "Hash table mapping keys to dlist nodes")
   (cache-list
    :initform (make-instance 'dl:dlist)
    :accessor cache-list
    :documentation "Doubly-linked list maintaining LRU order (head = most recent)")
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
  "Insert or update a key-value pair in the cache.
   If the key exists, update its value and move it to the front (most recent).
   If the key is new and cache is at max-size, evict the least recently used entry."
  (let ((cache-table (cache-table cache))
        (cache-list (cache-list cache)))
    (multiple-value-bind (node found)
      (gethash key cache-table)
      (if found
          ;; Key exists: update value and move to front
        (progn
            ;; Remove node associated with key from its current position
            (dl:delete-node cache-list node)
            ;; Create a node at the head of the list with the value
            (dl:push-head cache-list (list :key key :value value))
            ;; Update the hash table to point to the new head node
            (setf (gethash key cache-table) (dl:head cache-list)))
        ;; Key is new
        (let ((evict (>= (dl:len cache-list) (cache-max-size cache))))
          ;; Check if we need to evict
          (when evict
            ;; Evict least recently used (tail)
            (let* ((evicted-payload (dl:pop-tail cache-list))
                    (evicted-key (getf evicted-payload :key)))
              (remhash evicted-key cache-table)))
          ;; Add new entry at head (most recent)
          (dl:push-head cache-list (list :key key :value value))
          ;; Get the new head node and store it in the hash table
          (setf (gethash key cache-table) (dl:head cache-list))))))
  value)

(defmethod cache-get (key (cache lru-cache))
  "Retrieve a value from the cache by key.
   Returns two values: the cached value (or NIL if not found) and T/NIL indicating whether the key was found.
   When found, moves the entry to the front (most recent)."
  (let ((cache-table (cache-table cache))
         (cache-list (cache-list cache)))
    (multiple-value-bind (node found)
      (gethash key cache-table)
      (if found
        ;; Key found: move to front and return value
        (let* ((payload (dl:value node))
                (key (getf payload :key))
                (value (getf payload :value)))
          ;; Remove from current position
          (dl:delete-node cache-list node)
          ;; Add back at head (most recent)
          (dl:push-head cache-list (list :key key :value value))
          ;; Get the new head node
          (setf (gethash key cache-table) (dl:head cache-list))
          ;; Return value and found indicator
          (values value t))

        ;; Key not found
        (values nil nil)))))
