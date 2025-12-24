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
    :initform (dc-dlist:make-dlist)
    :accessor cache-list
    :documentation "Doubly-linked list maintaining LRU order (head = most recent)")
   (max-size
    :initarg :max-size
    :initform 100
    :accessor cache-max-size
    :documentation "Maximum number of entries before eviction"))
  (:documentation "LRU Cache with bounded size and automatic eviction"))

(defun make-lru-cache (&key (max-size 100))
  "Create a new LRU cache with the specified maximum size."
  (make-instance 'lru-cache :max-size max-size))

(defun cache-size (cache)
  "Return the current number of entries in the cache."
  (dc-dlist:len (cache-list cache)))

(defmethod put (key value (cache lru-cache))
  "Insert or update a key-value pair in the cache.
   If the key exists, update its value and move it to the front (most recent).
   If the key is new and cache is at max-size, evict the least recently used entry."
  (let ((cache-table (cache-table cache))
        (cache-list (cache-list cache)))
    
    (multiple-value-bind (node found) (gethash key cache-table)
      (if found
          ;; Key exists: update value and move to front
          (progn
            ;; Update the node's value
            (setf (dc-dlist:value node) value)
            ;; Remove node from its current position
            (dc-dlist:remove-node node cache-list)
            ;; Add it back at the head (most recent) with the key
            (dc-dlist:push-head key cache-list)
            ;; Update the hash table to point to the new head node
            (setf (gethash key cache-table) (dc-dlist:head cache-list)))
          
          ;; Key is new
          (progn
            ;; Check if we need to evict
            (when (>= (dc-dlist:len cache-list) (cache-max-size cache))
              ;; Evict least recently used (tail)
              (let ((evicted-key (dc-dlist:pop-tail cache-list)))
                (remhash evicted-key cache-table)))
            
            ;; Add new entry at head (most recent)
            (dc-dlist:push-head key cache-list)
            ;; Get the new head node and store it in the hash table
            (let ((new-node (dc-dlist:head cache-list)))
              (setf (dc-dlist:value new-node) value)
              (setf (gethash key cache-table) new-node))))))
  
  value)

(defmethod get (key (cache lru-cache))
  "Retrieve a value from the cache by key.
   Returns two values: the cached value (or NIL if not found) and T/NIL indicating whether the key was found.
   When found, moves the entry to the front (most recent)."
  (let ((cache-table (cache-table cache))
        (cache-list (cache-list cache)))
    
    (multiple-value-bind (node found) (gethash key cache-table)
      (if found
          ;; Key found: move to front and return value
          (let ((value (dc-dlist:value node)))
            ;; Remove from current position
            (dc-dlist:remove-node node cache-list)
            ;; Add back at head (most recent)
            (dc-dlist:push-head key cache-list)
            ;; Update hash table to point to the new head node
            (setf (gethash key cache-table) (dc-dlist:head cache-list))
            ;; Set the value on the new node
            (setf (dc-dlist:value (dc-dlist:head cache-list)) value)
            ;; Return value and found indicator
            (values value t))
          
          ;; Key not found
          (values nil nil)))))
