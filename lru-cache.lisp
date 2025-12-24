;;;; lru-cache.lisp

(in-package #:lru-cache)

;;; LRU Cache Implementation
;;;
;;; Uses a hash table for O(1) lookups and a doubly-linked list
;;; (via dc-dlist) to maintain LRU order.
;;;
;;; Each hash table entry stores both the value and a reference to
;;; the corresponding node in the doubly-linked list.

(defclass lru-cache ()
  ((hash-table
    :initform (make-hash-table :test 'equal)
    :accessor cache-hash-table
    :documentation "Hash table mapping keys to (value . dlist-node) pairs")
   (dlist
    :initform (dc-dlist:make-dlist)
    :accessor cache-dlist
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
  (dc-dlist:len (cache-dlist cache)))

(defmethod put (key value (cache lru-cache))
  "Insert or update a key-value pair in the cache.
   If the key exists, update its value and move it to the front (most recent).
   If the key is new and cache is at max-size, evict the least recently used entry."
  (let* ((hash-table (cache-hash-table cache))
         (dlist (cache-dlist cache))
         (existing-entry (gethash key hash-table)))
    
    (if existing-entry
        ;; Key exists: update value and move to front
        (let ((node (cdr existing-entry)))
          ;; Update the value in the hash table
          (setf (car existing-entry) value)
          ;; Remove node from its current position
          (dc-dlist:remove-node node dlist)
          ;; Add it back at the head (most recent)
          (let ((new-node (dc-dlist:push-head key dlist)))
            ;; Update the node reference in the hash table
            (setf (cdr existing-entry) new-node)))
        
        ;; Key is new
        (progn
          ;; Check if we need to evict
          (when (>= (dc-dlist:len dlist) (cache-max-size cache))
            ;; Evict least recently used (tail)
            (let ((evicted-key (dc-dlist:pop-tail dlist)))
              (remhash evicted-key hash-table)))
          
          ;; Add new entry at head (most recent)
          (let ((node (dc-dlist:push-head key dlist)))
            (setf (gethash key hash-table) (cons value node))))))
  
  value)

(defmethod get (key (cache lru-cache))
  "Retrieve a value from the cache by key.
   Returns two values: the cached value (or NIL if not found) and T/NIL indicating whether the key was found.
   When found, moves the entry to the front (most recent)."
  (let* ((hash-table (cache-hash-table cache))
         (dlist (cache-dlist cache))
         (entry (gethash key hash-table)))
    
    (if entry
        ;; Key found: move to front and return value
        (let ((value (car entry))
              (node (cdr entry)))
          ;; Remove from current position
          (dc-dlist:remove-node node dlist)
          ;; Add back at head (most recent)
          (let ((new-node (dc-dlist:push-head key dlist)))
            ;; Update node reference in hash table
            (setf (cdr entry) new-node))
          ;; Return value and found indicator
          (values value t))
        
        ;; Key not found
        (values nil nil))))
