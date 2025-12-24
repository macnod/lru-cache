# lru-cache

LRU Cache library for Common Lisp

## Description

A Common Lisp implementation of an LRU (Least Recently Used) cache using a hash table for O(1) lookups and a doubly-linked list (via [dc-dlist](https://github.com/macnod/dc-dlist)) to maintain LRU order.

## Features

- **Fast lookups**: O(1) average-case time complexity for both `get` and `put` operations
- **Automatic eviction**: Automatically evicts least recently used items when cache reaches maximum size
- **Simple API**: Easy-to-use interface with `put` and `get` methods
- **Flexible**: Store any type of values with any type of keys

## Installation

This library depends on [dc-dlist](https://github.com/macnod/dc-dlist). Make sure it's available in your Quicklisp or ASDF configuration.

```lisp
(ql:quickload :lru-cache)
```

## Usage

### Basic Example

```lisp
(use-package :lru-cache)

;; Create a cache with maximum size of 3
(defparameter *cache* (make-lru-cache :max-size 3))

;; Add items to the cache
(put "key1" "value1" *cache*)
(put "key2" "value2" *cache*)
(put "key3" "value3" *cache*)

;; Retrieve items from the cache
(get "key1" *cache*)  ; => "value1", T

;; When cache is full, adding a new item evicts the least recently used
(put "key4" "value4" *cache*)
(get "key2" *cache*)  ; => NIL, NIL (evicted as least recently used)

;; Accessing an item makes it most recently used
(get "key1" *cache*)
(put "key5" "value5" *cache*)
(get "key3" *cache*)  ; => NIL, NIL (evicted, key1 was made recent by get)
```

### API

#### `make-lru-cache &key (max-size 100)`

Creates a new LRU cache with the specified maximum size.

#### `put key value cache`

Inserts or updates a key-value pair in the cache. If the key already exists, updates its value and moves it to the front (most recent). If the cache is at maximum size and a new key is added, evicts the least recently used entry.

Returns the value.

#### `get key cache`

Retrieves a value from the cache by key. Returns two values:
- The cached value (or NIL if not found)
- T if the key was found, NIL otherwise

When a key is found, it's moved to the front (most recent).

#### `cache-size cache`

Returns the current number of entries in the cache.

#### `cache-max-size cache`

Returns the maximum size of the cache.

## Running Tests

```lisp
(ql:quickload :lru-cache/tests)
(asdf:test-system :lru-cache)
```

## License

MIT License - See LICENSE file for details
