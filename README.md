# lru-cache

LRU Cache library for Common Lisp

## Description

A Common Lisp implementation of an LRU (Least Recently Used) cache using a hash table for O(1) lookups and a doubly-linked list (via [dc-dlist](https://github.com/macnod/dc-dlist)) to maintain LRU order.

**Note**: This code is changing quickly, so if you decide to use it, please pin your use to a release. Here's a Roswell example:

```sh
ros install macnod/lru-cache/v1.1
```

## Features

- **Fast lookups**: O(1) time complexity for both `cache-get` and `cache-put` operations
- **Automatic eviction**: Automatically evicts least recently used items when cache reaches maximum size
- **Simple API**: Easy-to-use interface with `cache-put` and `cache-get` methods
- **Flexible**: Store any type of values with any type of keys

## Installation

This library depends on [dc-dlist](https://github.com/macnod/dc-dlist). Make sure it's available in your Quicklisp or ASDF configuration.

```lisp
(ql:quickload :lru-cache)
```

Using Roswell:
```sh
ros install macnod/lru-cache/v1.1
```

## Usage

### Basic Example

```lisp
(use-package :lru-cache)

;; Create a cache with maximum size of 3
(defparameter *cache* (make-instance 'lru-cache :max-size 3))

;; Add items to the cache
(cache-put "key-1" "value-1" *cache*)
(cache-put "key-2" "value-2" *cache*)
(cache-put "key-3" "value-3" *cache*)

;; Retrieve items from the cache
(cach-get "key-1" *cache*)  ; => "value-1", nil

;; When cache is full, adding a new item evicts the least recently used
(cache-put "key-4" "value-4" *cache*)
(cache-get "key-2" *cache*)  ; => NIL, NIL (evicted as least recently used)

;; Accessing an item makes it most recently used
(cache-get "key-1" *cache*)
(cache-put "key-5" "value5" *cache*)
(cache-get "key-3" *cache*)  ; => NIL, NIL (evicted, key-1 was made recent by get)
```

### API

#### make-instance 'lru-cache :max-size SIZE :test-function TEST

Creates a new LRU cache with `SIZE` and `TEST`.

Init forms:
- `MAX-SIZE`: the maximum size of the cache. If the cache reaches `SIZE`, then a new entry causes the least-recently-used entry to be evicted, such that cache never exceeds `SIZE`. The default is 100.
- `TEST-FUNCTION`: The test function to use for the cache's internal hash table. One of `eq`, `eql`, `equal`, or `equalp`. The default is `equal`.

#### `cache-put key value cache`

Inserts or updates a key-value pair in the cache. If the key already exists, updates its value and moves it to the front (most recent). If the cache is at maximum size and a new key is added, evicts the least recently used entry.

Returns the value.

#### `cache-get key cache`

Retrieves a value from the cache by key. Returns two values:
- The cached value (or NIL if not found)
- T if the key was found, NIL otherwise

When a key is found, it's moved to the front (most recent).

#### `cache-size cache`

Returns the current number of entries in the cache.

#### `cache-max-size cache`

Returns the maximum size of the cache.

## Running Tests

```sh
make install-roswell      # first time, only if needed
make install-dependencies # first time, only if needed
make tests
```

## License

MIT License - See LICENSE file for details
