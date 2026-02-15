//! Shared LRU cache and hashing utilities.
//!
//! Provides a generic `LruCache<K, V>` with true LRU access-order tracking
//! and a `hash_str` helper for non-cryptographic string hashing.
//!
//! Used by `pikchr_renderer`, `mermaid_renderer`, and `markdown_renderer`.

use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};

// ---------------------------------------------------------------------------
// hash_str — non-cryptographic string hashing for cache keys
// ---------------------------------------------------------------------------

/// Hash a string to a u64 using the standard library's DefaultHasher.
/// Used for cache key generation -- not cryptographic.
pub(crate) fn hash_str(s: &str) -> u64 {
    let mut h = DefaultHasher::new();
    s.hash(&mut h);
    h.finish()
}

// ---------------------------------------------------------------------------
// LruCache<K, V> — generic LRU cache with access-order tracking
// ---------------------------------------------------------------------------

/// Generic LRU cache that evicts the least-recently-used entry when capacity
/// is reached. The `get` method updates access order (true LRU, not FIFO).
///
/// Fields are `pub(crate)` to support white-box testing in consumer modules
/// (e.g., mermaid_renderer tests that verify internal state).
pub(crate) struct LruCache<K, V> {
    pub(crate) entries: HashMap<K, V>,
    pub(crate) order: VecDeque<K>,
    capacity: usize,
}

impl<K, V> LruCache<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            capacity: capacity.max(1),
        }
    }

    pub(crate) fn get(&mut self, key: &K) -> Option<V> {
        let value = self.entries.get(key).cloned();
        self.touch_if_present(&value, key);
        value
    }

    pub(crate) fn insert(&mut self, key: K, value: V) {
        if self.entries.contains_key(&key) {
            self.entries.insert(key.clone(), value);
            self.touch(&key);
            return;
        }
        while self.entries.len() >= self.capacity {
            if !self.evict_oldest() {
                break;
            }
        }
        self.order.push_back(key.clone());
        self.entries.insert(key, value);
    }

    pub(crate) fn remove(&mut self, key: &K) {
        self.entries.remove(key);
        self.order.retain(|entry| entry != key);
    }

    pub(crate) fn clear(&mut self) {
        self.entries.clear();
        self.order.clear();
    }

    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    pub(crate) fn touch_if_present(&mut self, value: &Option<V>, key: &K) {
        if value.is_some() {
            self.touch(key);
        }
    }

    pub(crate) fn evict_oldest(&mut self) -> bool {
        if let Some(old) = self.order.pop_front() {
            self.entries.remove(&old);
            true
        } else {
            false
        }
    }

    fn touch(&mut self, key: &K) {
        self.order.retain(|entry| entry != key);
        self.order.push_back(key.clone());
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lru_cache_insert_and_get() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.insert("c".to_string(), 3);
        assert_eq!(cache.get(&"a".to_string()), Some(1));
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.get(&"c".to_string()), Some(3));
        assert_eq!(cache.get(&"d".to_string()), None);
    }

    #[test]
    fn test_lru_cache_eviction() {
        let mut cache = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.insert("c".to_string(), 3);
        assert_eq!(cache.get(&"a".to_string()), None);
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.get(&"c".to_string()), Some(3));
    }

    #[test]
    fn test_lru_cache_access_order() {
        let mut cache = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        let _ = cache.get(&"a".to_string());
        cache.insert("c".to_string(), 3);
        assert_eq!(cache.get(&"a".to_string()), Some(1));
        assert_eq!(cache.get(&"b".to_string()), None);
        assert_eq!(cache.get(&"c".to_string()), Some(3));
    }

    #[test]
    fn test_lru_cache_clear() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        assert_eq!(cache.len(), 2);
        cache.clear();
        assert_eq!(cache.len(), 0);
        assert_eq!(cache.get(&"a".to_string()), None);
    }

    #[test]
    fn test_lru_cache_remove() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.remove(&"a".to_string());
        assert_eq!(cache.get(&"a".to_string()), None);
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn test_hash_str_deterministic() {
        let h1 = hash_str("hello");
        let h2 = hash_str("hello");
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_str_different_inputs() {
        let h1 = hash_str("hello");
        let h2 = hash_str("world");
        assert_ne!(h1, h2);
    }
}
