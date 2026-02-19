//! An ordered map

use core::fmt;
use std::borrow::Borrow;

#[derive(Clone)]
pub struct OrdMap<K, V>(Vec<(K, V)>);

impl<K, V> FromIterator<(K, V)> for OrdMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<K: PartialEq, V> OrdMap<K, V> {
    pub fn get<Q: Borrow<K>>(&self, key: Q) -> Option<&V> {
        self.iter()
            .find_map(|(k, v)| if k == key.borrow() { Some(v) } else { None })
    }

    pub fn get_mut<Q: Borrow<K>>(&mut self, key: Q) -> Option<&mut V> {
        self.iter_mut()
            .find_map(|(k, v)| if k == key.borrow() { Some(v) } else { None })
    }

    pub fn insert(&mut self, key: K, value: V) {
        if let Some(existing) = self.get_mut(&key) {
            *existing = value;
        } else {
            self.0.push((key, value));
        }
    }
}

impl<K, V> Default for OrdMap<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K, V> OrdMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter().map(|(k, v)| (k, v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&mut K, &mut V)> {
        self.0.iter_mut().map(|(k, v)| (k, v))
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.0.iter().map(|(_, typ)| typ)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.0.iter_mut().map(|(_, typ)| typ)
    }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for OrdMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}
