pub trait KeyValueStore<K, V> {
    fn get_type(&mut self, idx: &K) -> (K, V);
    fn put_type(&mut self, t: V) -> K;
}

pub trait KeyValueStore2<K, V> {
    fn get_type(&mut self, idx: &K) -> V;
    fn put_type(&mut self, t: V) -> K;
}
