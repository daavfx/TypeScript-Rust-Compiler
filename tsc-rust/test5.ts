interface Repository<T> { findById(id: number): T | null; }
