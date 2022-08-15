#![allow(unsafe_code)]

//! Module with a very simple str caching implementation.

use std::{
  collections::HashSet,
  ptr::null_mut,
  sync::{
    atomic::{AtomicPtr, Ordering},
    PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard,
  },
};

use crate::StaticStr;

type HashSetStaticStr = HashSet<StaticStr>;
type RwLockHashSetStaticStr = RwLock<HashSetStaticStr>;

static STR_DB_PTR: AtomicPtr<RwLockHashSetStaticStr> =
  AtomicPtr::new(null_mut());

/// Gets a reference to the "database" of str values.
///
/// Most importantly, this will allocate a new RwLock on the first call.
fn get_str_db_rw_lock_ref() -> &'static RwLockHashSetStaticStr {
  let p: *mut RwLockHashSetStaticStr = STR_DB_PTR.load(Ordering::Acquire);
  if p.is_null() {
    let new_p: *mut RwLockHashSetStaticStr =
      Box::leak(Box::new(RwLock::default()));
    match STR_DB_PTR.compare_exchange(
      null_mut(),
      new_p,
      Ordering::AcqRel,
      Ordering::Acquire,
    ) {
      Ok(_old_p) => unsafe {
        // if ok, then old_p is a null, and we return the reference to new_p
        &*new_p
      },
      Err(old_p) => unsafe {
        // if err, someone else just initialized the str database, so we back
        // out our stuff and return using the `p` we just got.
        drop(Box::from_raw(new_p));
        &*old_p
      },
    }
  } else {
    unsafe { &*p }
  }
}

/// Turns any `&str` into a `&'static str` using a minimal number of new
/// allocations.
///
/// This works by utilizing a global set of interned `str` values. When a
/// request comes in for a str to be cached it is looked up in the set. If the
/// str is *not* already in the set then it's automatically added to the set.
/// Then a reference to the str in the set is returned to you.
///
/// The primary limitation of this system is that it's *only* able to grow. It
/// cannot shrink the intern set, since there's no way to know which str values
/// might still be in use. However, it's generally sufficient for short lived
/// programs.
pub fn cache_str(s: &str) -> &'static str {
  let db: &RwLockHashSetStaticStr = get_str_db_rw_lock_ref();
  let read_guard: RwLockReadGuard<_> =
    db.read().unwrap_or_else(PoisonError::into_inner);
  match read_guard.get(s) {
    // If the str is already in our database we just return that.
    Some(db_str) => *db_str,
    None => {
      // if the str is not in the database we have to drop our read lock and
      // upgrade to a write lock.
      drop(read_guard);
      let mut write_guard: RwLockWriteGuard<_> =
        db.write().unwrap_or_else(PoisonError::into_inner);
      let new_static_str: &'static str =
        Box::leak(s.to_string().into_boxed_str());
      // someone else might have put the str into the database before we could
      // acquire the write lock. `insert` returns `true` when the value was
      // actually added. If we get a `false` back then the str is in the
      // database, so we destroy the new static string we just made and then get
      // the one from the database.
      if write_guard.insert(new_static_str) {
        new_static_str
      } else {
        drop(unsafe {
          Box::from_raw(new_static_str as *const str as *mut str)
        });
        *write_guard.get(s).unwrap()
      }
    }
  }
}
