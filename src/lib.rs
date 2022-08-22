//#![warn(missing_docs)]
#![cfg_attr(test, allow(nonstandard_style))]
#![allow(unused_parens)]

//! Stuff to work with Dmgrs assembly files.

use std::{collections::HashMap, sync::atomic::AtomicUsize};

pub mod easy_lexer;
pub mod header;
pub mod lexer;
pub mod multi_line_comments;
pub mod parser;
pub mod repeated_newline_filter;
pub mod str_cache_impl;

type StaticStr = &'static str;

/// Generates a `usize` id value, used for sticking on the end of made up
/// labels.
pub fn next_id() -> usize {
  static GLOBAL_COUNTER: AtomicUsize = AtomicUsize::new(0);
  GLOBAL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Condition {
  Always,
  Zero,
  NonZero,
  Carry,
  NoCarry,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SectionLine {
  /// Label inside this section.
  Label(StaticStr),
  /// Relative jump to a label in this same section.
  JumpRelativeLabel(StaticStr, Condition),
  /// Relative jump to a resolved byte delta.
  JumpRelativeDelta(i8, Condition),
  /// `nop`, for testing purposes.
  Nop,
}
impl SectionLine {
  pub const fn byte_len(&self) -> usize {
    use SectionLine::*;
    match self {
      Label(_) => 0,
      JumpRelativeLabel(_, _) | JumpRelativeDelta(_, _) => 2,
      Nop => 1,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Section {
  lines: Vec<SectionLine>,
}
impl Section {
  /// The required size to fit in this section.
  pub fn required_size(&self) -> usize {
    self.lines.iter().map(SectionLine::byte_len).sum()
  }
  /// Resolves all relative label jumps into relative delta jumps
  /// ## Failure
  /// * If any relative label jumps point to missing labels then those missing
  ///   labels are returned.
  pub fn resolve_relative_label_jumps(
    &mut self,
  ) -> Result<(), Vec<RelativeJumpResolveError>> {
    let mut label_offsets: HashMap<StaticStr, usize> = HashMap::new();
    let mut current_offset = 0_usize;
    // Build the table of what labels are at what offset.
    for line in self.lines.iter() {
      if let SectionLine::Label(name) = line {
        label_offsets.insert(name, current_offset);
      }
      current_offset += line.byte_len();
    }
    let mut errors: Vec<RelativeJumpResolveError> = vec![];
    let mut current_offset = 0_usize;
    for line in self.lines.iter_mut() {
      if let SectionLine::JumpRelativeLabel(name, cond) = line {
        if let Some(target_offset) = label_offsets.get(name) {
          let zero_point = (current_offset as i32) + 2;
          let full_delta = (*target_offset) as i32 - zero_point;
          match i8::try_from(full_delta) {
            Ok(i) => *line = SectionLine::JumpRelativeDelta(i, *cond),
            Err(_) => errors.push(RelativeJumpResolveError::DeltaTooLarge {
              src: current_offset,
              dest: *target_offset,
            }),
          }
        } else {
          errors.push(RelativeJumpResolveError::LabelMissing(*name));
        }
      }
      current_offset += line.byte_len();
    }

    if errors.is_empty() {
      Ok(())
    } else {
      Err(errors)
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RelativeJumpResolveError {
  LabelMissing(StaticStr),
  DeltaTooLarge { src: usize, dest: usize },
}

#[test]
fn test_Section_required_size() {
  let s = Section {
    lines: vec![
      SectionLine::Label("main"),
      SectionLine::Label("main~loop0"),
      SectionLine::JumpRelativeLabel("main~loop0", Condition::Always),
    ],
  };
  assert_eq!(s.required_size(), 2);
  //
  let s = Section {
    lines: vec![
      SectionLine::Label("main"),
      SectionLine::Label("main~loop0"),
      SectionLine::Nop,
      SectionLine::JumpRelativeLabel("main~loop0", Condition::Always),
    ],
  };
  assert_eq!(s.required_size(), 3);
}

#[test]
fn test_Section_resolve_relative_label_jumps() {
  //
  {
    let expected = Section {
      lines: vec![
        SectionLine::Label("main"),
        SectionLine::Label("main~loop0"),
        SectionLine::JumpRelativeDelta(-2, Condition::Always),
      ],
    };
    let mut actual = Section {
      lines: vec![
        SectionLine::Label("main"),
        SectionLine::Label("main~loop0"),
        SectionLine::JumpRelativeLabel("main~loop0", Condition::Always),
      ],
    };
    actual.resolve_relative_label_jumps().unwrap();
    assert_eq!(expected, actual);
  }
  //
  {
    let expected = Section {
      lines: vec![
        SectionLine::Label("main"),
        SectionLine::Label("main~loop0"),
        SectionLine::Nop,
        SectionLine::JumpRelativeDelta(-3, Condition::Always),
      ],
    };
    let mut actual = Section {
      lines: vec![
        SectionLine::Label("main"),
        SectionLine::Label("main~loop0"),
        SectionLine::Nop,
        SectionLine::JumpRelativeLabel("main~loop0", Condition::Always),
      ],
    };
    actual.resolve_relative_label_jumps().unwrap();
    assert_eq!(expected, actual);
  }

  // check missing label case
  {
    let mut should_be_missing = Section {
      lines: vec![
        SectionLine::Label("main"),
        SectionLine::JumpRelativeLabel("FOOBAR", Condition::Always),
      ],
    };
    if let Err(list) = should_be_missing.resolve_relative_label_jumps() {
      let e = RelativeJumpResolveError::LabelMissing("FOOBAR");
      assert_eq!(vec![e], list);
    } else {
      panic!("missing label didn't report error: {should_be_missing:?}")
    }
  }

  // check too far back
  {
    let mut too_far_back = Section { lines: vec![SectionLine::Label("main")] };
    (0..150).for_each(|_| too_far_back.lines.push(SectionLine::Nop));
    too_far_back
      .lines
      .push(SectionLine::JumpRelativeLabel("main", Condition::Always));
    if let Err(list) = too_far_back.resolve_relative_label_jumps() {
      let e = RelativeJumpResolveError::DeltaTooLarge { src: 150, dest: 0 };
      assert_eq!(vec![e], list);
    } else {
      panic!("missing label didn't report error: {too_far_back:?}")
    }
  }

  // check too far forward
  {
    let mut too_far_forward = Section {
      lines: vec![SectionLine::JumpRelativeLabel("main", Condition::Always)],
    };
    (0..150).for_each(|_| too_far_forward.lines.push(SectionLine::Nop));
    too_far_forward.lines.push(SectionLine::Label("main"));
    if let Err(list) = too_far_forward.resolve_relative_label_jumps() {
      let e = RelativeJumpResolveError::DeltaTooLarge { src: 0, dest: 152 };
      assert_eq!(vec![e], list);
    } else {
      panic!("missing label didn't report error: {too_far_forward:?}")
    }
  }
}
