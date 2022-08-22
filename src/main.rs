use dmgrs::{
  header::Header,
  section::{Condition, Section, SectionLine},
};
use tinyvec::SliceVec;

#[allow(dead_code)]
const SRC: &str = include_str!("../samples/minimum-program.s");

fn main() {
  let mut s_main = Section {
    lines: vec![
      SectionLine::Label("main"),
      SectionLine::Label("main~loop0"),
      SectionLine::Nop,
      SectionLine::JumpRelativeLabel("main~loop0", Condition::Always),
    ],
  };
  // If any relative jumps go to far, or point at missing labels, this fails
  s_main.resolve_relative_label_jumps().unwrap();

  // Generate a blank 32k block of memory
  let mut v = vec![0_u8; 0x8000];
  // place the header
  v[0x100..0x150].copy_from_slice(Header::default().as_bytes());
  // place the main section code
  let buf = SliceVec::from_slice_len(&mut v[0..s_main.required_size()], 0);
  s_main.write_to(buf);
  // trim blank bytes off the end of the rom
  while let Some(&0) = v.last() {
    v.pop();
  }
  // write out the result
  std::fs::write("target/demo.gb", &v).unwrap();
}
