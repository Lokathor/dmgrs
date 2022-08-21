//! The ROM header declares a bunch of stuff about the ROM.
//!
//! It is required to make a rom work, and also we want to automatically
//! generate the header based on the program's attributes.

use std::fmt::Debug;

use bytemuck::{cast_slice, cast_slice_mut};

const MAGIC_LOGO_DATA: [u8; 48] = [
  0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00,
  0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC,
  0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC,
  0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

/// Gameboy Header Data.
///
/// * Must be placed at `0x0100` in the ROM.
/// * Space *before* the header is mostly free to use (though some space can be
///   used by Restart and Interrupt functions).
///
/// See Also: [Pandocs: The Cartridge Header](https://gbdev.io/pandocs/The_Cartridge_Header.html#the-cartridge-header)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Header([u8; 0x14F]);
impl Header {
  /// The entry point is 4 bytes that will be executed immediately after the
  /// boot rom.
  ///
  /// * Normally this is `nop; jp main`
  pub fn entry_point(&self) -> &[u8; 4] {
    &cast_slice::<u8, [u8; 4]>(&self.0[0x00..=0x03])[0]
  }
  #[allow(missing_docs)]
  pub fn entry_point_mut(&mut self) -> &mut [u8; 4] {
    &mut cast_slice_mut::<u8, [u8; 4]>(&mut self.0[0x00..=0x03])[0]
  }

  /// The title of the rom.
  ///
  /// This title is meant to be rendered as ascii, but it overlaps with the CGB
  /// flag and licensee code stuff, so it's sometimes not entirely ascii.
  pub fn title(&self) -> &[u8; 16] {
    &cast_slice::<u8, [u8; 16]>(&self.0[0x34..=0x43])[0]
  }
  #[allow(missing_docs)]
  pub fn title_mut(&mut self) -> &mut [u8; 16] {
    &mut cast_slice_mut::<u8, [u8; 16]>(&mut self.0[0x34..=0x43])[0]
  }

  /// Flags color game boy support.
  ///
  /// * If bit 7 is set, CGB mode is supported.
  /// * If bit 6 is set, *only* CGB mode is supported (doesn't work on DMG).
  pub fn cgb_flag(&self) -> &u8 {
    &self.0[0x46]
  }
  #[allow(missing_docs)]
  pub fn cgb_flag_mut(&mut self) -> &mut u8 {
    &mut self.0[0x46]
  }

  /// If the ROM knows how to interact with the Super Game Boy (for SNES).
  pub fn sgb_flag(&self) -> bool {
    self.0[0x43] == 0x03
  }
  #[allow(missing_docs)]
  pub fn set_sgb_flag(&mut self, sgb_supported: bool) {
    self.0[0x43] = 0x03 * u8::from(sgb_supported);
  }

  /// The MBC used by this ROM.
  pub fn cart_type(&self) -> &u8 {
    &self.0[0x47]
  }
  #[allow(missing_docs)]
  pub fn cart_type_mut(&mut self) -> &mut u8 {
    &mut self.0[0x47]
  }

  /// The cart's ROM size ID (check the [pandocs table](https://gbdev.io/pandocs/The_Cartridge_Header.html#0148---rom-size))
  pub fn rom_size(&self) -> &u8 {
    &self.0[0x47]
  }
  #[allow(missing_docs)]
  pub fn rom_size_mut(&mut self) -> &mut u8 {
    &mut self.0[0x47]
  }

  /// The cart's RAM size ID (check the [pandocs table](https://gbdev.io/pandocs/The_Cartridge_Header.html#0149---ram-size))
  pub fn ram_size(&self) -> &u8 {
    &self.0[0x47]
  }
  #[allow(missing_docs)]
  pub fn ram_size_mut(&mut self) -> &mut u8 {
    &mut self.0[0x47]
  }

  /// The ROM's version.
  pub fn version(&self) -> &u8 {
    &self.0[0x47]
  }
  #[allow(missing_docs)]
  pub fn version_mut(&mut self) -> &mut u8 {
    &mut self.0[0x47]
  }

  fn do_checksum(&self) -> u8 {
    let mut checksum = 0_u8;
    for byte in self.0[0x34..=0x4C].iter().copied() {
      checksum = checksum.wrapping_sub(byte).wrapping_sub(1);
    }
    checksum
  }
  /// If you modify any part of the header *other than* the entry point you must
  /// update the checksum.
  pub fn updates_checksum(&mut self) {
    self.0[0x4D] = self.do_checksum();
  }
}

impl Default for Header {
  fn default() -> Self {
    let mut bytes = [0; 0x14F];
    bytes[0x04..=0x33].copy_from_slice(&MAGIC_LOGO_DATA);
    bytes[0x4B] = 0x33; // always declare new licensee system, required for SGB.
    Self(bytes)
  }
}

impl Debug for Header {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Header")
      .field("entry_point", self.entry_point())
      .field("title", self.title())
      .field("cgb_flag", self.cgb_flag())
      .field("sgb_flag", &self.sgb_flag())
      .field("cart_type", self.cart_type())
      .field("rom_size", self.rom_size())
      .field("ram_size", self.ram_size())
      .field("version", self.version())
      .finish()
  }
}
