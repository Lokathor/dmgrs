use dmgrs::lexer::DmgToken;
use logos::Logos;

const SRC: &str = include_str!("../samples/hello_world.s");

fn main() {
  let mut eol_count = 0;
  for t in DmgToken::lexer(SRC) {
    match t {
      DmgToken::EndOfLine => {
        if eol_count == 0 {
          print!("{t:?}");
          eol_count += 1;
        } else {
          eol_count += 1;
        }
      }
      other => {
        if eol_count != 0 {
          if eol_count > 1 {
            print!(" x{eol_count}");
          }
          println!();
          eol_count = 0;
        }
        println!("{other:?}");
      }
    }
  }
}
