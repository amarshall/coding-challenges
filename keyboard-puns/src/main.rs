use std::collections::HashSet;
use std::io;
use std::io::prelude::*;

static QWERTY_TO_DVORAK: [u8; 123] = [
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,

  b'A', b'X', b'J', b'E',    0,
  b'U', b'I', b'D', b'C', b'H',
  b'T', b'N', b'M', b'B', b'R',
  b'L',    0, b'P', b'O', b'Y',
  b'G', b'K',    0, b'Q', b'F',
     0,    0,    0,    0,    0,
     0,    0, b'a', b'x', b'j',
  b'e',    0, b'u', b'i', b'd',
  b'c', b'h', b't', b'n', b'm',
  b'b', b'r', b'l',    0, b'p',
  b'o', b'y', b'g', b'k',    0,
  b'q', b'f',    0,
];


fn read_words() -> HashSet<String> {
  let stdin = io::stdin();
  let mut words: HashSet<String> = HashSet::new();
  for line in stdin.lock().lines() {
    words.insert(line.unwrap());
  };
  words
}

fn transform(input: String) -> Option<String> {
  let mut output: Vec<u8> = Vec::with_capacity(input.len());
  for byte in input.into_bytes() {
    if byte == 0 {
      return None
    }
    output.push(QWERTY_TO_DVORAK[byte as usize])
  }
  Some(String::from_utf8(output).unwrap())
}

fn main() {
  let words = read_words();
  for word in words.iter() {
    let pun = transform(word.to_owned());
    match pun {
      Some(pun_word) => {
        if words.contains(&pun_word) {
          println!("{} -> {}", word, pun_word)
        }
      }
      None => {}
    };
  };
}
