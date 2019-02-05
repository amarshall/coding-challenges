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

   65,  88,  74,  69,   0,
   89,  73,  68,  67,  72,
   84,  78,  77,  66,  82,
   76,   0,  80,  79,  89,
   71,  75,   0,  74,  70,
    0,   0,   0,   0,   0,
    0,   0,  97, 120, 106,
  101,   0, 121, 105, 100,
   99, 104, 116, 110, 109,
   98, 114, 108,   0, 112,
  111, 121, 103, 107,   0,
  106, 102,   0
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
