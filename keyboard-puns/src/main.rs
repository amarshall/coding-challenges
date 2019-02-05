use std::collections::HashSet;
use std::io;
use std::io::prelude::*;
use std::io::Write;
use std::str;
use std::sync::mpsc;
use std::thread;

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


fn read_words<T>(handle: T) -> Vec<String> where T: BufRead {
  let mut words: Vec<String> = Vec::with_capacity(1_000_000);
  for line in handle.lines() {
    words.push(line.unwrap());
  };
  words
}

fn transform(word: &[u8]) -> Vec<u8> {
  let mut output: Vec<u8> = Vec::with_capacity(word.len());
  for byte in word {
    let dvorak_char = QWERTY_TO_DVORAK[*byte as usize];
    output.push(dvorak_char)
  }
  output
}

fn valid_convert_from_qwerty(word: &[u8]) -> bool {
  for chr in word {
    if QWERTY_TO_DVORAK[chr.to_ascii_lowercase() as usize] == 0 {
      return false;
    }
  }
  true
}

fn valid_convert_from_dvorak(word: &[u8]) -> bool {
  for chr in word {
    let c = chr.to_ascii_lowercase();
    if c == b's' || c == b'v' || c == b'w' || c == b'z' {
      return false;
    }
  }
  true
}

fn spawn_printer(spool: mpsc::Receiver<String>) -> thread::JoinHandle<()> {
  thread::spawn(move || {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    loop {
      let res = spool.recv().or(Err(()))
        .and_then(|s| writeln!(handle, "{}", s).or(Err(())));
      match res {
        Ok(_) => (),
        Err(_) => break,
      };
    };
  })
}

fn run(spool: mpsc::Sender<String>) {
  let words = read_words(io::stdin().lock());
  let qwerty_words = words.iter()
    .map(|word| word.as_bytes())
    .filter(|word| valid_convert_from_qwerty(word));
  let dvorak_words: HashSet<&[u8]> = words.iter()
    .map(|word| word.as_bytes())
    .filter(|word| valid_convert_from_dvorak(word))
    .collect();

  for word in qwerty_words {
    let pun = transform(word);
    if dvorak_words.contains(&*pun) {
      let s = format!("{} -> {}", str::from_utf8(word).unwrap(), String::from_utf8(pun).unwrap());
      match spool.send(s) {
        Ok(_) => (),
        Err(_) => break,
      };
    }
  };
}

fn main() {
  let (spool_tx, spool_rx) = mpsc::channel::<String>();
  let printer = spawn_printer(spool_rx);
  run(spool_tx);
  printer.join().unwrap();
}
