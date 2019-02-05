use std::collections::HashSet;
use std::io;
use std::io::prelude::*;
use std::sync::Arc;
use std::thread;

use crossbeam::crossbeam_channel as channel;
use num_cpus;

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

fn process_word(words: &Arc<HashSet<String>>, word: String) -> Option<String> {
  let pun = transform(word.clone());
  match pun {
    Some(pun_word) => {
      if words.contains(&pun_word) {
        Some(format!("{} -> {}", word, pun_word))
      } else {
        None
      }
    }
    None => None
  }
}

fn spawn_worker(words: Arc<HashSet<String>>, feed: channel::Receiver<String>, printer: channel::Sender<String>) -> thread::JoinHandle<()> {
  thread::spawn(move || {
    loop {
      match feed.recv() {
        Ok(word) => {
          match process_word(&words, word) {
            Some(output) => printer.send(output).unwrap(),
            None => (),
          }
        }
        Err(_) => break
      }
    };
  })
}

fn spawn_workers(words: Arc<HashSet<String>>, feed: channel::Receiver<String>, printer: channel::Sender<String>) -> Vec<thread::JoinHandle<()>> {
  let ncpu = num_cpus::get();
  let mut workers: Vec<thread::JoinHandle<()>> = Vec::with_capacity(ncpu);

  for _i in 0..ncpu {
    let worker = spawn_worker(Arc::clone(&words), feed.clone(), printer.clone());
    workers.push(worker);
  };
  workers
}

fn spawn_printer(spool: channel::Receiver<String>) -> thread::JoinHandle<()> {
  thread::spawn(move || {
    loop {
      match spool.recv() {
        Ok(line) => println!("{}", line),
        Err(_) => break
      };
    };
  })
}

fn spawn_feeder(words: Arc<HashSet<String>>, feed: channel::Sender<String>) -> thread::JoinHandle<()> {
  thread::spawn(move || {
    for word in words.iter() {
      feed.send(word.to_owned()).unwrap();
    };
  })
}

fn main() {
  let (feed_tx, feed_rx) = channel::unbounded::<String>();
  let (print_tx, print_rx) = channel::unbounded::<String>();
  let words = Arc::new(read_words());

  let feeder = spawn_feeder(Arc::clone(&words), feed_tx);
  let workers = spawn_workers(words, feed_rx, print_tx);
  let printer = spawn_printer(print_rx);

  feeder.join().unwrap();
  for worker in workers {
    worker.join().unwrap();
  }
  printer.join().unwrap();
}
