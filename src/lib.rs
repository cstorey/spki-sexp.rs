#![feature(custom_attribute)]
#![feature(plugin)]
#![allow(dead_code)]
#![plugin(quickcheck_macros)]
#![feature(rand)]
#![feature(collections)]
#![feature(convert)]

extern crate rand;
extern crate quickcheck;

use rand::Rng;

use quickcheck::{quickcheck, Arbitrary, Gen};
use quickcheck as qc;

use std::io::Write;
use std::iter::{FromIterator,Peekable};

#[derive(PartialEq, Eq,Debug, Clone)]
enum SexpToken {
  OpenParen,
  CloseParen,
  Str(String)
}

impl qc::Arbitrary for SexpToken {
  fn arbitrary<G: qc::Gen>(g : &mut G) -> SexpToken {
    match u64::arbitrary(g) % 3 {
      0 => SexpToken::OpenParen,
      1 => SexpToken::CloseParen,
      2 => SexpToken::Str(qc::Arbitrary::arbitrary(g)),
      n => panic!("Unexpected value mod 3: {:?}", n)
    }
  } 

  fn shrink(&self) -> Box<Iterator<Item=SexpToken>+'static> {
//    writeln!(std::io::stderr(),"shrink: {:?}", self).unwrap();
    match *self {
      SexpToken::OpenParen => qc::empty_shrinker(),
      SexpToken::CloseParen => qc::single_shrinker(SexpToken::OpenParen),
      SexpToken::Str(ref x) => {
	let chained = qc::single_shrinker(SexpToken::CloseParen)
		      .chain(qc::single_shrinker(SexpToken::OpenParen))
		      .chain(x.shrink().map(SexpToken::Str));
	Box::new(chained)
      }
    }
  }
}

struct EncodingIterator<I: Iterator> {
  iter: I
}

fn encode<'a, I>(it : I) -> Vec<u8> where I : Iterator<Item=&'a SexpToken> {
  let mut out = vec![];
  for t in it {
    match t {
      &SexpToken::OpenParen => out.push('(' as u8),
      &SexpToken::CloseParen => out.push(')' as u8),
      &SexpToken::Str(ref s) => { 
	let bytes = s.clone().into_bytes();
	out.push_all(format!("{}", bytes.len()).into_bytes().as_slice());
	out.push(':' as u8);
	out.push_all(bytes.as_slice())
      }
    }
  }
  out
}

struct DecodingIterator<I: Iterator> {
  iter: Peekable<I>
}

impl<'a, I> Iterator for DecodingIterator<I> where I : Iterator<Item=&'a u8> {
  type Item = SexpToken;
  fn next(&mut self) -> Option<SexpToken> {
    let current = self.iter.peek().map(|cp| **cp);
    current.map(|c|
      match c {
	c if c == '(' as u8 => { self.iter.next(); SexpToken::OpenParen }
      , c if c == ')' as u8 => { self.iter.next(); SexpToken::CloseParen }
      , c if '0' as u8 <= c && c <= '9' as u8 => { let s = read_string(&mut self.iter); SexpToken::Str(s) }
      , _ => panic!("Unexpected char in sexp decode: {:?}", c)
      }
    )
  }
}


fn decode<'a, I>(it : I) -> DecodingIterator<I> where I : Iterator<Item=&'a u8> {
  DecodingIterator { iter: it.peekable() }
}

fn read_string<'a, I>(it : &mut Peekable<I>) -> String where I : Iterator<Item=&'a u8> {
//  writeln!(std::io::stderr(),"read_string: {:?}" , it.peek());
  let mut len = 0;
  while let Some(&c) = it.next() {
    if c <= '9' as u8 {
      let digit = c - '0' as u8;
      len = len * 10 + digit as usize;
//      writeln!(std::io::stderr(),"read_string! {:?}; len:{:?}" , c as char, len).unwrap();
    } else if c == ':' as u8 {
//      writeln!(std::io::stderr(),"read_string! Colon!");
      break;
    } else {
//      writeln!(std::io::stderr(),"read_string! wat: {:?}", c);
      panic!("Unexpected char in string: {:?}", c);
    }
  }

//  writeln!(std::io::stderr(),"read_string! len: {:?}", len);
  let bytes : Vec<u8> = it.take(len).map(|c| *c).collect();
  let s = String::from_utf8_lossy(bytes.as_slice());
//  writeln!(std::io::stderr(),"read_string! {:?}; peek:{:?}", s, it.peek());
  s.into_owned()
}

fn vec8_as_str(v :&Vec<u8>) -> String {
  String::from_utf8_lossy(v).into_owned()
}

#[quickcheck]
fn round_trip(toks : Vec<SexpToken>) -> bool {
//   writeln!(std::io::stderr(),"orig: {:?}", toks).unwrap();
  let encd = encode(toks.iter());
//   writeln!(std::io::stderr(),"{:?}", vec8_as_str(&encd)).unwrap();
  let res : Vec<_> = decode(encd.iter()).collect();
//   writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?} => {:?}", toks, vec8_as_str(&encd), res, res == toks).unwrap();
  res == toks
}
