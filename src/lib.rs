#![feature(custom_attribute)]
#![feature(plugin)]
#![feature(rand)]
#![feature(collections)]
#![feature(convert)]

#![feature(core)]
extern crate core;
extern crate rand;

use std::io::{self, Write};
use std::iter::{Iterator, FromIterator,Peekable};
use std::{error,fmt};
use std::rc::Rc;

#[derive(PartialEq, Eq,Debug, Clone)]
pub enum SexpToken {
  OpenParen,
  CloseParen,
  Atom(Vec<u8>)
}

struct EncodingIterator<I: Iterator> {
  iter: I
}

pub fn encode<'a, I>(it : I) -> Vec<u8> where I : Iterator<Item=&'a SexpToken> {
  let mut out = vec![];
  for t in it {
    match t {
      &SexpToken::OpenParen => out.push('(' as u8),
      &SexpToken::CloseParen => out.push(')' as u8),
      &SexpToken::Atom(ref s) => {
	out.push_all(format!("{}", s.len()).into_bytes().as_slice());
	out.push(':' as u8);
	out.push_all(s.as_slice())
      }
    }
  }
  out
}

pub struct TokenisingIterator<I: Iterator<Item=u8>> {
  iter: Peekable<I>
}

impl<'a, I> Iterator for TokenisingIterator<I> where I :Iterator<Item=u8> {
  type Item = SexpToken;
  fn next(&mut self) -> Option<SexpToken> {
    let current = self.iter.peek().map(|p|*p);
    let r = match current {
      Some(c) if c == '(' as u8 => { self.iter.next(); Some(SexpToken::OpenParen) }
    , Some(c) if c == ')' as u8 => { self.iter.next(); Some(SexpToken::CloseParen) }
    , Some(c) if c >= '0' as u8 && c <= '9' as u8 => { let s = read_atom(&mut self.iter); Some(SexpToken::Atom(s)) }
    , None => None
    , peeked => panic!("Unexpected char in sexp tokenise: {:?}", peeked)
    };
    // writeln!(std::io::stderr(),"TokenisingIterator::next: peek:{:?} => {:?}" , current.map(|c| c as char), r);
    r
  }
}


pub fn tokenise<I:Iterator<Item=u8>>(it : I) -> TokenisingIterator<I> {
  TokenisingIterator { iter: it.peekable() }
}

fn read_atom<'a, I>(it : &mut Peekable<I>) -> Vec<u8> where I : Iterator<Item=u8> {
//  writeln!(std::io::stderr(),"read_string: {:?}" , it.peek());
  let mut len = 0;
  while let Some(c) = it.next() {
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
  let bytes : Vec<u8> = it.take(len).collect();
  bytes
}

#[derive(Clone, Debug, PartialEq)]
pub enum SexpInfo {
  Atom(Vec<u8>),
  List(Rc<Vec<SexpInfo>>)
}

#[derive(PartialEq, Eq,Debug, Clone)]
pub enum Error {
  UnexpectedToken(SexpToken),
  EofError
}

impl SexpInfo {
  pub fn write_to<W: io::Write>(&self, wr: &mut W) -> Result<(), io::Error> {
    match self {
      &SexpInfo::Atom(ref v) => {
	try!(wr.write_all(format!("{}", v.len()).into_bytes().as_slice()));
	try!(wr.write_all(b":"));
	try!(wr.write_all(v.as_slice()));
	Ok(())
      },
      &SexpInfo::List(ref l) => {
	try!(wr.write_all(b"("));
	for exp in l.iter() {
	  try!(exp.write_to(wr))
	}
	wr.write_all(b")")
      }
    }
  }
  pub fn read_from<R: io::Read>(mut rdr: R) -> Result<SexpInfo, Error> {
    let mut bytes_or_errs = rdr.bytes();
    let mut bytes = bytes_or_errs.map(Result::unwrap);
    let mut it = tokenise(bytes).peekable();
    SexpInfo::parse_expr(&mut it)
  }

  fn parse_expr<I: Iterator<Item=SexpToken>>(mut it: &mut Peekable<I>) -> Result<SexpInfo, Error> {
    // writeln!(std::io::stderr(),"parse_expr: {:?}" , it.peek());
    match(it.next()) {
      Some(SexpToken::OpenParen) => SexpInfo::parse_list_remainder(it),
      Some(SexpToken::Atom(s)) => Ok(SexpInfo::Atom(s)),
      Some(t) => Err(Error::UnexpectedToken(t)),
      None => Err(Error::EofError)
    }
  }

  fn parse_list_remainder<I: Iterator<Item=SexpToken>>(it: &mut Peekable<I>) -> Result<SexpInfo, Error> {
    // writeln!(std::io::stderr(),"parse_list_remainder: {:?}" , it.peek());
    let mut list = vec![];
    loop {
      // writeln!(std::io::stderr(),"parse_list_remainder:loop: {:?}" , it.peek());
      match it.peek() {
	Some(&SexpToken::CloseParen) => {
	  let _ = it.next();
	  return Ok(SexpInfo::List(Rc::new(list)))
	},
	None => return Err(Error::EofError),
	_ => {
	  let exp = try!(SexpInfo::parse_expr(it));
	  // writeln!(std::io::stderr(),"parse_list_member -> {:?}" , exp);
	  list.push(exp)
	}
      }
    }
  }
}
