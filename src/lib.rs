#![feature(custom_attribute)]
#![feature(plugin)]
#![allow(dead_code)]
#![plugin(quickcheck_macros)]
#![feature(rand)]
#![feature(collections)]
#![feature(convert)]

#![feature(custom_derive, plugin)]
#[plugin(serde_macros)]

extern crate rand;
extern crate quickcheck;

extern crate serde;

use serde::{ser,de};


use quickcheck::{Arbitrary, Gen};
use quickcheck as qc;

use std::io::{self, Write};
use std::iter::{Iterator, FromIterator,Peekable};
use std::{error,fmt};

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

struct TokenisingIterator<I: Iterator<Item=u8>> {
  iter: Peekable<I>
}

impl<'a, I> Iterator for TokenisingIterator<I> where I :Iterator<Item=u8> {
  type Item = SexpToken;
  fn next(&mut self) -> Option<SexpToken> {
    let current = self.iter.peek().map(|p|*p);
    match current {
      Some(c) if c == '(' as u8 => { self.iter.next(); Some(SexpToken::OpenParen) }
    , Some(c) if c == ')' as u8 => { self.iter.next(); Some(SexpToken::CloseParen) }
    , Some(c) if c >= '0' as u8 && c <= '9' as u8 => { let s = read_string(&mut self.iter); Some(SexpToken::Str(s)) }
    , None => None
    , peeked => panic!("Unexpected char in sexp tokenise: {:?}", peeked)
    }
  }
}


fn tokenise<I:Iterator<Item=u8>>(it : I) -> TokenisingIterator<I> {
  TokenisingIterator { iter: it.peekable() }
}

fn read_string<'a, I>(it : &mut Peekable<I>) -> String where I : Iterator<Item=u8> {
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
  let s = String::from_utf8_lossy(bytes.as_slice());
//  writeln!(std::io::stderr(),"read_string! {:?}; peek:{:?}", s, it.peek());
  s.into_owned()
}

fn vec8_as_str(v :&Vec<u8>) -> String {
  String::from_utf8_lossy(v).into_owned()
}

#[quickcheck]
fn round_trip_tokens(toks : Vec<SexpToken>) -> bool {
  use std::ops::Deref;
//   writeln!(std::io::stderr(),"orig: {:?}", toks).unwrap();
  let encd = encode(toks.iter());
//   writeln!(std::io::stderr(),"{:?}", vec8_as_str(&encd)).unwrap();
  let it : std::slice::Iter<u8> = encd.iter();
  let mut res = Vec::from_iter(tokenise(encd.iter().map(|p|*p)));
//   writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?} => {:?}", toks, vec8_as_str(&encd), res, res == toks).unwrap();
  res == toks
}

pub fn to_bytes<T>(value: &T) -> Vec<u8> {
  vec![0u8]
}

#[derive(Debug)]
enum Error {
  SyntaxError,
  UnexpectedTokenError(SexpToken),
  EofError,
  IoError(io::Error),
  InvalidNumber(std::num::ParseIntError),
  UnknownField(String),
  MissingField(String),
}

impl error::Error for Error {
  fn description(&self) -> &str {
    match *self {
      Error::SyntaxError => "Syntax error",
      Error::UnexpectedTokenError(_) => "Unexpected Token",
      Error::EofError => "EOF",
      Error::IoError(ref e) => error::Error::description(e),
      Error::InvalidNumber(ref e) => error::Error::description(e),
      Error::UnknownField(_) => "unknown field",
      Error::MissingField(_) => "missing field"
    }
  }

  fn cause(&self) -> Option<&error::Error> {
    None
  }
}

impl fmt::Display for Error {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "It broke. Sorry")
  }
}

impl de::Error for Error {
  fn syntax_error() -> Error {
    Error::SyntaxError
  }

  fn end_of_stream_error() -> Error {
    Error::EofError
  }

  fn unknown_field_error(field: &str) -> Error {
    Error::UnknownField(field.to_string())
  }

  fn missing_field_error(field: &'static str) -> Error {
    Error::MissingField(field.to_string())
  }
}

impl From<io::Error> for Error {
  fn from(error: io::Error) -> Error {
    Error::IoError(error)
  }
}

impl From<std::num::ParseIntError> for Error {
  fn from(error: std::num::ParseIntError) -> Error {
    Error::InvalidNumber(error)
  }
}

static SEQ: &'static str = "seq";
static UINT: &'static str = "uint";

struct Deserializer<I> where I : Iterator<Item=u8> {
  iter : Peekable<TokenisingIterator<I>>
}

impl<I> Deserializer<I> where I : Iterator<Item=u8> {
  fn new (iter: I) -> Deserializer<I> {
    Deserializer { iter: tokenise(iter).peekable() }
  }

  fn end(&self) -> Result<(), Error> {
    Ok(())
  }

  fn parse_list<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
    visitor.visit_seq(ListParser{ de: self })
  }

  fn parse_uint<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => visitor.visit_u64(try!(s.parse()))
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
    }
  }
}

impl<I> de::Deserializer for Deserializer<I> where I : Iterator<Item=u8> {
  type Error = Error;
  fn visit<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
    // writeln!(std::io::stderr(), "Deserializer::visit: {:?}", self.iter.peek());

    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => visitor.visit_str(s),
      Some(SexpToken::OpenParen) => {
	match self.iter.next() {
	  Some(SexpToken::Str(ref s)) if s == SEQ => self.parse_list(visitor)
	, Some(SexpToken::Str(ref s)) if s == UINT => self.parse_uint(visitor)
	// , Some(SexpToken::CloseParen) => self.visit_unit()
	, None => Err(Error::EofError)
	, Some(tok) => Err(Error::UnexpectedTokenError(tok))
	}
      }
      , Some(tok) => Err(Error::UnexpectedTokenError(tok))
      , None => Err(Error::EofError)
    }
  }

  fn visit_option<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_option").unwrap();
    visitor.visit_bool(false)
  }
  fn visit_seq<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_seq").unwrap();
    visitor.visit_bool(false)
  }
  fn visit_map<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_map").unwrap();
    visitor.visit_bool(false)
  }
  fn visit_named_unit<V>(&mut self, _name: &str, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_named_unit: {}", _name).unwrap();
    visitor.visit_bool(false)
  }
  fn visit_named_seq<V>(&mut self, _name: &str, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_named_seq: {}", _name).unwrap();
    visitor.visit_bool(false)
  }
  fn visit_named_map<V>(&mut self, _name: &str, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_named_map: {}", _name).unwrap();
    visitor.visit_bool(false)
  }
  /* fn visit_enum<V>(&mut self, _enum: &str, _visitor: V) -> Result<V::Value, Self::Error>
        where V: de::EnumVisitor {
    writeln!(std::io::stderr(), "Deserializer::visit_enum: {}", _enum);
    _visitor.visit(de::SeqDeserializer{de: self, iter: vec![].into_iter() })
  } */

  fn visit_bytes<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor {
    writeln!(std::io::stderr(), "Deserializer::visit_bytes").unwrap();
    visitor.visit_bool(false)
  }
}

struct ListParser<'a, I: Iterator<Item=u8> + 'a> {
  de: &'a mut Deserializer<I>
}

impl<'a, I: Iterator<Item=u8> + 'a> de::SeqVisitor for ListParser<'a, I> {
  type Error = Error;
  fn visit<T>(&mut self) -> Result<Option<T>, Error> where T : de::Deserialize {
    // writeln!(std::io::stderr(), "ListParser::visit: peek: {:?}", self.de.iter.peek()).unwrap();
    match self.de.iter.peek() {
      Some(&SexpToken::CloseParen) => {
	let _ = self.de.iter.next().unwrap();
	Ok(None)
      }
      None => Err(Error::EofError),
      _ => {
	let val = try!(de::Deserialize::deserialize(self.de));
	Ok(Some(val))
      }
    }
  }

  fn end(&mut self) -> Result<(), Error> {
    let cur = self.de.iter.peek().map(|p| p.clone());
    // writeln!(std::io::stderr(), "ListParser::end: peek: {:?}", self.de.iter.peek()).unwrap();
    Ok(())
  }
}

struct Serializer<W> {
  writer : W
}

impl<W> Serializer<W> where W : Write {
  fn new (wr: W) -> Serializer<W> {
    Serializer{ writer : wr }
  }

  fn write_str(&mut self, s: String) -> Result<(), Error> {
    let pfx = format!("{}:", s.len());
    try!(self.writer.write_all(pfx.into_bytes().as_slice()));
    try!(self.writer.write_all(s.into_bytes().as_slice()));
    Ok(())
  }

  fn open(&mut self) -> Result<(), Error> {
    Ok(try!(self.writer.write_all(b"(")))
  }
  fn close(&mut self) -> Result<(), Error> {
    Ok(try!(self.writer.write_all(b")")))
  }
}

impl<W> ser::Serializer for Serializer<W> where W: Write {
  type Error = Error;

  fn visit_unit(&mut self) -> Result<(), Error> {
    // writeln!(std::io::stderr(), "Serializer::visit_unit").unwrap();
    try!(self.open());
    try!(self.write_str(SEQ.to_string()));
    self.close()
  }
  fn visit_bool(&mut self, val: bool) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_bool: {:?}", val).unwrap();
    Ok(())
  }
  // TODO: Consider using display-hints for types of atoms.
  fn visit_u64(&mut self, v: u64) -> Result<(), Error> {
    // writeln!(std::io::stderr(), "Serializer::visit_u64: {:?}", v).unwrap();
    try!(self.open());
    try!(self.write_str(UINT.to_string()));
    try!(self.write_str(format!("{}", v)));
    self.close()
  }
  fn visit_i64(&mut self, v: i64) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_i64: {:?}", v).unwrap();
    Ok(())
  }
  fn visit_f64(&mut self, v: f64) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_f64: {:?}", v).unwrap();
    Ok(())
  }
  fn visit_str(&mut self, v: &str) -> Result<(), Error> {
    // writeln!(std::io::stderr(), "Serializer::visit_str: {:?}", v).unwrap();
    self.write_str(format!("{}", v))
  }
  fn visit_none(&mut self) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_none").unwrap();
    Ok(())
  }
  fn visit_some<T>(&mut self, v: T) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_some(?)").unwrap();
    Ok(())
  }
  fn visit_seq<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
    // writeln!(std::io::stderr(), "Serializer::visit_seq").unwrap();
    try!(self.open());
    try!(self.write_str(SEQ.to_string()));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }
  fn visit_seq_elt<T>(&mut self, v: T) -> Result<(), Error> where T: ser::Serialize {
    // writeln!(std::io::stderr(), "Serializer::visit_seq_elt").unwrap();
    v.serialize(self)
  }
  fn visit_map<V>(&mut self, mut visitor: V) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_map").unwrap();
    Ok(())
  }
  fn visit_map_elt<K, V>(&mut self, key: K, value: V) -> Result<(), Error> {
    writeln!(std::io::stderr(), "Serializer::visit_map_elt").unwrap();
    Ok(())
  }
}


fn from_bytes<T>(value: &[u8]) -> Result<T, Error> where T : de::Deserialize {
    let mut de = Deserializer::new(value.iter().map(|p|*p));
    let value = try!(de::Deserialize::deserialize(&mut de));
    // Make sure the whole stream has been consumed.
    try!(de.end());
    Ok(value)
}

fn as_bytes<T>(value: &T) -> Vec<u8> where T : ser::Serialize {
    let mut out = Vec::new();
    {
      let mut ser = Serializer::new(&mut out);
      value.serialize(&mut ser).unwrap();
    }
    out
}

fn round_trip_prop<T : fmt::Debug + ser::Serialize + de::Deserialize + PartialEq>(val: T) -> bool{
  let encd = as_bytes(&val);
  let dec = from_bytes::<T>(encd.as_slice());
  // writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?}", val, vec8_as_str(&encd), dec).unwrap();
  dec.unwrap() == val
}

#[quickcheck]
fn serde_round_trip_unit(val: ()) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_string(val: String) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_vec_string(val: Vec<String>) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_u64(val: u64) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_u8(val: u8) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_tuple_u64(val: (u64,)) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_tuple_string_u64(val: (String,u64)) -> bool {
  round_trip_prop(val)
}
