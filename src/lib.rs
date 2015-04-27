#![feature(custom_attribute)]
#![feature(plugin)]
#![feature(rand)]
#![feature(collections)]
#![feature(convert)]

#![feature(core)]
extern crate core;
extern crate rand;

extern crate serde;

use serde::{ser,de};

use std::io::{self, Write};
use std::iter::{Iterator, FromIterator,Peekable};
use std::{error,fmt};

#[derive(PartialEq, Eq,Debug, Clone)]
pub enum SexpToken {
  OpenParen,
  CloseParen,
  Str(String)
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
    , Some(c) if c >= '0' as u8 && c <= '9' as u8 => { let s = read_string(&mut self.iter); Some(SexpToken::Str(s)) }
    , None => None
    , peeked => panic!("Unexpected char in sexp tokenise: {:?}", peeked)
    };
//    writeln!(std::io::stderr(),"TokenisingIterator::next: peek:{:?} => {:?}" , current.map(|c| c as char), r);
    r
  }
}


pub fn tokenise<I:Iterator<Item=u8>>(it : I) -> TokenisingIterator<I> {
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


#[derive(Debug)]
pub enum Error {
  SyntaxError,
  UnexpectedTokenError(SexpToken),
  EofError,
  IoError(io::Error),
  InvalidInt(std::num::ParseIntError),
  InvalidFloat(core::num::ParseFloatError),
  InvalidBool(std::str::ParseBoolError),
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
      Error::InvalidInt(ref e) => error::Error::description(e),
      Error::InvalidFloat(ref e) => "Invalid floating point number", // error::Error::description(e),
      Error::InvalidBool(ref e) => error::Error::description(e),
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
//    writeln!(std::io::stderr(), "Error::end_of_stream_error");
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
    Error::InvalidInt(error)
  }
}

impl From<std::str::ParseBoolError> for Error {
  fn from(error: std::str::ParseBoolError) -> Error {
    Error::InvalidBool(error)
  }
}

// http://stackoverflow.com/questions/29830005/try-macro-stopped-working-after-rust-upgrade
impl From<core::num::ParseFloatError> for Error {
  fn from(error: core::num::ParseFloatError) -> Error {
    Error::InvalidFloat(error)
  }
}

static SEQ: &'static str = "seq";
static MAP: &'static str = "map";
static UINT: &'static str = "uint";
static INT: &'static str = "int";
static BOOL: &'static str = "bool";
static FLOAT: &'static str = "float";

struct Deserializer<I> where I : Iterator<Item=u8> {
  iter : Peekable<TokenisingIterator<I>>
}

impl<I> Deserializer<I> where I : Iterator<Item=u8> {
  fn new (iter: I) -> Deserializer<I> {
    Deserializer { iter: tokenise(iter).peekable() }
  }

  fn end(&mut self) -> Result<(), Error> {
    match self.iter.peek() {
      Some(tok) => {
//	writeln!(std::io::stderr(), "Deserializer::end: found at end: {:?}", tok);
	Err(Error::UnexpectedTokenError(tok.clone()))
      }
    , None => Ok(())
    }
  }

  fn parse_list<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
    visitor.visit_seq(ListParser{ de: self })
  }

  fn parse_map<V>(&mut self, mut visitor: V, name: &str) -> Result<V::Value, Error> where V : de::Visitor {
    visitor.visit_map(MapParser{ de: self })
  }

  fn parse_uint<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::parse_uint: {:?}", self.iter.peek());
    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => {
	match self.iter.next() {
	  Some(SexpToken::CloseParen) => visitor.visit_u64(try!(s.parse()))
	, Some(tok) => {
//	    writeln!(std::io::stderr(), "Deserializer::parse_uint: wanted closeparen, unexpected: {:?}", tok);
	    Err(Error::UnexpectedTokenError(tok))
	  }
	, None => Err(Error::EofError)
	}
      }
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
    }
  }

  fn parse_bool<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::parse_bool: {:?}", self.iter.peek());
    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => {
	match self.iter.next() {
	  Some(SexpToken::CloseParen) => visitor.visit_bool(try!(s.parse()))
	, Some(tok) => Err(Error::UnexpectedTokenError(tok))
	, None => Err(Error::EofError)
	}
      }
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
    }
  }

  fn parse_int<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::parse_int: {:?}", self.iter.peek());
    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => {
	match self.iter.next() {
	  Some(SexpToken::CloseParen) => visitor.visit_i64(try!(s.parse()))
	, Some(tok) => {
//	    writeln!(std::io::stderr(), "Deserializer::parse_int: wanted closeparen, unexpected: {:?}", tok);
	    Err(Error::UnexpectedTokenError(tok))
	  }
	, None => Err(Error::EofError)
	}
      }
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
    }
  }

  fn parse_float<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::parse_float: {:?}", self.iter.peek());
    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => {
	match self.iter.next() {
	  Some(SexpToken::CloseParen) => visitor.visit_f64(try!(s.parse()))
	, Some(tok) => {
//	    writeln!(std::io::stderr(), "Deserializer::parse_uint: wanted closeparen, unexpected: {:?}", tok);
	    Err(Error::UnexpectedTokenError(tok))
	  }
	, None => Err(Error::EofError)
	}
      }
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
    }
  }
  fn parse_option<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V: de::Visitor {
    match self.iter.next() {
      Some(SexpToken::Str(ref s)) if s == "none" => visitor.visit_none()
    , Some(SexpToken::OpenParen) => {
	match self.iter.next() {
	  Some(SexpToken::Str(ref s)) if s == "some" => {
	    let result = visitor.visit_some(self);
	    match self.iter.next() {
	      Some(SexpToken::CloseParen) => result
	    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
	    , None => Err(Error::EofError)
	    }
	  }
	, Some(tok) => Err(Error::UnexpectedTokenError(tok))
	, None => Err(Error::EofError)
	}
    }
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok))
    }
  }
}

// #![feature(core)]
impl<I> de::Deserializer for Deserializer<I> where I : Iterator<Item=u8> {
  type Error = Error;
  fn visit<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
    // writeln!(std::io::stderr(), "Deserializer::visit -> {:?}", unsafe { std::intrinsics::type_name::<V::Value>() });
    // writeln!(std::io::stderr(), "Deserializer::visit: peek: {:?}", self.iter.peek());

    match self.iter.next() {
      Some(SexpToken::Str(ref s)) => visitor.visit_str(s),
      Some(SexpToken::OpenParen) => {
	// writeln!(std::io::stderr(), "Deserializer::visit-next: Open {:?}", self.iter.peek());
	match self.iter.next() {
	  Some(SexpToken::Str(ref s)) if s == SEQ => self.parse_list(visitor)
	, Some(SexpToken::Str(ref s)) if s == MAP => self.parse_map(visitor, MAP)
	, Some(SexpToken::Str(ref s)) if s == UINT => self.parse_uint(visitor)
	, Some(SexpToken::Str(ref s)) if s == INT => self.parse_int(visitor)
	, Some(SexpToken::Str(ref s)) if s == FLOAT => self.parse_float(visitor)
	, Some(SexpToken::Str(ref s)) if s == BOOL => self.parse_bool(visitor)
	, None => Err(Error::EofError)
	, Some(tok) => Err(Error::UnexpectedTokenError(tok))
	}
      }
      , Some(tok) => Err(Error::UnexpectedTokenError(tok))
      , None => Err(Error::EofError)
    }
  }

  fn visit_option<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
    self.parse_option(visitor)
  }
  fn visit_seq<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_seq").unwrap();
    panic!("Unsupported visitation: {}", "visit_seq")
  }
  fn visit_map<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_map").unwrap();
    self.parse_map(visitor, MAP)
  }
  fn visit_named_map<V>(&mut self, name: &str, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_named_map: {}", name).unwrap();
    self.parse_map(visitor, name)
  }

  fn visit_named_unit<V>(&mut self, _name: &str, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_named_unit: {}", _name).unwrap();
    match self.iter.next() {
	Some(SexpToken::Str(ref s)) if _name == s => visitor.visit_named_unit(_name)
      , Some(tok) => Err(Error::UnexpectedTokenError(tok))
      , None => Err(Error::EofError)
    }
  }
  fn visit_named_seq<V>(&mut self, _name: &str, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_named_seq: {}", _name).unwrap();
    panic!("Unsupported visitation: {}", "visit_named_seq")
  }

  /* fn visit_enum<V>(&mut self, _enum: &str, _visitor: V) -> Result<V::Value, Self::Error>
        where V: de::EnumVisitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_enum: {}", _enum);
    _visitor.visit(de::SeqDeserializer{de: self, iter: vec![].into_iter() })
  } */

  fn visit_bytes<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor {
//    writeln!(std::io::stderr(), "Deserializer::visit_bytes").unwrap();
    panic!("Unsupported visitation: {}", "visit_bytes")
  }
}

struct ListParser<'a, I: Iterator<Item=u8> + 'a> {
  de: &'a mut Deserializer<I>
}

impl<'a, I: Iterator<Item=u8> + 'a> de::SeqVisitor for ListParser<'a, I> {
  type Error = Error;
  fn visit<T>(&mut self) -> Result<Option<T>, Error> where T : de::Deserialize {
//    writeln!(std::io::stderr(), "ListParser::visit: peek: {:?}", self.de.iter.peek()).unwrap();
    match self.de.iter.peek() {
      Some(&SexpToken::CloseParen) => {
//	writeln!(std::io::stderr(), "ListParser::ending: peek: {:?}", self.de.iter.peek()).unwrap();
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
    let cur = self.de.iter.next();
//    writeln!(std::io::stderr(), "ListParser::end: got: {:?}", cur).unwrap();
    match cur {
      Some(SexpToken::CloseParen) => Ok(())
    , None => Err(Error::EofError)
    , Some(tok) => Err(Error::UnexpectedTokenError(tok.clone()))
    }
  }
}

struct MapParser<'a, I: Iterator<Item=u8> + 'a> {
  de: &'a mut Deserializer<I>
}

impl<'a, I: Iterator<Item=u8> + 'a> de::MapVisitor for MapParser<'a, I> {
  type Error = Error;
  fn visit_key<K>(&mut self) -> Result<Option<K>, Error> where K : de::Deserialize {
//    writeln!(std::io::stderr(), "MapParser::visit_key: peek: {:?}", self.de.iter.peek()).unwrap();
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

  fn visit_value<V>(&mut self) -> Result<V, Error> where V : de::Deserialize {
//    writeln!(std::io::stderr(), "MapParser::visit_value: peek: {:?}", self.de.iter.peek()).unwrap();
    let current = self.de.iter.peek().map(|p| p.clone());
    match current {
      Some(tok @ SexpToken::CloseParen) => {
	Err(Error::UnexpectedTokenError(tok.clone()))
      }
      None => Err(Error::EofError),
      _ => {
	let val = try!(de::Deserialize::deserialize(self.de));
	Ok(val)
      }
    }
  }

  fn end(&mut self) -> Result<(), Error> {
    let cur = self.de.iter.peek().map(|p| p.clone());
//     writeln!(std::io::stderr(), "MapParser::end: peek: {:?}", self.de.iter.peek()).unwrap();
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
//     writeln!(std::io::stderr(), "Serializer::visit_unit").unwrap();
    try!(self.open());
    try!(self.write_str(SEQ.to_string()));
    self.close()
  }

  fn visit_named_unit(&mut self, name: &str) -> Result<(), Error> {
    self.write_str(name.to_string())
  }

  fn visit_bool(&mut self, val: bool) -> Result<(), Error> {
    //writeln!(std::io::stderr(), "Serializer::visit_bool: {:?}", val).unwrap();
    try!(self.open());
    try!(self.write_str(BOOL.to_string()));
    if val {
      try!(self.write_str("true".to_string()))
    } else {
      try!(self.write_str("false".to_string()))
    }
    self.close()
  }
  // TODO: Consider using display-hints for types of atoms.
  fn visit_u64(&mut self, v: u64) -> Result<(), Error> {
//     writeln!(std::io::stderr(), "Serializer::visit_u64: {:?}", v).unwrap();
    try!(self.open());
    try!(self.write_str(UINT.to_string()));
    try!(self.write_str(v.to_string()));
    self.close()
  }
  fn visit_i64(&mut self, v: i64) -> Result<(), Error> {
//    writeln!(std::io::stderr(), "Serializer::visit_i64: {:?}", v).unwrap();
    try!(self.open());
    try!(self.write_str(INT.to_string()));
    try!(self.write_str(v.to_string()));
    self.close()
  }
  fn visit_f64(&mut self, v: f64) -> Result<(), Error> {
//    writeln!(std::io::stderr(), "Serializer::visit_f64: {:?}", v).unwrap();
    try!(self.open());
    try!(self.write_str(FLOAT.to_string()));
    try!(self.write_str(v.to_string()));
    self.close()

  }
  fn visit_str(&mut self, v: &str) -> Result<(), Error> {
//     writeln!(std::io::stderr(), "Serializer::visit_str: {:?}", v).unwrap();
    self.write_str(format!("{}", v))
  }
  fn visit_none(&mut self) -> Result<(), Error> {
//    writeln!(std::io::stderr(), "Serializer::visit_none").unwrap();
    self.write_str("none".to_string())
  }
  fn visit_some<T>(&mut self, v: T) -> Result<(), Error> where T: ser::Serialize {
 //    writeln!(std::io::stderr(), "Serializer::visit_some(?)").unwrap();
    try!(self.open());
    try!(self.write_str("some".to_string()));
    v.serialize(self);
    self.close()
  }
  fn visit_seq<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
//     writeln!(std::io::stderr(), "Serializer::visit_seq").unwrap();
    try!(self.open());
    try!(self.write_str(SEQ.to_string()));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }
  fn visit_seq_elt<T>(&mut self, v: T) -> Result<(), Error> where T: ser::Serialize {
//     writeln!(std::io::stderr(), "Serializer::visit_seq_elt").unwrap();
    v.serialize(self)
  }
  fn visit_map<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::MapVisitor {
//    writeln!(std::io::stderr(), "Serializer::visit_map").unwrap();
    try!(self.open());
    try!(self.write_str(MAP.to_string()));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }
  fn visit_named_map<V>(&mut self, name: &str, mut visitor: V) -> Result<(), Error> where V: ser::MapVisitor {
//     writeln!(std::io::stderr(), "Serializer::visit_named_map: {:?}", name).unwrap();
    try!(self.open());
    try!(self.write_str(name.to_string()));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }

  fn visit_map_elt<K, V>(&mut self, key: K, value: V) -> Result<(), Error>
    where K: ser::Serialize, V:  ser::Serialize {
//     writeln!(std::io::stderr(), "Serializer::visit_map_elt").unwrap();
    try!(key.serialize(self));
    value.serialize(self)
  }
}

pub fn from_bytes<T>(value: &[u8]) -> Result<T, Error> where T : de::Deserialize {
    let mut de = Deserializer::new(value.iter().map(|p|*p));
    let value = try!(de::Deserialize::deserialize(&mut de));
    // Make sure the whole stream has been consumed.
    try!(de.end());
    Ok(value)
}

pub fn as_bytes<T>(value: &T) -> Vec<u8> where T : ser::Serialize {
    let mut out = Vec::new();
    {
      let mut ser = Serializer::new(&mut out);
      value.serialize(&mut ser).unwrap();
    }
    out
}
