#![feature(custom_attribute)]
#![feature(plugin)]
#![feature(rand)]
#![feature(collections)]
#![feature(convert)]

#![feature(core)]
extern crate core;
extern crate rand;
extern crate rustc_serialize;

use std::io::{self, Write};
use std::iter::{Iterator, FromIterator,Peekable, IntoIterator};
use std::{error,fmt};
use std::rc::Rc;

use rustc_serialize::{Decodable, Encodable};

#[derive(PartialEq, Eq,Debug, Clone)]
pub enum SexpToken {
  OpenParen,
  CloseParen,
  Atom(Vec<u8>)
}

struct EncodingIterator<I: Iterator> {
  iter: I
}

fn encode_token<W>(t: &SexpToken, out: &mut W) -> Result<(), io::Error> where W : Write {
  match t {
    &SexpToken::OpenParen => try!(out.write_all(&['(' as u8])),
      &SexpToken::CloseParen => try!(out.write_all(&[')' as u8])),
      &SexpToken::Atom(ref s) => {
	try!(out.write_all(format!("{}", s.len()).into_bytes().as_slice()));
	try!(out.write_all(&[':' as u8]));
	try!(out.write_all(s.as_slice()))
      }
  }

  Ok(())
}

pub fn encode<'a, I>(it : I) -> Result<Vec<u8>, io::Error> where I : Iterator<Item=&'a SexpToken> {
  let mut out = vec![];
  for t in it {
    try!(encode_token(t, &mut out))
  }
  Ok(out)
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

#[derive(Debug)]
struct Encoder<W> {
  writer: W
}

impl<W> Encoder<W> where W : Write {
  fn new(wr: W) -> Encoder<W> {
    Encoder { writer: wr }
  }
}

#[derive(Debug)]
pub enum EncoderError {
  TokenError(Error),
  IoError(io::Error)
}
pub type EncodeResult<T> = Result<T, EncoderError>;

impl std::convert::From<Error> for EncoderError{
  fn from(error: Error) -> EncoderError {
    EncoderError::TokenError(error)
  }
}

impl std::convert::From<io::Error> for EncoderError{
  fn from(error: io::Error) -> EncoderError {
    EncoderError::IoError(error)
  }
}

#[derive(Debug)]
pub enum DecoderError {
  TokenError(Error),
  ApplicationError(String),
  SyntaxError(&'static str, SexpInfo),
  ParseIntError(std::num::ParseIntError),
  ParseFloatError(std::num::ParseFloatError),
  ParseBoolError(std::str::ParseBoolError)
}

pub type DecodeResult<T> = Result<T, DecoderError>;

impl std::convert::From<Error> for DecoderError{
  fn from(error: Error) -> DecoderError {
    DecoderError::TokenError(error)
  }
}

impl std::convert::From<std::num::ParseIntError> for DecoderError{
  fn from(error: std::num::ParseIntError) -> DecoderError {
    DecoderError::ParseIntError(error)
  }
}

impl std::convert::From<std::num::ParseFloatError> for DecoderError{
  fn from(error: std::num::ParseFloatError) -> DecoderError {
    DecoderError::ParseFloatError(error)
  }
}

impl std::convert::From<std::str::ParseBoolError> for DecoderError{
  fn from(error: std::str::ParseBoolError) -> DecoderError {
    DecoderError::ParseBoolError(error)
  }
}

fn emit_with_display<T, W>(v: T, wr: &mut W) -> EncodeResult<()> where T: fmt::Display, W: Write {
  try!(encode_token(&SexpToken::Atom(format!("{}", v).as_bytes().into_iter().map(|v|v.clone()).collect()), wr));
  Ok(())
}

impl <W> rustc_serialize::Encoder for Encoder<W> where W : Write {
  type Error = EncoderError;

  fn emit_nil(&mut self) -> EncodeResult<()> {
    // writeln!(std::io::stderr(),"emit_nil:" );
    try!(encode_token(&SexpToken::Atom(b"nil".into_iter().map(|v|v.clone()).collect()), &mut self.writer));
    Ok(())
  }

  fn emit_usize(&mut self, v: usize) -> EncodeResult<()> {
    emit_with_display(v, &mut self.writer)
  }
  fn emit_u64(&mut self, v: u64) -> EncodeResult<()> {
    emit_with_display(v, &mut self.writer)
  }
  fn emit_u32(&mut self, v: u32) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_u16(&mut self, v: u16) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_u8(&mut self, v: u8) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }

  fn emit_isize(&mut self, v: isize) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_i64(&mut self, v: i64) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_i32(&mut self, v: i32) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_i16(&mut self, v: i16) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_i8(&mut self, v: i8) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }

  fn emit_bool(&mut self, v: bool) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }

  fn emit_f64(&mut self, v: f64) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }
  fn emit_f32(&mut self, v: f32) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }

  fn emit_char(&mut self, v: char) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }

  fn emit_str(&mut self, v: &str) -> EncodeResult<()> { emit_with_display(v, &mut self.writer) }

  fn emit_enum<F>(&mut self, _name: &str, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    f(self)
  }

  fn emit_enum_variant<F>(&mut self,
      name: &str,
      _id: usize,
      cnt: usize,
      f: F)
    -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
    {
      panic!("emit_enum_variant")
    }

  fn emit_enum_variant_arg<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    panic!("emit_enum_variant_arg")
  }

  fn emit_enum_struct_variant<F>(&mut self,
      name: &str,
      id: usize,
      cnt: usize,
      f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    panic!("emit_enum_struct_variant")
  }

  fn emit_enum_struct_variant_field<F>(&mut self,
      _: &str,
      idx: usize,
      f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    panic!("emit_enum_struct_variant_field")
  }


  fn emit_struct<F>(&mut self, name: &str, len: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    try!(encode_token(&SexpToken::OpenParen, &mut self.writer));
    try!(encode_token(&SexpToken::Atom(name.bytes().map(|c|c.clone()).collect()), &mut self.writer));
    try!(f(self));
    try!(encode_token(&SexpToken::CloseParen, &mut self.writer));
    Ok(())
  }

  fn emit_struct_field<F>(&mut self, name: &str, idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    try!(encode_token(&SexpToken::Atom(name.bytes().map(|c|c.clone()).collect()), &mut self.writer));
    f(self)
  }

  fn emit_tuple<F>(&mut self, len: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    self.emit_seq(len, f)
  }
  fn emit_tuple_arg<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    self.emit_seq_elt(idx, f)
  }

  fn emit_tuple_struct<F>(&mut self, _: &str, len: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    self.emit_seq(len, f)
  }
  fn emit_tuple_struct_arg<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    self.emit_seq_elt(idx, f)
  }

  fn emit_option<F>(&mut self, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    f(self)
  }

  fn emit_option_none(&mut self) -> EncodeResult<()> {
    self.emit_nil()
  }

  fn emit_option_some<F>(&mut self, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    try!(encode_token(&SexpToken::OpenParen, &mut self.writer));
    try!(encode_token(&SexpToken::Atom(b"some".iter().map(|c|c.clone()).collect()), &mut self.writer));
    try!(f(self));
    try!(encode_token(&SexpToken::CloseParen, &mut self.writer));
    Ok(())
  }

  fn emit_seq<F>(&mut self, len: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    try!(encode_token(&SexpToken::OpenParen, &mut self.writer));
    try!(f(self));
    try!(encode_token(&SexpToken::CloseParen, &mut self.writer));
    Ok(())
  }

  fn emit_seq_elt<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    f(self)
  }

  fn emit_map<F>(&mut self, len: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    try!(encode_token(&SexpToken::OpenParen, &mut self.writer));
    try!(f(self));
    try!(encode_token(&SexpToken::CloseParen, &mut self.writer));
    Ok(())
  }

  fn emit_map_elt_key<F>(&mut self, idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    f(self)
  }

  fn emit_map_elt_val<F>(&mut self, _idx: usize, f: F) -> EncodeResult<()> where
    F: FnOnce(&mut Self) -> EncodeResult<()>,
  {
    f(self)
  }
}

#[derive(Debug)]
struct Decoder(SexpInfo);
impl Decoder {
  fn new(sexp: SexpInfo) -> Decoder {
    Decoder(sexp)
  }
}

macro_rules! parse_atom {
  ($name:ident, $ty:ident) => {
    fn $name(&mut self) -> DecodeResult<$ty> {
      match &self.0 {
	&SexpInfo::Atom(ref s) => Ok(try!(String::from_utf8_lossy(s).parse::<$ty>())),
	  other => Err(DecoderError::SyntaxError("parse_atom", other.clone()))
      }
    }
  }
}

impl rustc_serialize::Decoder for Decoder {
    type Error = DecoderError;

    fn read_nil(&mut self) -> DecodeResult<()> {
      // writeln!(std::io::stderr(),"read_nil");
      match &self.0 {
	&SexpInfo::Atom(ref s) if s == b"nil" => Ok(()),
	other => Err(DecoderError::SyntaxError("read_nil", other.clone()))
      }
    }

    parse_atom!(read_u64, u64);
    parse_atom!(read_usize, usize);
    parse_atom!(read_u32, u32);
    parse_atom!(read_u16, u16);
    parse_atom!(read_u8, u8);

    parse_atom!(read_isize, isize);
    parse_atom!(read_i64, i64);
    parse_atom!(read_i32, i32);
    parse_atom!(read_i16, i16);
    parse_atom!(read_i8, i8);

    parse_atom!(read_f32, f32);
    parse_atom!(read_f64, f64);
    parse_atom!(read_bool, bool);

    fn read_char(&mut self) -> DecodeResult<char> {
      match &self.0 {
	&SexpInfo::Atom(ref bytes) => {
	  let s = String::from_utf8_lossy(bytes);

	  if s.chars().count() == 1 {
	     Ok(s.chars().next().unwrap())
	  } else {
	    Err(DecoderError::SyntaxError("read_char", self.0.clone()))
	  }
	},
	other => Err(DecoderError::SyntaxError("read_char", other.clone()))
      }
    }

    fn read_str(&mut self) -> DecodeResult<String> {
      match &self.0 {
	&SexpInfo::Atom(ref bytes) => {
	  let s = String::from_utf8_lossy(bytes);
	  Ok(s.into_owned())
	},
	other => Err(DecoderError::SyntaxError("read_str", other.clone()))
      }
    }

    fn read_enum<T, F>(&mut self, _name: &str, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
        f(self)
    }

    fn read_enum_variant<T, F>(&mut self, names: &[&str],
                               mut f: F) -> DecodeResult<T>
        where F: FnMut(&mut Decoder, usize) -> DecodeResult<T>,
    {
      unimplemented!()
    }

    fn read_enum_variant_arg<T, F>(&mut self, _idx: usize, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
        f(self)
    }

    fn read_enum_struct_variant<T, F>(&mut self, names: &[&str], f: F) -> DecodeResult<T> where
        F: FnMut(&mut Decoder, usize) -> DecodeResult<T>,
    {
      panic!("read_enum_struct_variant")
    }


    fn read_enum_struct_variant_field<T, F>(&mut self,
                                         _name: &str,
                                         idx: usize,
                                         f: F)
                                         -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
        self.read_enum_variant_arg(idx, f)
    }

    fn read_struct<T, F>(&mut self, name: &str, len: usize, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      // writeln!(std::io::stderr(),"read_struct: {:?}, {:?}; {:?}" , name, len, &self.0).unwrap();
      match &self.0.clone() {
	&SexpInfo::List(ref l) if l.len() == len*2+1 && l[0] == SexpInfo::Atom(name.bytes().map(|c|c.clone()).collect()) => {
	  f(self)
	},

	other => Err(DecoderError::SyntaxError("read_struct", other.clone()))
      }
    }

    fn read_struct_field<T, F>(&mut self,
                               name: &str,
                               _idx: usize,
                               f: F)
                               -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      // writeln!(std::io::stderr(),"read_struct_field: {:?}, {:?}; {:?}" , name, _idx, &self.0).unwrap();
      match &self.0.clone() {
	&SexpInfo::List(ref l) => {
	  let off = _idx*2+1;
	  if l[off] == SexpInfo::Atom(name.bytes().map(|c|c.clone()).collect()) {
	    f(&mut Decoder(l[off+1].clone()))
	  } else {
	    Err(DecoderError::SyntaxError("read_struct_field; wrong name", self.0.clone()))
	  }
	},

	other => Err(DecoderError::SyntaxError("read_struct_field", other.clone()))
      }
    }

    fn read_tuple<T, F>(&mut self, tuple_len: usize, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      self.read_seq(move |d, len| { f(d) })
    }

    fn read_tuple_arg<T, F>(&mut self, idx: usize, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      self.read_seq_elt(idx, f)
    }

    fn read_tuple_struct<T, F>(&mut self,
                               _name: &str,
                               len: usize,
                               f: F)
                               -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      panic!("read_tuple_struct")
    }

    fn read_tuple_struct_arg<T, F>(&mut self,
                                   idx: usize,
                                   f: F)
                                   -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      panic!("read_tuple_struct_arg")
    }

    fn read_option<T, F>(&mut self, mut f: F) -> DecodeResult<T> where
        F: FnMut(&mut Decoder, bool) -> DecodeResult<T>,
    {
      match &self.0.clone() {
	&SexpInfo::Atom(ref atom) if atom == b"nil" => {
	  f(self, false)
	},
	&SexpInfo::List(ref l) if l.len() == 2 && l[0] == SexpInfo::Atom(b"some".iter().map(|c|c.clone()).collect()) => {
	  f(&mut Decoder(l[1].clone()), true)
	},

	other => Err(DecoderError::SyntaxError("read_option", other.clone()))
      }
    }

    fn read_seq<T, F>(&mut self, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder, usize) -> DecodeResult<T>,
    {
      match &self.0.clone() {
	&SexpInfo::List(ref l) => {
	  f(self, l.len())
	},
	other => Err(DecoderError::SyntaxError("read_seq", other.clone()))
      }
    }

    fn read_seq_elt<T, F>(&mut self, _idx: usize, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      match &self.0.clone() {
	&SexpInfo::List(ref l) => {
	  f(&mut Decoder(l[_idx].clone()))
	},
	other => Err(DecoderError::SyntaxError("read_seq_elt", other.clone()))
      }
    }

    fn read_map<T, F>(&mut self, f: F) -> DecodeResult<T> where
        F: FnOnce(&mut Decoder, usize) -> DecodeResult<T>,
    {
      match &self.0.clone() {
	&SexpInfo::List(ref l) => {
	  f(self, l.len() / 2)
	},
	other => Err(DecoderError::SyntaxError("read_seq", other.clone()))
      }
    }

    fn read_map_elt_key<T, F>(&mut self, _idx: usize, f: F) -> DecodeResult<T> where
       F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      // writeln!(std::io::stderr(),"read_map_elt_key: {:?}, {:?}" , _idx, self.0).unwrap();
      match &self.0.clone() {
	&SexpInfo::List(ref l) => f(&mut Decoder(l[2*_idx].clone())),
        other => Err(DecoderError::SyntaxError("read_map_elt_key", other.clone()))
      }
    }

    fn read_map_elt_val<T, F>(&mut self, _idx: usize, f: F) -> DecodeResult<T> where
       F: FnOnce(&mut Decoder) -> DecodeResult<T>,
    {
      // writeln!(std::io::stderr(),"read_map_elt_val: {:?}, {:?}" , _idx, self.0).unwrap();
      match &self.0.clone() {
	&SexpInfo::List(ref l) => f(&mut Decoder(l[2*_idx+1].clone())),
        other => Err(DecoderError::SyntaxError("read_map_elt_key", other.clone()))
      }
    }

    fn error(&mut self, err: &str) -> DecoderError {
        DecoderError::ApplicationError(err.to_string())
    }
}

pub fn to_bytes<T : Encodable>(obj: &T) -> EncodeResult<Vec<u8>> {
  let mut out = Vec::new();
  {
    let mut encoder = Encoder::new(&mut out);
    try!(obj.encode(&mut encoder))
  }
  return Ok(out)
}

pub fn from_bytes<T : Decodable>(obj: &[u8]) -> DecodeResult<T> {
  let sexp = try!(SexpInfo::read_from(obj));
  let mut decoder = Decoder::new(sexp);
  Decodable::decode(&mut decoder)
}

