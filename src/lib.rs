extern crate core;
extern crate rand;

extern crate serde;

use serde::{ser,de};

use std::io::{self, Write};
use std::iter::Iterator;
use std::{error,fmt};

mod writer;
mod reader;
mod tokeniser;

pub use writer::Serializer;
pub use reader::Deserializer;

pub use tokeniser::{encode,tokenise};

#[derive(PartialEq, Eq,Debug, Clone)]
pub enum SexpToken {
  OpenParen,
  CloseParen,
  Str(String)
}

#[derive(Debug)]
pub enum Error {
  SyntaxError(String),
  UnexpectedTokenError(SexpToken, Vec<SexpToken>),
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
      Error::SyntaxError(_) => "Syntax error",
      Error::UnexpectedTokenError(_, _) => "Unexpected Token",
      Error::EofError => "EOF",
      Error::IoError(ref e) => error::Error::description(e),
      Error::InvalidInt(ref e) => error::Error::description(e),
      Error::InvalidFloat(ref e) => error::Error::description(e),
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
  fn syntax(msg: &str) -> Error {
    Error::SyntaxError(msg.to_string())
  }

  fn end_of_stream() -> Error {
//    writeln!(std::io::stderr(), "Error::end_of_stream_error");
    Error::EofError
  }

  fn unknown_field(field: &str) -> Error {
    Error::UnknownField(field.to_string())
  }

  fn missing_field(field: &'static str) -> Error {
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
