#[macro_use]
extern crate error_chain;

extern crate core;
extern crate serde;
#[macro_use]
extern crate log;

#[macro_use]
extern crate quick_error;
extern crate itoa;
extern crate dtoa;

use serde::{ser, de};

use std::io;
use std::iter::Iterator;

mod errors;
mod writer;
mod reader;
mod tokeniser;
mod packetiser;

pub use errors::*;

pub use writer::Serializer;
pub use reader::Reader;

pub use tokeniser::{encode, tokenise, TokenError, Tokeniser};
pub use packetiser::Packetiser;

#[derive(PartialEq, Eq,Debug, Clone)]
pub enum SexpToken {
    OpenParen,
    CloseParen,
    Atom(Vec<u8>),
}

impl ser::Error for Error {
    fn custom<T: Into<String>>(msg: T) -> Self {
        ErrorKind::SyntaxError(msg.into()).into()
    }
}

impl de::Error for Error {
    fn custom<T: Into<String>>(msg: T) -> Self {
        ErrorKind::SyntaxError(msg.into()).into()
    }

    fn end_of_stream() -> Error {
        //    writeln!(std::io::stderr(), "Error::end_of_stream_error");
        ErrorKind::EofError.into()
    }

    fn unknown_field(field: &str) -> Error {
        ErrorKind::UnknownField(field.to_string()).into()
    }

    fn missing_field(field: &'static str) -> Error {
        ErrorKind::MissingField(field.to_string()).into()
    }
}

pub fn from_bytes<T>(value: &[u8]) -> Result<T>
    where T: de::Deserialize
{
    let mut de = Reader::new(value.iter().cloned().map(|v| Ok(v).map_err(|e: TokenError| e)));
    let value = try!(de::Deserialize::deserialize(&mut de));
    // Make sure the whole stream has been consumed.
    try!(de.end());
    Ok(value)
}

pub fn from_reader<T, R: io::Read>(rdr: R) -> Result<T>
    where T: de::Deserialize
{
    let mut de = Reader::new(rdr.bytes());
    let value = try!(de::Deserialize::deserialize(&mut de));
    // Make sure the whole stream has been consumed.
    try!(de.end());
    Ok(value)
}

pub fn as_bytes<T>(value: &T) -> Result<Vec<u8>>
    where T: ser::Serialize
{
    let mut out = Vec::new();
    try!(to_writer(&mut out, value));
    Ok(out)
}

pub fn to_writer<T, W: io::Write>(out: W, value: &T) -> Result<()>
    where T: ser::Serialize
{
    let mut ser = Serializer::new(out);
    try!(value.serialize(&mut ser));
    Ok(())
}
