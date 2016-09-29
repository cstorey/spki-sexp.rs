use std::iter::{Iterator, Peekable};
use std::convert::From;
use std::str::FromStr;
use std::error;
use serde::de;
use serde::de::Deserializer;

use super::{Error, ErrorKind, SexpToken};
use tokeniser::{TokenisingIterator, TokenError, tokenise};

pub struct Reader<I, E>
    where I: Iterator<Item = Result<SexpToken, E>>,
          Error: From<E>
{
    iter: Peekable<I>,
}

impl<I, E> Reader<I, E>
    where I: Iterator<Item = Result<SexpToken, E>>,
          Error: From<E>
{
    pub fn of_tokens(iter: I) -> Reader<I, E> {
        Reader { iter: iter.peekable() }
    }
}

impl<I, E> Reader<TokenisingIterator<I, E>, TokenError>
    where I: Iterator<Item = Result<u8, E>>,
          E: Into<TokenError>
{
    pub fn new(iter: I) -> Reader<TokenisingIterator<I, E>, TokenError> {
        Reader { iter: tokenise(iter).peekable() }
    }
}

impl<I, E: ::std::error::Error> Reader<I, E>
    where I: Iterator<Item = Result<SexpToken, E>>,
          Error: From<E>
{
    pub fn end(mut self) -> Result<(), Error> {
        match self.iter.next() {
            Some(Ok(ref tok)) => {

                trace!("Reader::end: found at end: {:?}", tok);
                Err(ErrorKind::UnexpectedTokenError(tok.clone(), vec![]).into())
            }
            Some(Err(e)) => Err(From::from(e)),
            None => Ok(()),
        }
    }

    fn parse_list<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor
    {
        visitor.visit_seq(ListParser { de: self })
    }
    //
    fn parse_map<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor
    {

        trace!("Reader::parse_map(): {:?}", self.iter.peek());
        visitor.visit_map(MapParser { de: self })
    }

    fn parse_option<V>(&mut self, mut visitor: V) -> Result<V::Value, Error>
        where V: de::Visitor
    {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) if &**s == "none".as_bytes() => visitor.visit_none(),
            Some(Ok(SexpToken::OpenParen)) => {
                match self.iter.next() {
                    Some(Ok(SexpToken::Atom(ref s))) if &**s == "some".as_bytes() => {
                        let result = visitor.visit_some(self);
                        match self.iter.next() {
                            Some(Ok(SexpToken::CloseParen)) => result,
                            Some(other) => {
                                Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                                    vec![SexpToken::CloseParen])
                                    .into())
                            }
                            None => Err(ErrorKind::EofError.into()),
                        }
                    }
                    Some(other) => {
                        Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                            vec![SexpToken::Atom("?".as_bytes()
                                                                     .to_vec())])
                            .into())
                    }
                    None => Err(ErrorKind::EofError.into()),
                }
            }
            None => Err(ErrorKind::EofError.into()),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                    vec![SexpToken::CloseParen,
                                                         SexpToken::Atom("?".as_bytes().to_vec())])
                    .into())
            }
        }
    }

    fn read_atom_scalar<T, F: FnOnce(&[u8]) -> Result<T, Error>>(&mut self,
                                                                 f: F)
                                                                 -> Result<T, Error> {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => f(s),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                    vec![SexpToken::Atom("<atom>"
                                                             .as_bytes()
                                                             .to_vec())])
                    .into())
            }
            None => Err(ErrorKind::EofError.into()),
        }
    }
}

fn parse_and_visit_with<V: FromStr, R, F: FnMut(V) -> Result<R, Error>>(s: &[u8],
                                                                     mut f: F)
                                                                     -> Result<R, Error>
    where V::Err: Into<Error>
{
    match String::from_utf8_lossy(s).parse() {
        Ok(n) => f(n),
        Err(e) => Err(e.into()),
    }
}

macro_rules! impl_parseable_atom {
    ($ty:ty, $dser_method:ident, $visitor_method:ident) => {
        fn $dser_method<V: de::Visitor>(&mut self,
                mut visitor: V)
            -> Result<V::Value, Self::Error> {
                self.read_atom_scalar(move |s| parse_and_visit_with(s, move |val| visitor.$visitor_method(val)))
            }
    }
}

impl<I, E: error::Error> de::Deserializer for Reader<I, E>
    where I: Iterator<Item = Result<SexpToken, E>>,
          Error: From<E>
{
    type Error = Error;

    fn deserialize<V: de::Visitor>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> {
        let message = "deserialize not supported";
        Err(message.into())
    }

    fn deserialize_unit<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V: de::Visitor>(&mut self,
                                               _name: &str,
                                               mut visitor: V)
                                               -> Result<V::Value, Self::Error> {
        self.deserialize_unit(visitor)
    }

    fn deserialize_string<V: de::Visitor>(&mut self,
                                          mut visitor: V)
                                          -> Result<V::Value, Self::Error> {
        self.deserialize_str(visitor)
    }
    fn deserialize_str<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => visitor.visit_bytes(s),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                    vec![SexpToken::Atom("<string>"
                                                             .as_bytes()
                                                             .to_vec())])
                    .into())
            }
            None => Err(ErrorKind::EofError.into()),
        }
    }

    fn deserialize_bytes<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => visitor.visit_bytes(s),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                    vec![SexpToken::Atom("<bytes>"
                                                             .as_bytes()
                                                             .to_vec())])
                    .into())
            }
            None => Err(ErrorKind::EofError.into()),
        }
    }

    //

    impl_parseable_atom!(usize, deserialize_usize, visit_usize);
    impl_parseable_atom!(usize, deserialize_u64, visit_u64);
    impl_parseable_atom!(usize, deserialize_u32, visit_u32);
    impl_parseable_atom!(usize, deserialize_u16, visit_u16);
    impl_parseable_atom!(usize, deserialize_u8, visit_u8);

    impl_parseable_atom!(isize, deserialize_isize, visit_isize);
    impl_parseable_atom!(isize, deserialize_i64, visit_i64);
    impl_parseable_atom!(isize, deserialize_i32, visit_i32);
    impl_parseable_atom!(isize, deserialize_i16, visit_i16);
    impl_parseable_atom!(isize, deserialize_i8, visit_i8);

    impl_parseable_atom!(f64, deserialize_f64, visit_f64);
    impl_parseable_atom!(f32, deserialize_f32, visit_f32);

    impl_parseable_atom!(bool, deserialize_bool, visit_bool);

    fn deserialize_char<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        self.read_atom_scalar(|bs| {
            let s = try!(String::from_utf8(bs.to_vec()));
            let mut cs = s.chars();
            let c = try!(cs.next().ok_or_else(|| {
                let e : Error = ErrorKind::BadChar(bs.to_vec()).into();
                e
            }));
            if let Some(_) = cs.next() {
                let error: Error = ErrorKind::BadChar(bs.to_vec()).into();
                return Err(error);
            }

            visitor.visit_char(c)
        })
    }


    //
    fn deserialize_option<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        self.parse_option(visitor)
    }

    fn deserialize_seq<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        trace!("Reader::deserialize_seq");
        match self.iter.next() {
            Some(Ok(SexpToken::OpenParen)) => self.parse_list(visitor),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other), vec![SexpToken::OpenParen]).into())
            }
            None => Err(ErrorKind::EofError.into()),
        }
    }

    fn deserialize_seq_fixed_size<V: de::Visitor>(&mut self,
                                                  _len: usize,
                                                  visitor: V)
                                                  -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {

        trace!("Reader::deserialize_map");
        match self.iter.next() {
            Some(Ok(SexpToken::OpenParen)) => self.parse_map(visitor),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other), vec![SexpToken::OpenParen]).into())
            }
            None => Err(ErrorKind::EofError.into()),
        }


    }
    fn deserialize_struct<V>(&mut self,
                             _name: &'static str,
                             _fields: &'static [&'static str],
                             visitor: V)
                             -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_struct_field<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {
        self.deserialize_string(visitor)
    }

    fn deserialize_newtype_struct<V: de::Visitor>(&mut self,
                                                  _name: &'static str,
                                                  visitor: V)
                                                  -> Result<V::Value, Self::Error> {
        self.deserialize_struct(_name, &[], visitor)
    }


    fn deserialize_enum<V>(&mut self,
                           _enum: &'static str,
                           _variants: &'static [&'static str],
                           mut visitor: V)
                           -> Result<V::Value, Self::Error>
        where V: de::EnumVisitor
    {
        visitor.visit(self)
    }

    fn deserialize_tuple_struct<V: de::Visitor>(&mut self,
                                                _name: &'static str,
                                                len: usize,
                                                visitor: V)
                                                -> Result<V::Value, Self::Error> {
        self.deserialize_tuple(len, visitor)
    }
    fn deserialize_tuple<V: de::Visitor>(&mut self,
                                         len: usize,
                                         visitor: V)
                                         -> Result<V::Value, Self::Error> {
        self.deserialize_seq_fixed_size(len, visitor)
    }

    fn deserialize_ignored_any<V: de::Visitor>(&mut self,
                                               visitor: V)
                                               -> Result<V::Value, Self::Error> {
        let message = "deserialize_ignored_any not supported";
        Err(message.into())

    }
}

impl<'a, I, E: error::Error> de::VariantVisitor for Reader<I, E>
    where I: Iterator<Item = Result<SexpToken, E>> + 'a,
          Error: From<E>
{
    type Error = Error;
    fn visit_variant<V: de::Deserialize>(&mut self) -> Result<V, Self::Error> {

        trace!("EnumParser::deserialize_variant: peek: {:?}",
               self.iter.peek());

        match self.iter.next() {
            Some(Ok(SexpToken::OpenParen)) => (),
            Some(other) => {
                return Err(ErrorKind::UnexpectedTokenError(try!(other), vec![SexpToken::OpenParen])
                    .into())
            }
            None => return Err(ErrorKind::EofError.into()),
        };

        let val = try!(de::Deserialize::deserialize(self));


        Ok(val)
    }

    fn visit_unit(&mut self) -> Result<(), Self::Error> {

        trace!("EnumParser::deserialize_unit: peek: {:?}", self.iter.peek());
        let val = try!(de::Deserialize::deserialize(self));

        match self.iter.next() {
            Some(Ok(SexpToken::CloseParen)) => (),
            Some(other) => {
                return Err(ErrorKind::UnexpectedTokenError(try!(other),
                                                           vec![SexpToken::CloseParen])
                    .into())
            }
            None => return Err(ErrorKind::EofError.into()),
        };
        Ok(val)
    }

    fn visit_tuple<V>(&mut self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {

        trace!("EnumParser::deserialize_tuple({:?}): peek: {:?}",
               _len,
               self.iter.peek());
        // panic!("deserialize_tuple: {:?}", _len)
        self.parse_list(_visitor)
    }

    fn visit_struct<V>(&mut self,
                       _fields: &'static [&'static str],
                       _visitor: V)
                       -> Result<V::Value, Self::Error>
        where V: de::Visitor
    {

        trace!("EnumParser::deserialize_struct({:?}): peek: {:?}",
               _fields,
               self.iter.peek());
        // panic!("deserialize_struct: {:?}", _fields)
        self.parse_map(_visitor)
    }

    fn visit_newtype<T: de::Deserialize>(&mut self) -> Result<T, Self::Error> {
        de::Deserialize::deserialize(self)
    }
}

struct ListParser<'a, I, E>
    where I: Iterator<Item = Result<SexpToken, E>> + 'a,
          E: 'a,
          Error: From<E>
{
    de: &'a mut Reader<I, E>,
}
//
impl<'a, I, E: error::Error> de::SeqVisitor for ListParser<'a, I, E>
    where I: Iterator<Item = Result<SexpToken, E>> + 'a,
          E: 'a,
          Error: From<E>
{
    type Error = Error;
    fn visit<T>(&mut self) -> Result<Option<T>, Error>
        where T: de::Deserialize
    {

        trace!("ListParser::visit: peek: {:?}", self.de.iter.peek());
        match self.de.iter.peek() {
            Some(&Ok(SexpToken::CloseParen)) => {

                trace!("ListParser::ending: peek: {:?}", self.de.iter.peek());
                Ok(None)
            }
            None => Err(ErrorKind::EofError.into()),
            _ => {
                let val = try!(de::Deserialize::deserialize(self.de));
                Ok(Some(val))
            }
        }
    }

    fn end(&mut self) -> Result<(), Error> {
        let cur = self.de.iter.next();

        trace!("ListParser::end: got: {:?}", cur);
        match cur {
            Some(Ok(SexpToken::CloseParen)) => Ok(()),
            None => Err(ErrorKind::EofError.into()),
            Some(other) => {
                Err(ErrorKind::UnexpectedTokenError(try!(other), vec![SexpToken::CloseParen])
                    .into())
            }
        }
    }
}

struct MapParser<'a, I, E>
    where I: Iterator<Item = Result<SexpToken, E>> + 'a,
          E: 'a,
          Error: From<E>
{
    de: &'a mut Reader<I, E>,
}

impl<'a, I, E: error::Error> de::MapVisitor for MapParser<'a, I, E>
    where I: Iterator<Item = Result<SexpToken, E>> + 'a,
          E: 'a,
          Error: From<E>
{
    type Error = Error;
    fn visit_key<K>(&mut self) -> Result<Option<K>, Error>
        where K: de::Deserialize
    {

        trace!("MapParser::deserialize_key: peek: {:?}",
               self.de.iter.peek());
        match self.de.iter.peek() {
            Some(&Ok(SexpToken::CloseParen)) => {
                let _ = self.de.iter.next().unwrap();

                trace!("MapParser::deserialize_key: closed: {:?}",
                       self.de.iter.peek());
                Ok(None)
            }
            None => Err(ErrorKind::EofError.into()),
            _ => {
                //      let _ = self.de.iter.next().unwrap();

                trace!("MapParser::deserialize_key: read key @{:?}",
                       self.de.iter.peek());
                let val = try!(de::Deserialize::deserialize(self.de));
                Ok(Some(val))
            }
        }
    }

    fn visit_value<V>(&mut self) -> Result<V, Error>
        where V: de::Deserialize
    {

        trace!("MapParser::deserialize_value: peek: {:?}",
               self.de.iter.peek());
        match self.de.iter.peek().map(|p| p.clone()) {
            Some(&Ok(ref tok @ SexpToken::CloseParen)) => {
                return Err(ErrorKind::UnexpectedTokenError(tok.clone(),
                                                           vec![SexpToken::Atom("Any value, \
                                                                                 honest"
                                                                    .as_bytes()
                                                                    .to_vec())]).into())
            }
            None => return Err(ErrorKind::EofError.into()),
            _ => (),
        }
        trace!("MapParser::deserialize_key: read value");
        let val = try!(de::Deserialize::deserialize(self.de));
        Ok(val)
    }

    fn end(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
