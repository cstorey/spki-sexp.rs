use std::iter::{Iterator, Peekable};
use std::io;
use serde::{self, de};

use super::{Error,SexpToken};
use tokeniser::{TokenisingIterator,tokenise};

pub struct Deserializer<I> where I : Iterator<Item=Result<u8, io::Error>> {
  iter : Peekable<TokenisingIterator<I>>
}

impl<I> Deserializer<I> where I : Iterator<Item=Result<u8, io::Error>> {
  pub fn new (iter: I) -> Deserializer<I> {
    Deserializer { iter: tokenise(iter).peekable() }
  }

  pub fn end(mut self) -> Result<(), Error> {
    match self.iter.next() {
      Some(Ok(ref tok)) => {
//      writeln!(std::io::stderr(), "Deserializer::end: found at end: {:?}", tok);
        Err(Error::UnexpectedTokenError(tok.clone(), vec![]))
      }
    , Some(Err(e)) => Err(e.into())
    , None => Ok(())
    }
  }

   fn parse_list<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
     visitor.visit_seq(ListParser{ de: self })
   }
//
   fn parse_map<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
     // writeln!(std::io::stderr(), "Deserializer::parse_map(): {:?}", self.iter.peek());
     visitor.visit_map(MapParser{ de: self })
   }


  fn parse_option<V>(&mut self, mut visitor: V) -> Result<V::Value, Error> where V: de::Visitor {
    match self.iter.next() {
      Some(Ok(SexpToken::Atom(ref s))) if &**s == "none".as_bytes() => visitor.visit_none()
    , Some(Ok(SexpToken::OpenParen)) => {
        match self.iter.next() {
          Some(Ok(SexpToken::Atom(ref s))) if &**s == "some".as_bytes() => {
            let result = visitor.visit_some(self);
            match self.iter.next() {
              Some(Ok(SexpToken::CloseParen)) => result
            , Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::CloseParen]))
            , None => Err(Error::EofError)
            }
          }
        , Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::Atom("?".as_bytes().to_vec())]))
        , None => Err(Error::EofError)
        }
    }
    , None => Err(Error::EofError)
    , Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::CloseParen, SexpToken::Atom("?".as_bytes().to_vec())]))
    }
  }
}

impl<I> de::Deserializer for Deserializer<I> where I : Iterator<Item=Result<u8, io::Error>> {
   type Error = Error;
   fn visit<V>(&mut self, _visitor: V) -> Result<V::Value, Error> where V : de::Visitor {
       Err(serde::de::Error::syntax("spki-sexp does not support Deserializer::visit"))
   }

    fn visit_unit<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor,
    {
        visitor.visit_unit()
    }

    fn visit_string<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => visitor.visit_bytes(s),
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::Atom("<String>".as_bytes().to_vec())])),
            None => Err(Error::EofError),
        }
    }

    fn visit_u64<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => match String::from_utf8_lossy(s).parse() {
                Ok(n) => visitor.visit_u64(n),
                Err(e) => Err(Error::InvalidInt(e)),
            },
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::Atom("<u64>".as_bytes().to_vec())])),
            None => Err(Error::EofError),
        }
    }

    fn visit_i64<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => match String::from_utf8_lossy(s).parse() {
                Ok(n) => visitor.visit_i64(n),
                Err(e) => Err(Error::InvalidInt(e)),
            },
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::Atom("<i64>".as_bytes().to_vec())])),
            None => Err(Error::EofError),
        }
    }

    fn visit_f64<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => match String::from_utf8_lossy(s).parse() {
                Ok(n) => visitor.visit_f64(n),
                Err(e) => Err(Error::InvalidFloat(e)),
            },
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::Atom("<f64>".as_bytes().to_vec())])),
            None => Err(Error::EofError),
        }
    }

    fn visit_bool<V>(&mut self, mut visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        match self.iter.next() {
            Some(Ok(SexpToken::Atom(ref s))) => match String::from_utf8_lossy(s).parse() {
                Ok(n) => visitor.visit_bool(n),
                Err(e) => Err(Error::InvalidBool(e)),
            },
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::Atom("<bool>".as_bytes().to_vec())])),
            None => Err(Error::EofError),
        }
    }


//
   fn visit_option<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
     self.parse_option(visitor)
   }

   fn visit_seq<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
     // writeln!(std::io::stderr(), "Deserializer::visit_seq").unwrap();
     match self.iter.next() {
            Some(Ok(SexpToken::OpenParen)) => self.parse_list(visitor),
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::OpenParen])),
            None => Err(Error::EofError),
        }
   }
   fn visit_map<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
//     writeln!(std::io::stderr(), "Deserializer::visit_map").unwrap();
     match self.iter.next() {
            Some(Ok(SexpToken::OpenParen)) => self.parse_map(visitor),
            Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::OpenParen])),
            None => Err(Error::EofError),
        }


   }
   fn visit_struct<V>(&mut self, _name: &'static str, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error>
         where V: de::Visitor {
     self.visit_map(visitor)
   }

    fn visit_struct_field<V>(&mut self, visitor: V) -> Result<V::Value, Self::Error>
        where V: de::Visitor,
    {
        self.visit_string(visitor)
    }

  fn visit_enum<V>(&mut self, _enum: &'static str, _variants: &'static [&'static str], mut visitor: V) -> Result<V::Value, Self::Error>
        where V: de::EnumVisitor {
    visitor.visit(self)
  }
}

impl<'a, I: Iterator<Item=Result<u8, io::Error>> + 'a> de::VariantVisitor for Deserializer<I> where I : Iterator<Item=Result<u8, io::Error>> {
    type Error = Error;
    fn visit_variant<V: de::Deserialize>(&mut self) -> Result<V, Self::Error>{
        // writeln!(std::io::stderr(), "EnumParser::visit_variant: peek: {:?}", self.iter.peek()).unwrap();
        match self.iter.next() {
            Some(Ok(SexpToken::OpenParen)) => (),
            Some(other) => return Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::OpenParen])),
            None => return Err(Error::EofError)
        };

        let val = try!(de::Deserialize::deserialize(self));


        Ok(val)
    }

    fn visit_unit(&mut self) -> Result<(), Self::Error> {
        // writeln!(std::io::stderr(), "EnumParser::visit_unit: peek: {:?}", self.iter.peek()).unwrap();
        let val = try!(de::Deserialize::deserialize(self));

        match self.iter.next() {
            Some(Ok(SexpToken::CloseParen)) => (),
            Some(other) => return Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::CloseParen])),
            None => return Err(Error::EofError)
        };
        Ok(val)
    }

    fn visit_tuple<V>(&mut self, _len: usize,_visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        // writeln!(std::io::stderr(), "EnumParser::visit_tuple({:?}): peek: {:?}", _len, self.iter.peek()).unwrap();
        // panic!("visit_tuple: {:?}", _len)
        self.parse_list(_visitor)
    }

    fn visit_struct<V>(&mut self, _fields: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: de::Visitor {
        // writeln!(std::io::stderr(), "EnumParser::visit_struct({:?}): peek: {:?}", _fields, self.iter.peek()).unwrap();
        // panic!("visit_struct: {:?}", _fields)
        self.parse_map(_visitor)
    }
}

struct ListParser<'a, I: Iterator<Item=Result<u8, io::Error>> + 'a> {
  de: &'a mut Deserializer<I>
}
//
impl<'a, I: Iterator<Item=Result<u8, io::Error>> + 'a> de::SeqVisitor for ListParser<'a, I> {
  type Error = Error;
  fn visit<T>(&mut self) -> Result<Option<T>, Error> where T : de::Deserialize {
//    writeln!(std::io::stderr(), "ListParser::visit: peek: {:?}", self.de.iter.peek()).unwrap();
    match self.de.iter.peek() {
      Some(&Ok(SexpToken::CloseParen)) => {
//      writeln!(std::io::stderr(), "ListParser::ending: peek: {:?}", self.de.iter.peek()).unwrap();
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
      Some(Ok(SexpToken::CloseParen)) => Ok(())
    , None => Err(Error::EofError)
    , Some(other) => Err(Error::UnexpectedTokenError(try!(other), vec![SexpToken::CloseParen]))
    }
  }
}

struct MapParser<'a, I: Iterator<Item=Result<u8, io::Error>> + 'a> {
  de: &'a mut Deserializer<I>
}

impl<'a, I: Iterator<Item=Result<u8, io::Error>> + 'a> de::MapVisitor for MapParser<'a, I> {
  type Error = Error;
  fn visit_key<K>(&mut self) -> Result<Option<K>, Error> where K : de::Deserialize {
    // writeln!(std::io::stderr(), "MapParser::visit_key: peek: {:?}", self.de.iter.peek()).unwrap();
    match self.de.iter.peek() {
      Some(&Ok(SexpToken::CloseParen)) => {
        let _ = self.de.iter.next().unwrap();
        // writeln!(std::io::stderr(), "MapParser::visit_key: closed: {:?}", self.de.iter.peek()).unwrap();
        Ok(None)
      }
      None => Err(Error::EofError),
      _ => {
//      let _ = self.de.iter.next().unwrap();
        // writeln!(std::io::stderr(), "MapParser::visit_key: read key @{:?}", self.de.iter.peek());
        let val = try!(de::Deserialize::deserialize(self.de));
        Ok(Some(val))
      }
    }
  }

  fn visit_value<V>(&mut self) -> Result<V, Error> where V : de::Deserialize {
//    writeln!(std::io::stderr(), "MapParser::visit_value: peek: {:?}", self.de.iter.peek()).unwrap();
    match self.de.iter.peek().map(|p| p.clone()) {
      Some(&Ok(ref tok @ SexpToken::CloseParen)) => {
        return Err(Error::UnexpectedTokenError(tok.clone(), vec![SexpToken::Atom("Any value, honest".as_bytes().to_vec())]))
      }
      None => return Err(Error::EofError),
      _ => (),
    }
//  writeln!(std::io::stderr(), "MapParser::visit_key: read value");
    let val = try!(de::Deserialize::deserialize(self.de));
    Ok(val)
  }

  fn end(&mut self) -> Result<(), Error> {
    Ok(())
  }
}
