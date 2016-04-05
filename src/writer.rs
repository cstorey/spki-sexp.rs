use std::io::{Write,Cursor};
use std::fmt;

use serde::ser;

use super::Error;

pub struct Serializer<W> {
  writer : W
}

impl<W> Serializer<W> where W : Write {
  pub fn new (wr: W) -> Serializer<W> {
    Serializer{ writer : wr }
  }

  fn write_str(&mut self, s: &str) -> Result<(), Error> {
    self.write_bytes(s.as_bytes())
  }

  fn write_bytes(&mut self, s: &[u8]) -> Result<(), Error> {
    let mut pfx = [0u8; 22];
    let off = {
        let mut cur = Cursor::new(pfx.as_mut());
        try!(write!(cur, "{}:", s.len()));
        cur.position() as usize
    };
    try!(self.writer.write_all(&pfx[..off]));
    try!(self.writer.write_all(s));
    Ok(())
  }

  fn write_displayable<T: fmt::Display>(&mut self, v: T) -> Result<(), Error> {
    let mut bytes = [0u8; 22];
    let off = {
        let mut cur = Cursor::new(bytes.as_mut());
        try!(write!(cur, "{}", v));
        cur.position() as usize
    };

    Ok(try!(self.write_bytes(&bytes[..off])))
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
//
  fn serialize_unit(&mut self) -> Result<(), Error> {
    Ok(())
  }

  fn serialize_bool(&mut self, val: bool) -> Result<(), Error> {
    //writeln!(std::io::stderr(), "Serializer::serialize_bool: {:?}", val).unwrap();
    if val {
      try!(self.write_str("true"))
    } else {
      try!(self.write_str("false"))
    }
    Ok(())
  }
//   // TODO: Consider using display-hints for types of atoms.
  fn serialize_u64(&mut self, v: u64) -> Result<(), Error> {
      self.write_displayable(v)
  }

  fn serialize_i64(&mut self, v: i64) -> Result<(), Error> {
      self.write_displayable(v)
  }
  fn serialize_f64(&mut self, v: f64) -> Result<(), Error> {
      self.write_displayable(v)
  }
  fn serialize_str(&mut self, v: &str) -> Result<(), Error> {
    self.write_str(v)
  }
  fn serialize_bytes(&mut self, v: &[u8]) -> Result<(), Error> {
    self.write_bytes(v)
  }
  fn serialize_none(&mut self) -> Result<(), Error> {
//    writeln!(std::io::stderr(), "Serializer::serialize_none").unwrap();
    self.write_str("none")
  }
  fn serialize_some<T>(&mut self, v: T) -> Result<(), Error> where T: ser::Serialize {
 //    writeln!(std::io::stderr(), "Serializer::serialize_some(?)").unwrap();
    try!(self.open());
    try!(self.write_str("some"));
    try!(v.serialize(self));
    self.close()
  }
  fn serialize_seq<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
    // writeln!(std::io::stderr(), "Serializer::serialize_seq").unwrap();
    try!(self.open());
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }
//
//   fn serialize_named_seq<V>(&mut self, name: &'static str, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
//     // writeln!(std::io::stderr(), "Serializer::serialize_named_seq:{}", name).unwrap();
//     try!(self.open());
//     try!(self.write_str(name));
//     while let Some(()) = try!(visitor.visit(self)) {}
//     self.close()
//   }
//
  fn serialize_seq_elt<T>(&mut self, v: T) -> Result<(), Error> where T: ser::Serialize {
//     writeln!(std::io::stderr(), "Serializer::serialize_seq_elt").unwrap();
    v.serialize(self)
  }
  fn serialize_map<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::MapVisitor {
//    writeln!(std::io::stderr(), "Serializer::serialize_map").unwrap();
    try!(self.open());
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }

  fn serialize_map_elt<K, V>(&mut self, key: K, value: V) -> Result<(), Error>
    where K: ser::Serialize, V:  ser::Serialize {
//     writeln!(std::io::stderr(), "Serializer::serialize_map_elt").unwrap();
    try!(key.serialize(self));
    value.serialize(self)
  }

   fn serialize_unit_variant(&mut self, _name: &'static str,
                          _variant_index: usize,
                          variant: &'static str) -> Result<(), Error> {
     // writeln!(std::io::stderr(), "Serializer::serialize_named_unit:{:?}", name).unwrap();
    try!(self.open());
    try!(self.write_str(variant));
    Ok(try!(self.close()))
   }

   fn serialize_tuple_variant<V: ser::SeqVisitor>(&mut self, _name: &'static str,
                              _variant_index: usize,
                              variant: &'static str, mut visitor: V)  -> Result<(), Error> {
     // writeln!(std::io::stderr(), "Serializer::serialize_named_unit:{:?}", name).unwrap();
    try!(self.open());
    try!(self.write_str(variant));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
   }

   fn serialize_struct_variant<V: ser::MapVisitor>(&mut self, _name: &'static str,
                              _variant_index: usize,
                              variant: &'static str, mut visitor: V)  -> Result<(), Error> {
    try!(self.open());
    try!(self.write_str(variant));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
   }
}
