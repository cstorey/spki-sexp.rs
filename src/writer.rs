use std::io::Write;

use serde::ser;

use super::Error;

pub struct Serializer<W> {
  writer : W
}

impl<W> Serializer<W> where W : Write {
  pub fn new (wr: W) -> Serializer<W> {
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
//
  fn visit_unit(&mut self) -> Result<(), Error> {
    Ok(())
  }

//
  fn visit_bool(&mut self, val: bool) -> Result<(), Error> {
    //writeln!(std::io::stderr(), "Serializer::visit_bool: {:?}", val).unwrap();
    if val {
      try!(self.write_str("true".to_string()))
    } else {
      try!(self.write_str("false".to_string()))
    }
    Ok(())
  }
//   // TODO: Consider using display-hints for types of atoms.
  fn visit_u64(&mut self, v: u64) -> Result<(), Error> {
//     writeln!(std::io::stderr(), "Serializer::visit_u64: {:?}", v).unwrap();
    Ok(try!(self.write_str(v.to_string())))
  }
  fn visit_i64(&mut self, v: i64) -> Result<(), Error> {
//    writeln!(std::io::stderr(), "Serializer::visit_i64: {:?}", v).unwrap();
    try!(self.write_str(v.to_string()));
    Ok(())
  }
  fn visit_f64(&mut self, v: f64) -> Result<(), Error> {
//    writeln!(std::io::stderr(), "Serializer::visit_f64: {:?}", v).unwrap();
    try!(self.write_str(v.to_string()));
    Ok(())
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
    try!(v.serialize(self));
    self.close()
  }
  fn visit_seq<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
    // writeln!(std::io::stderr(), "Serializer::visit_seq").unwrap();
    try!(self.open());
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }
//
//   fn visit_named_seq<V>(&mut self, name: &'static str, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
//     // writeln!(std::io::stderr(), "Serializer::visit_named_seq:{}", name).unwrap();
//     try!(self.open());
//     try!(self.write_str(name.to_string()));
//     while let Some(()) = try!(visitor.visit(self)) {}
//     self.close()
//   }
//
  fn visit_seq_elt<T>(&mut self, v: T) -> Result<(), Error> where T: ser::Serialize {
//     writeln!(std::io::stderr(), "Serializer::visit_seq_elt").unwrap();
    v.serialize(self)
  }
  fn visit_map<V>(&mut self, mut visitor: V) -> Result<(), Error> where V: ser::MapVisitor {
//    writeln!(std::io::stderr(), "Serializer::visit_map").unwrap();
    try!(self.open());
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
  }

  fn visit_map_elt<K, V>(&mut self, key: K, value: V) -> Result<(), Error>
    where K: ser::Serialize, V:  ser::Serialize {
//     writeln!(std::io::stderr(), "Serializer::visit_map_elt").unwrap();
    try!(key.serialize(self));
    value.serialize(self)
  }

   fn visit_unit_variant(&mut self, _name: &'static str,
                          _variant_index: usize,
                          variant: &'static str) -> Result<(), Error> {
     // writeln!(std::io::stderr(), "Serializer::visit_named_unit:{:?}", name).unwrap();
    try!(self.open());
    try!(self.write_str(variant.to_string()));
    Ok(try!(self.close()))
   }

   fn visit_tuple_variant<V: ser::SeqVisitor>(&mut self, _name: &'static str,
                              _variant_index: usize,
                              variant: &'static str, mut visitor: V)  -> Result<(), Error> {
     // writeln!(std::io::stderr(), "Serializer::visit_named_unit:{:?}", name).unwrap();
    try!(self.open());
    try!(self.write_str(variant.to_string()));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
   }

   fn visit_struct_variant<V: ser::MapVisitor>(&mut self, _name: &'static str,
                              _variant_index: usize,
                              variant: &'static str, mut visitor: V)  -> Result<(), Error> {
    try!(self.open());
    try!(self.write_str(variant.to_string()));
    while let Some(()) = try!(visitor.visit(self)) {}
    self.close()
   }
}
