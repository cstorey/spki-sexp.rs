use std::io::{Write, Cursor};
use std::fmt;

use serde::ser;
use itoa;

use super::Error;

pub struct Serializer<W> {
    writer: W,
}

impl<W> Serializer<W>
    where W: Write
{
    pub fn new(wr: W) -> Serializer<W> {
        Serializer { writer: wr }
    }

    fn write_str(&mut self, s: &str) -> Result<(), Error> {
        self.write_bytes(s.as_bytes())
    }

    fn write_bytes(&mut self, s: &[u8]) -> Result<(), Error> {
        try!(itoa::write(&mut self.writer, s.len()));
        try!(self.writer.write_all(b":"));
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

macro_rules! impl_displayable_atom {
    ($ty:ty, $ser_method:ident) => {
        fn $ser_method(&mut self, v: $ty) -> Result<(), Error> {
            self.write_displayable(v)
        }
    }
}

impl<W> ser::Serializer for Serializer<W>
    where W: Write
{
    type SeqState = ();
    type TupleState = ();
    type TupleStructState = ();
    type TupleVariantState = ();
    type MapState = ();
    type StructState = ();
    type StructVariantState = ();

    type Error = Error;
    //
    fn serialize_unit(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn serialize_unit_struct(&mut self, _name: &str) -> Result<(), Error> {
        Ok(())
    }

    fn serialize_bool(&mut self, val: bool) -> Result<(), Error> {
        trace!("Serializer::serialize_bool: {:?}", val);
        if val {
            try!(self.write_str("true"))
        } else {
            try!(self.write_str("false"))
        }
        Ok(())
    }

    impl_displayable_atom!(usize, serialize_usize);
    impl_displayable_atom!(u64, serialize_u64);
    impl_displayable_atom!(u32, serialize_u32);
    impl_displayable_atom!(u16, serialize_u16);
    impl_displayable_atom!(u8, serialize_u8);

    impl_displayable_atom!(isize, serialize_isize);
    impl_displayable_atom!(i64, serialize_i64);
    impl_displayable_atom!(i32, serialize_i32);
    impl_displayable_atom!(i16, serialize_i16);
    impl_displayable_atom!(i8, serialize_i8);

    impl_displayable_atom!(f64, serialize_f64);
    impl_displayable_atom!(f32, serialize_f32);

    impl_displayable_atom!(char, serialize_char);

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
    fn serialize_some<T>(&mut self, v: T) -> Result<(), Error>
        where T: ser::Serialize
    {
        //    writeln!(std::io::stderr(), "Serializer::serialize_some(?)").unwrap();
        try!(self.open());
        try!(self.write_str("some"));
        try!(v.serialize(self));
        self.close()
    }
    fn serialize_seq(&mut self, _len: Option<usize>) -> Result<Self::SeqState, Self::Error> {
        trace!("Serializer::serialize_seq");
        try!(self.open());
        Ok(())
    }
    fn serialize_seq_fixed_size(&mut self, len: usize) -> Result<Self::SeqState, Self::Error> {
        self.serialize_seq(Some(len))
    }
    //   fn serialize_named_seq<V>(&mut self, name: &'static str, mut visitor: V) -> Result<(), Error> where V: ser::SeqVisitor {
    //     // trace!("Serializer::serialize_named_seq:{}", name);
    //     try!(self.open());
    //     try!(self.write_str(name));
    //     while let Some(()) = try!(visitor.visit(self)) {}
    //     self.close()
    //   }
    //
    fn serialize_seq_elt<V: ser::Serialize>(&mut self, _: &mut (), value: V) -> Result<(), Error> {
        trace!("Serializer::serialize_seq_elt");
        value.serialize(self)
    }
    fn serialize_seq_end(&mut self, _: ()) -> Result<(), Error> {
        try!(self.close());
        Ok(())
    }

    fn serialize_map(&mut self, _len: Option<usize>) -> Result<(), Error> {
        try!(self.open());
        Ok(())
    }

    fn serialize_map_key<K: ser::Serialize>(&mut self, _: &mut (), key: K) -> Result<(), Error> {
        trace!("Serializer::serialize_map_key");
        try!(key.serialize(self));
        Ok(())
    }

    fn serialize_map_value<V: ser::Serialize>(&mut self,
                                              _: &mut (),
                                              value: V)
                                              -> Result<(), Error> {
        trace!("Serializer::serialize_map_value");
        try!(value.serialize(self));
        Ok(())
    }

    fn serialize_map_end(&mut self, _: ()) -> Result<(), Error> {
        self.close()
    }

    fn serialize_tuple(&mut self, len: usize) -> Result<(), Error> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_elt<V: ser::Serialize>(&mut self, st: &mut (), val: V) -> Result<(), Error> {
        self.serialize_seq_elt(st, val)
    }
    fn serialize_tuple_end(&mut self, st: ()) -> Result<(), Error> {
        self.serialize_seq_end(st)
    }
    fn serialize_tuple_struct(&mut self, _name: &str, len: usize) -> Result<(), Error> {
        try!(self.serialize_tuple(len));
        Ok(())
    }
    fn serialize_tuple_struct_elt<V: ser::Serialize>(&mut self,
                                                     st: &mut (),
                                                     val: V)
                                                     -> Result<(), Error> {
        self.serialize_tuple_elt(st, val)
    }

    fn serialize_tuple_struct_end(&mut self, st: ()) -> Result<(), Error> {
        self.serialize_tuple_end(st)
    }


    fn serialize_unit_variant(&mut self,
                              _name: &'static str,
                              _variant_index: usize,
                              variant: &'static str)
                              -> Result<(), Error> {
        trace!("Serializer::serialize_named_unit:{:?}", _name);
        try!(self.open());
        try!(self.write_str(variant));
        Ok(try!(self.close()))
    }

    fn serialize_newtype_variant<T: ser::Serialize>(&mut self,
                                                    _name: &'static str,
                                                    _variant_index: usize,
                                                    variant: &'static str,
                                                    value: T)
                                                    -> Result<(), Error> {
        trace!("Serializer::serialize_newtype_variant:{:?}/{:?}",
               _name,
               variant);
        try!(self.open());
        try!(self.write_str(variant));
        try!(value.serialize(self));
        Ok(try!(self.close()))
    }


    fn serialize_tuple_variant(&mut self,
                               _name: &'static str,
                               _variant_index: usize,
                               variant: &'static str,
                               _len: usize)
                               -> Result<(), Error> {
        trace!("Serializer::serialize_tuple_variant:{:?}/{:?}",
               _name,
               variant);
        try!(self.open());
        try!(self.write_str(variant));
        Ok(())
    }

    fn serialize_tuple_variant_elt<V: ser::Serialize>(&mut self,
                                                      _: &mut (),
                                                      value: V)
                                                      -> Result<(), Error> {
        trace!("Serializer::serialize_tuple_variant_elt");
        value.serialize(self)
    }

    fn serialize_tuple_variant_end(&mut self, _: ()) -> Result<(), Error> {
        trace!("Serializer::serialize_tuple_variant_end");
        try!(self.close());
        Ok(())
    }

    fn serialize_newtype_struct<V: ser::Serialize>(&mut self,
                                                   _name: &'static str,
                                                   v: V)
                                                   -> Result<(), Error> {
        trace!("Serializer::serialize_newtype_struct: {:?}", _name);
        v.serialize(self)
    }

    fn serialize_struct(&mut self, _name: &'static str, _len: usize) -> Result<(), Error> {
        trace!("Serializer::serialize_struct: {:?}/{:?}", _name, _len);
        try!(self.open());
        Ok(())
    }

    fn serialize_struct_elt<V: ser::Serialize>(&mut self,
                                               _: &mut (),
                                               key: &'static str,
                                               value: V)
                                               -> Result<(), Error> {
        trace!("Serializer::serialize_struct_elt: {:?}", key);
        try!(self.write_str(key));
        try!(value.serialize(self));
        Ok(())
    }

    fn serialize_struct_end(&mut self, _: ()) -> Result<(), Error> {
        trace!("Serializer::serialize_struct_end");
        try!(self.close());
        Ok(())
    }


    fn serialize_struct_variant(&mut self,
                                _name: &'static str,
                                _variant_index: usize,
                                variant: &'static str,
                                _len: usize)
                                -> Result<(), Error> {
        trace!("Serializer::serialize_struct_variant: {:?}/{:?}/{:?}",
               _name,
               variant,
               _len);
        try!(self.open());
        try!(self.write_str(variant));
        Ok(())
    }
    fn serialize_struct_variant_elt<V: ser::Serialize>(&mut self,
                                                       _st: &mut (),
                                                       _key: &'static str,
                                                       value: V)
                                                       -> Result<(), Error> {
        trace!("Serializer::serialize_struct_variant_elt: {:?}", _key);
        self.serialize_struct_elt(_st, _key, value)
    }
    fn serialize_struct_variant_end(&mut self, _: ()) -> Result<(), Error> {
        trace!("Serializer::serialize_struct_variant_end");
        self.serialize_struct_end(())
    }
}
