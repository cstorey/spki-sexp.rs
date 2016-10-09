extern crate serde;
extern crate spki_sexp;
#[macro_use]
extern crate quickcheck;
extern crate env_logger;
#[macro_use]
extern crate log;

mod lib_types;
use lib_types::*;

use serde::{ser, de};
use std::fmt;
use std::cmp;
use std::result;
use std::iter::Iterator;

use spki_sexp::*;

fn vec8_as_str(v: &[u8]) -> String {
    String::from_utf8_lossy(v).into_owned()
}

#[derive(PartialEq, Eq,Debug, Clone)]
struct Tok(spki_sexp::SexpToken);

fn detok(t: &Tok) -> SexpToken {
    match t {
        &Tok(ref sexp) => sexp.clone(),
    }
}

impl quickcheck::Arbitrary for Tok {
    fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Tok {
        match u64::arbitrary(g) % 3 {
            0 => Tok(SexpToken::OpenParen),
            1 => Tok(SexpToken::CloseParen),
            2 => {
                let mut val: Vec<u8> = quickcheck::Arbitrary::arbitrary(g);
                val.truncate(8);
                Tok(SexpToken::Atom(val))
            }
            n => panic!("Unexpected value mod 3: {:?}", n),
        }
    }

    fn shrink(&self) -> Box<Iterator<Item = Tok> + 'static> {
        match *self {
            Tok(SexpToken::OpenParen) => quickcheck::empty_shrinker(),
            Tok(SexpToken::CloseParen) => quickcheck::empty_shrinker(),
            Tok(SexpToken::Atom(ref x)) => {
                let chained = x.shrink().map(SexpToken::Atom).map(Tok);
                Box::new(chained)
            }
        }
    }
}


quickcheck! {
    fn round_trip_tokens(toks : Vec<Tok>) -> Result<bool> {
        fn okay<X>(val: X) -> result::Result<X, TokenError> { Ok(val) }

        let toks : Vec<SexpToken> = toks.iter().map(detok).collect();
        debug!("orig: {:?}", toks);
        let encd : Vec<u8> = encode((&toks).iter());
        debug!("Encoded: {:?}", vec8_as_str(&encd));
        let res : Vec<SexpToken> = match tokenise(encd.into_iter().map(okay)).collect() {
            Ok(res) => res,
                Err(e) => {
                    debug!("Decode error: {:?}", e);
                    return Err(e.into());
                }
        };
        debug!("{:?} -> {:?} -> {:?} => {:?}", toks, (), res, res == toks);
        Ok(res == toks)
    }
}

fn round_trip_prop<T: fmt::Debug + ser::Serialize + de::Deserialize + PartialEq>
    (val: T,
     equalish: fn(&T, &T) -> bool)
     -> Result<bool> {
    env_logger::init().unwrap_or(());
    info!("---");
    let encd = try!(as_bytes(&val));
    info!("encoded: {:?} -> {:?}", val, vec8_as_str(&encd));
    let dec = try!(from_bytes::<T>(encd.as_slice()));
    let res = equalish(&dec, &val);
    info!("decoded: {:?} -> {:?} -> {:?}; okay? {:?}",
          val,
          vec8_as_str(&encd),
          dec,
          res);
    Ok(res)
}

#[test]
fn test_bytes_repr() {
    use serde::bytes::Bytes;
    let val: Bytes = b"hello world!".as_ref().into();
    let encd = as_bytes(&val).expect("encode");
    assert_eq!(String::from_utf8_lossy(&encd), "12:hello world!");
    assert_eq!(encd, b"12:hello world!");
}

fn round_trip_prop_eq<T: fmt::Debug + ser::Serialize + de::Deserialize + PartialEq>
    (val: T)
     -> Result<bool> {
    round_trip_prop(val, cmp::PartialEq::eq)
}


quickcheck! {
    fn serde_round_trip_unit(val: ()) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_string(val: String) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_u64(val: u64) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_bool(val: bool) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_u8(val: u8) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_isize(val: isize) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

// Textual floating point representations will always be lossy, so this is something of a fudge.
// http://c-faq.com/fp/fpequal.html
fn close_enough(x: &f64, y: &f64) -> bool {
    let epsilon = 0.00001;
    let max = x.abs().max(y.abs());
    if max == 0.0 {
        true
    } else {
        let delta = (x - *y).abs() / max;
        let ret = delta <= epsilon;
        debug!("delta: {:?} - {:?} -> {:?}; eps: {:?}; eq? {:?}",
               x,
               y,
               delta,
               epsilon,
               ret);
        ret
    }
}
// TODO: Also check for NaN / Infinites
quickcheck! {
    fn serde_round_trip_f64(val: f64) -> Result<bool> {
        round_trip_prop(val, close_enough)
    }
}

quickcheck! {
    fn serde_round_trip_vec_string(val: Vec<String>) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_vec_u64(val: Vec<u64>) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_bytes(val: Vec<u8>) -> Result<bool> {
        use serde::bytes::ByteBuf;
        let val : ByteBuf = val.into();
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_tuple_u64(val: (u64,)) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_tuple_u64_u64(val: (u64,u64)) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_tuple_u64_u64_u64(val: (u64,u64,u64)) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_tuple_string_u64(val: (String,u64)) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_map_u64_u64(val: std::collections::HashMap<u64,u64>) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_option_u64(val: Option<u64>) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_option_string(val: Option<String>) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_unity_struct(val: MyUnityType) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_newtype_struct(val: MyNewtype) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_struct_tuple(val: StructTuple) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_named_struct(val: Point) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_onetuple_named_struct(val: (Point,)) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_option_point(val: Option<Point>) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}

quickcheck! {
    fn serde_round_trip_someenum(val: SomeEnum) -> Result<bool> {
        round_trip_prop_eq(val)
    }
}


quickcheck! {
    fn serde_round_trip_incremental_option_u64(toks: Vec<Tok>, chunks: Vec<u16>) -> Result<bool> {
        let toks : Vec<SexpToken> = toks.iter().map(detok).collect();
        debug!("orig: {:?}", toks);
        let buf = encode((&toks).iter());
        debug!("Encoded: {:?}", vec8_as_str(&buf));

        let mut dec = spki_sexp::Tokeniser::new();
        let mut outputs = Vec::new();
        let offs = vec![0].into_iter().chain(chunks.into_iter())
            .scan(0usize, |a, n| { *a += n as usize; Some (*a)} )
            .take_while(|&n| n < buf.len())
            .chain(vec![buf.len()].into_iter())
            .collect::<Vec<usize>>();

        debug!("Offsets: {:?}", offs);
        for (&start, &end) in offs.iter().zip(offs.iter().skip(1)) {
            dec.feed(&buf[start..end]);
            debug!("Feed: {:?}, {:?}", start..end, vec8_as_str(&buf[start..end]));

            while let Some(it) = try!(dec.take()) {
                debug!("Took: {:?}", it);
                outputs.push(it);
            }
        }

        debug!("Result: {:?}: {:?}", outputs == toks, outputs);

        Ok(outputs == toks)
    }
}

quickcheck! {
    fn serde_round_trip_incremental_framing(items: Vec<SomeEnum>, chunks: Vec<u16>) -> Result<bool> {
        debug!("orig: {:?}", items);
        let mut buf = Vec::new();
        for it in items.iter() {
            to_writer(&mut buf, it).expect("to_writer");
        }

        debug!("Encoded: {:?}", vec8_as_str(&buf));

        let mut packets = Packetiser::new();
        let mut outputs : Vec<SomeEnum> = Vec::new();

        let offs = vec![0].into_iter().chain(chunks.into_iter())
            .scan(0usize, |a, n| { *a += n as usize; Some (*a)} )
            .take_while(|&n| n < buf.len())
            .chain(vec![buf.len()].into_iter())
            .collect::<Vec<usize>>();

        debug!("Offsets: {:?}", offs);
        for (&start, &end) in offs.iter().zip(offs.iter().skip(1)) {
            packets.feed(&buf[start..end]);
            debug!("Feed: {:?}, {:?}", start..end, vec8_as_str(&buf[start..end]));

            while let Some(it) = try!(packets.take()) {
                debug!("Took: {:?}", it);
                outputs.push(it);
            }
        }

        debug!("Result: {:?}: {:?}", outputs == items, outputs);

        Ok(outputs == items)
    }
}

quickcheck! {
    fn serde_round_trip_incremental_framing_atom(items: Vec<usize>, chunks: Vec<u16>) -> Result<bool> {
        debug!("orig: {:?}", items);
        let mut buf = Vec::new();
        for it in items.iter() {
            to_writer(&mut buf, it).expect("to_writer");
        }

        debug!("Encoded: {:?}", vec8_as_str(&buf));

        let mut packets = Packetiser::new();
        let mut outputs : Vec<usize> = Vec::new();

        let offs = vec![0].into_iter().chain(chunks.into_iter())
            .scan(0usize, |a, n| { *a += n as usize; Some (*a)} )
            .take_while(|&n| n < buf.len())
            .chain(vec![buf.len()].into_iter())
            .collect::<Vec<usize>>();

        debug!("Offsets: {:?}", offs);
        for (&start, &end) in offs.iter().zip(offs.iter().skip(1)) {
            packets.feed(&buf[start..end]);
            debug!("Feed: {:?}, {:?}", start..end, vec8_as_str(&buf[start..end]));

            while let Some(it) = try!(packets.take()) {
                debug!("Took: {:?}", it);
                outputs.push(it);
            }
        }

        debug!("Result: {:?}: {:?}", outputs == items, outputs);

        Ok(outputs == items)
    }
}
