#![feature(plugin, custom_attribute, custom_derive)]
#![plugin(quickcheck_macros)]
#![plugin(serde_macros)]

extern crate serde;
extern crate spki_sexp;
extern crate quickcheck;

use quickcheck::{Arbitrary};
use serde::{ser,de};
use std::fmt;
use std::io::Write;
use std::iter::Iterator;

use spki_sexp::*;

fn vec8_as_str(v :&Vec<u8>) -> String {
  String::from_utf8_lossy(v).into_owned()
}

#[derive(PartialEq, Eq,Debug, Clone)]
struct Tok(spki_sexp::SexpToken);

fn detok(t: &Tok) -> SexpToken {
  match t {
    &Tok(ref sexp) => sexp.clone()
  }
}

impl quickcheck::Arbitrary for Tok {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> Tok {
    match u64::arbitrary(g) % 3 {
      0 => Tok(SexpToken::OpenParen),
      1 => Tok(SexpToken::CloseParen),
      2 => Tok(SexpToken::Atom(quickcheck::Arbitrary::arbitrary(g))),
      n => panic!("Unexpected value mod 3: {:?}", n)
    }
  }

  fn shrink(&self) -> Box<Iterator<Item=Tok>+'static> {
//    writeln!(std::io::stderr(),"shrink: {:?}", self).unwrap();
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


#[quickcheck]
fn round_trip_tokens(toks : Vec<Tok>) -> Result<bool, spki_sexp::Error> {
  let toks : Vec<SexpToken> = toks.iter().map(detok).collect();
  // writeln!(std::io::stderr(),"orig: {:?}", toks).unwrap();
  let encd = encode((&toks).iter());
  // writeln!(std::io::stderr(),"Encoded: {:?}", vec8_as_str(&encd)).unwrap();
  let res : Vec<SexpToken> = match tokenise(encd.into_iter().map(Ok)).collect() {
    Ok(res) => res,
    Err(e) => {
      // writeln!(std::io::stderr(),"Decode error: {:?}", e).unwrap();
     return Err(e.into());
    }
  };
  // writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?} => {:?}", toks, (), res, res == toks).unwrap();
  Ok(res == toks)
}

fn round_trip_prop<T : fmt::Debug + ser::Serialize + de::Deserialize + PartialEq>(val: T, verbose: bool, equalish: fn(&T, &T) -> bool) -> Result<bool, Error> {
  let encd = try!(as_bytes(&val));
  if verbose { writeln!(std::io::stderr(),"round_trip: {:?} -> {:?}", val, vec8_as_str(&encd)).unwrap(); };
  let dec = try!(from_bytes::<T>(encd.as_slice()));
  if verbose { writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?}", val, vec8_as_str(&encd), dec).unwrap(); };
  Ok(equalish(&dec, &val))
}

fn round_trip_prop_eq<T : fmt::Debug + ser::Serialize + de::Deserialize + PartialEq>(val: T, verbose: bool) -> Result<bool, Error> {
  round_trip_prop(val, verbose, std::cmp::PartialEq::eq)
}


#[quickcheck]
fn serde_round_trip_unit(val: ()) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_string(val: String) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_u64(val: u64) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_bool(val: bool) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_u8(val: u8) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_isize(val: isize) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

// Textual floating point representations will always be lossy, so this is something of a fudge.
// http://c-faq.com/fp/fpequal.html
fn close_enough(x: &f64, y: &f64) -> bool {
  let epsilon = 0.00001;
  let max = x.abs().max(y.abs());
  if max == 0.0 {
    true
  } else {
    let delta = x.abs_sub(*y) / max;
    let ret = delta <= epsilon;
    // writeln!(std::io::stderr(),"delta: {:?} - {:?} -> {:?}; eps: {:?}; eq? {:?}", x, y, delta, epsilon, ret).unwrap();
    ret
  }
}
// TODO: Also check for NaN / Infinites
#[quickcheck]
fn serde_round_trip_f64(val: f64) -> Result<bool, Error> {
  round_trip_prop(val, false, close_enough)
}

#[quickcheck]
fn serde_round_trip_vec_string(val: Vec<String>) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_vec_u64(val: Vec<u64>) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_u64(val: (u64,)) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_u64_u64(val: (u64,u64)) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_u64_u64_u64(val: (u64,u64,u64)) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_string_u64(val: (String,u64)) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_map_u64_u64(val: std::collections::HashMap<u64,u64>) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_option_u64(val: Option<u64>) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_option_string(val: Option<String>) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct MyUnityType;
impl quickcheck::Arbitrary for MyUnityType {
  fn arbitrary<G: quickcheck::Gen>(_ : &mut G) -> MyUnityType {
    MyUnityType
  }
}

#[quickcheck]
fn serde_round_trip_unity_struct(val: MyUnityType) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct StructTuple(i32, i32);

impl quickcheck::Arbitrary for StructTuple {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> StructTuple {
    StructTuple(i32::arbitrary(g), i32::arbitrary(g))
  }
}

#[quickcheck]
fn serde_round_trip_struct_tuple(val: StructTuple) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl quickcheck::Arbitrary for Point {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> Point {
    Point { x: i32::arbitrary(g), y: i32::arbitrary(g) }
  }
}

#[quickcheck]
fn serde_round_trip_named_struct(val: Point) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_onetuple_named_struct(val: (Point,)) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_option_point(val: Option<Point>) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
enum SomeEnum {
  Foo,
  Quux,
  Bar(u64),
  Baz { some: i32 }
}

impl quickcheck::Arbitrary for SomeEnum {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> SomeEnum {
    match u64::arbitrary(g) % 4 {
      0 => SomeEnum::Foo,
      1 => SomeEnum::Quux,
      2 => SomeEnum::Bar(quickcheck::Arbitrary::arbitrary(g)),
      3 => SomeEnum::Baz { some: quickcheck::Arbitrary::arbitrary(g) },
      n => panic!("Unexpected value mod 4: {:?}", n)
    }
  }

  fn shrink(&self) -> Box<Iterator<Item=SomeEnum>+'static> {
//    writeln!(std::io::stderr(),"shrink: {:?}", self).unwrap();
    match *self {
      SomeEnum::Foo => quickcheck::empty_shrinker(),
      SomeEnum::Quux => quickcheck::single_shrinker(SomeEnum::Foo),
      SomeEnum::Bar(ref x) => {
	let chained = quickcheck::single_shrinker(SomeEnum::Foo)
		      .chain(x.shrink().map(SomeEnum::Bar));
	Box::new(chained)
      }
      SomeEnum::Baz { some: x } => {
	let chained = quickcheck::single_shrinker(SomeEnum::Foo)
		      .chain(x.shrink().map(|n| SomeEnum::Baz { some: n}));
	Box::new(chained)
      } 
    }
  }
}

#[quickcheck]
fn serde_round_trip_someenum(val: SomeEnum) -> Result<bool, Error> {
  round_trip_prop_eq(val, false)
}
