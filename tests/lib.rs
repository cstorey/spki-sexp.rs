#![feature(plugin, custom_attribute, custom_derive)]
#![feature(convert)]
#![plugin(quickcheck_macros)]
#![plugin(serde_macros)]

extern crate serde;
extern crate spki_sexp;
extern crate quickcheck;

use quickcheck::{Arbitrary, Gen};
use serde::{ser,de};
use std::{error,fmt};
use std::io::Write;
use std::iter::{Iterator, FromIterator,Peekable};

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
      2 => Tok(SexpToken::Str(quickcheck::Arbitrary::arbitrary(g))),
      n => panic!("Unexpected value mod 3: {:?}", n)
    }
  }

  fn shrink(&self) -> Box<Iterator<Item=Tok>+'static> {
//    writeln!(std::io::stderr(),"shrink: {:?}", self).unwrap();
    match *self {
      Tok(SexpToken::OpenParen) => quickcheck::empty_shrinker(),
      Tok(SexpToken::CloseParen) => quickcheck::single_shrinker(Tok(SexpToken::OpenParen)),
      Tok(SexpToken::Str(ref x)) => {
	let chained = quickcheck::single_shrinker(Tok(SexpToken::CloseParen))
		      .chain(quickcheck::single_shrinker(Tok(SexpToken::OpenParen)))
		      .chain(x.shrink().map(SexpToken::Str).map(Tok));
	Box::new(chained)
      }
    }
  }
}


#[quickcheck]
fn round_trip_tokens(toks : Vec<Tok>) -> bool {
  let toks : Vec<SexpToken> = toks.iter().map(detok).collect();
//   writeln!(std::io::stderr(),"orig: {:?}", toks).unwrap();
  let encd = encode((&toks).iter());
//   writeln!(std::io::stderr(),"{:?}", vec8_as_str(&encd)).unwrap();
  let mut res = Vec::from_iter(tokenise(encd.iter().map(|p|*p)));
//   writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?} => {:?}", toks, vec8_as_str(&encd), res, res == toks).unwrap();
  res == toks
}

fn round_trip_prop<T : fmt::Debug + ser::Serialize + de::Deserialize + PartialEq>(val: T) -> bool{
  let encd = as_bytes(&val);
//  writeln!(std::io::stderr(),"round_trip: {:?} -> {:?}", val, vec8_as_str(&encd)).unwrap();
  let dec = from_bytes::<T>(encd.as_slice());
//   writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?}", val, vec8_as_str(&encd), dec).unwrap();
  dec.unwrap() == val
}

#[quickcheck]
fn serde_round_trip_unit(val: ()) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_string(val: String) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_vec_string(val: Vec<String>) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_u64(val: u64) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_u8(val: u8) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_tuple_u64(val: (u64,)) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_tuple_u64_u64(val: (u64,u64)) -> bool {
  round_trip_prop(val)
}
#[quickcheck]
fn serde_round_trip_tuple_u64_u64_u64(val: (u64,u64,u64)) -> bool {
  round_trip_prop(val)
}
#[quickcheck]
fn serde_round_trip_tuple_string_u64(val: (String,u64)) -> bool {
  round_trip_prop(val)
}

#[quickcheck]
fn serde_round_trip_map_u64_u64(val: std::collections::HashMap<u64,u64>) -> bool {
  round_trip_prop(val)
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct MyUnityType;
impl quickcheck::Arbitrary for MyUnityType {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> MyUnityType {
    MyUnityType
  }
}

#[quickcheck]
fn serde_round_trip_unity_struct(val: MyUnityType) -> bool {
  round_trip_prop(val)
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

// #[quickcheck]
fn serde_round_trip_named_struct(val: Point) -> bool {
  round_trip_prop(val)
}

fn should_raise_error_when_wrong_struct_name() {}
