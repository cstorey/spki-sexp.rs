#![feature(plugin, custom_attribute, custom_derive)]
#![feature(convert)]
#![feature(alloc)]
#![plugin(quickcheck_macros)]

extern crate spki_sexp;
extern crate quickcheck;
extern crate rustc_serialize;
extern crate num;

use quickcheck::{Arbitrary, Gen};
use std::{error,fmt};
use std::io::{self, Write};
use std::iter::{Iterator, FromIterator,Peekable};
use std::sync::Arc;
use std::rc::{self, Rc};
use std::result::Result;
use std::collections::HashMap;

use rustc_serialize::{Decodable, Encodable};

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
      Tok(SexpToken::CloseParen) => quickcheck::single_shrinker(Tok(SexpToken::OpenParen)),
      Tok(SexpToken::Atom(ref x)) => {
	let chained = quickcheck::single_shrinker(Tok(SexpToken::CloseParen))
		      .chain(quickcheck::single_shrinker(Tok(SexpToken::OpenParen)))
		      .chain(x.shrink().map(SexpToken::Atom).map(Tok));
	Box::new(chained)
      }
    }
  }
}


#[quickcheck]
fn round_trip_tokens(toks : Vec<Tok>) -> bool {
  let toks : Vec<SexpToken> = toks.iter().map(detok).collect();
//   writeln!(std::io::stderr(),"orig: {:?}", toks).unwrap();
  let encd = encode((&toks).iter()).unwrap();
//   writeln!(std::io::stderr(),"{:?}", vec8_as_str(&encd)).unwrap();
  let mut res = Vec::from_iter(tokenise(encd.iter().map(|p|p.clone())));
//   writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?} => {:?}", toks, vec8_as_str(&encd), res, res == toks).unwrap();
  res == toks
}

// struct <'a>(spki_sexp::SexpInfo<'a>);
#[derive(PartialEq, Eq,Debug, Clone)]
pub enum ASexp {
  Atom(Vec<u8>),
  List(Arc<Vec<ASexp>>)
}

impl ASexp {
  fn desexp(&self) -> SexpInfo {
    match self {
      &ASexp::Atom(ref n) => SexpInfo::Atom(n.clone()),
      &ASexp::List(ref l) => SexpInfo::List(Rc::new(l.iter().map(ASexp::desexp).collect()))
    }
  }
}

impl fmt::Display for ASexp {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
     match self {
      &ASexp::Atom(ref n) => String::from_utf8_lossy(n).fmt(fmt),
      &ASexp::List(ref l) => {
	try!(fmt.write_str("("));
	for (n, val) in l.iter().enumerate() {
	  if n != 0 {
	    try!(fmt.write_str(" "))
	  }
	  try!(val.fmt(fmt))

	}
	fmt.write_str(")")
      }
    }
  }
}
thread_local!(static DEPTH: Rc<()> = Rc::new(()));

impl<'a> quickcheck::Arbitrary for ASexp {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> ASexp {
    DEPTH.with(|d| {
      let guard = d.clone();
      let ndeep = rc::strong_count(d);
      // writeln!(std::io::stderr(),"Depth: {:?} / {:?}", ndeep, 1 <<ndeep).unwrap();

      match u64::arbitrary(g) % (1<<ndeep) {
	0 => ASexp::List(Arc::new(Arbitrary::arbitrary(g))),
	_ => {
	  let word : String = Arbitrary::arbitrary(g);
	  ASexp::Atom(word.into_bytes())
	}
      }
    })
  }

  fn shrink(&self) -> Box<Iterator<Item=ASexp>+'static> {
//    writeln!(std::io::stderr(),"shrink: {:?}", self).unwrap();
    match *self {
      ASexp::Atom(ref x) => {
	let chained = quickcheck::empty_shrinker()
		      .chain(x.shrink().map(ASexp::Atom));
	Box::new(chained)
      }
      ASexp::List(ref l) => {
	let chained = l.shrink().map(|l| ASexp::List(Arc::new(l)))
		      .chain(quickcheck::single_shrinker(ASexp::Atom("shrunken-list".to_string().into_bytes())))
		      .map(|x| { writeln!(std::io::stderr(),"shrunk: {:?}", x).unwrap(); x }) ;
	Box::new(chained)
      }
    }
  }
}


#[quickcheck]
fn round_trip_sexp_tree(val : ASexp) -> bool {
  let v = val.desexp();
  // writeln!(std::io::stderr(),"wheee: {:?}", val).unwrap();
  let mut buf = vec![];
  v.write_to(&mut buf).unwrap();
  // writeln!(std::io::stderr(),"wheee: {:?}", vec8_as_str(&buf)).unwrap();
  let decd = SexpInfo::read_from(&mut io::Cursor::new(buf.clone())).unwrap();
  // writeln!(std::io::stderr(),"result: {:?} -> {:?} -> {:?}; {:?}", v, vec8_as_str(&buf), decd, v == decd).unwrap();
  decd == v
}

fn round_trip_prop<T : fmt::Debug + Encodable + Decodable + PartialEq>(val: T, verbose: bool, equalish: fn(&T, &T) -> bool) -> bool{
  let encd = spki_sexp::to_bytes(&val).unwrap();
  if verbose { writeln!(std::io::stderr(),"round_trip: {:?} -> {:?}", val, vec8_as_str(&encd)).unwrap(); };
  let dec = spki_sexp::from_bytes::<T>(encd.as_slice());
  if verbose { writeln!(std::io::stderr(),"{:?} -> {:?} -> {:?}", val, vec8_as_str(&encd), dec).unwrap(); };
  if let Ok(decoded) = dec {
    if verbose { writeln!(std::io::stderr(),"-> {:?}", equalish(&decoded, &val)).unwrap(); };
    equalish(&decoded, &val)
  } else {
    false
  }
}

fn round_trip_prop_eq<T : fmt::Debug + Encodable + Decodable + PartialEq>(val: T, verbose: bool) -> bool{
  round_trip_prop(val, verbose, std::cmp::PartialEq::eq)
}

trait Epsilon { fn eps() -> Self; }

impl Epsilon for f64 { fn eps() -> f64 { 0.00001f64 } }
impl Epsilon for f32 { fn eps() -> f32 { 0.00001f32 } }

fn close_enough<T>(x: &T, y: &T) -> bool where T: num::Float + Epsilon {
  let max = x.abs().max(y.abs());
  if max == T::zero() {
    true
  } else {
    let delta = x.abs_sub(*y) / max;
    let ret = delta <= T::eps();
    ret
  }
}

#[quickcheck] fn round_trip_unit(val: ()) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_bool(val: bool) -> bool { round_trip_prop_eq(val, false) }

#[quickcheck] fn round_trip_usize(val: usize) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_u64(val: u64) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_u8(val: u8) -> bool { round_trip_prop_eq(val, false) }

#[quickcheck] fn round_trip_isize(val: isize) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_i64(val: i64) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_i8(val: i8) -> bool { round_trip_prop_eq(val, false) }

#[quickcheck] fn round_trip_f64(val: f64) -> bool { round_trip_prop(val, false, close_enough::<f64>) }
#[quickcheck] fn round_trip_f32(val: f32) -> bool { round_trip_prop(val, false, close_enough::<f32>) }

#[quickcheck] fn round_trip_char(val: char) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_string(val: String) -> bool { round_trip_prop_eq(val, false) }

#[quickcheck] fn round_trip_tuple_u64_u64(val: (u64,u64)) -> bool { round_trip_prop_eq(val, false) }
#[quickcheck] fn round_trip_vec_u64(val: Vec<u64>) -> bool { round_trip_prop_eq(val, false) }

#[quickcheck] fn round_trip_option_u64(val: Option<u64>) -> bool { round_trip_prop_eq(val, false) }
#[test] fn parse_bad_none() { assert!(spki_sexp::from_bytes::<Option<u64>>(b"5:nodnol").is_err()) }
#[test] fn parse_bad_some_insufficient_items() { assert!(spki_sexp::from_bytes::<Option<u64>>(b"(4:some)").is_err()) }
#[test] fn parse_bad_some_too_many_items() { assert!(spki_sexp::from_bytes::<Option<u64>>(b"(4:some1:14:spam)").is_err()) }
#[test] fn parse_bad_some_bad_label() { assert!(spki_sexp::from_bytes::<Option<u64>>(b"(3:lol1:2)").is_err()) }
#[test] fn parse_bad_some_ok() { assert_eq!(spki_sexp::from_bytes::<Option<u64>>(b"(4:some2:99)").unwrap(), Some(99)) }

#[quickcheck] fn round_trip_map_u64_u64(val: HashMap<u64,u64>) -> bool { round_trip_prop_eq(val, false) }
