#![feature(plugin, custom_attribute, custom_derive)]
#![plugin(quickcheck_macros)]
#![plugin(serde_macros)]

extern crate serde;
extern crate spki_sexp;
extern crate quickcheck;
extern crate rustc_serialize;
extern crate num;
#[cfg(nightly)]
extern crate test;

use quickcheck::{Arbitrary};
use serde::{ser,de};
use std::fmt;
use std::io::Write;
use std::iter::{Iterator, FromIterator};

use spki_sexp::*;
use spki_sexp::SexpInfo::*;

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


//#[quickcheck]
fn round_trip_tokens(toks : Vec<Tok>) -> bool {
  let toks : Vec<SexpToken> = toks.iter().map(detok).collect();
//   writeln!(std::io::stderr(),"orig: {:?}", toks).unwrap();
  let encd = encode((&toks).iter()).unwrap();
//   writeln!(std::io::stderr(),"{:?}", vec8_as_str(&encd)).unwrap();
  let res = Vec::from_iter(tokenise(encd.iter().map(|p|*p)));
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
thread_local!(static DEPTH: std::cell::RefCell<usize> = std::cell::RefCell::new(0));

#[quickcheck]
fn serde_round_trip_u64(val: u64) -> bool {
  round_trip_prop_eq(val, false)
}

impl Drop for DepthGuard {
    fn drop(&mut self) {
        DEPTH.with(|d| { *d.borrow_mut() -= 1 });
    }
}

impl<'a> quickcheck::Arbitrary for ASexp {
  fn arbitrary<G: quickcheck::Gen>(g : &mut G) -> ASexp {
    let guard = DepthGuard::new();
    let ndeep = guard.depth();
    // writeln!(std::io::stderr(),"Depth: {:?} / {:?}", ndeep, 1 <<ndeep).unwrap();

    match u64::arbitrary(g) % (1<<ndeep) {
        0 => ASexp::List(Arc::new(Arbitrary::arbitrary(g))),
          _ => {
              let word : String = Arbitrary::arbitrary(g);
              ASexp::Atom(word.into_bytes())
          }
    }
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
fn serde_round_trip_vec_string(val: Vec<String>) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_vec_u64(val: Vec<u64>) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_u64(val: (u64,)) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_u64_u64(val: (u64,u64)) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_u64_u64_u64(val: (u64,u64,u64)) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_tuple_string_u64(val: (String,u64)) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_map_u64_u64(val: std::collections::HashMap<u64,u64>) -> bool {
  round_trip_prop_eq(val, false)
}

fn round_trip_prop_eq<T : fmt::Debug + Encodable + Decodable + PartialEq>(val: T, verbose: bool) -> bool{
  round_trip_prop(val, verbose, std::cmp::PartialEq::eq)
}

trait Epsilon { fn eps() -> Self; }

impl Epsilon for f64 { fn eps() -> f64 { 0.00001f64 } }
impl Epsilon for f32 { fn eps() -> f32 { 0.0001f32 } }

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct MyUnityType;
impl quickcheck::Arbitrary for MyUnityType {
  fn arbitrary<G: quickcheck::Gen>(_ : &mut G) -> MyUnityType {
    MyUnityType
  }
}

macro_rules! qc_round_trip {
    ($fn_name:ident, $type_a:ty) => (
         #[test] fn $fn_name() {
            fn prop(val: $type_a) -> bool { round_trip_prop_eq(val, false) }
            quickcheck(prop as fn($type_a) -> bool);
        }
    );
    ($fn_name:ident, $type_a:ty, $pat:pat => $expr:expr) => (
         #[test] fn $fn_name() {
            fn prop(val: $type_a) -> bool { let $pat = val; round_trip_prop_eq($expr, false) }
            quickcheck(prop as fn($type_a) -> bool);
        }
    )
}


qc_round_trip!(round_trip_unit, ());
qc_round_trip!(round_trip_bool, bool);

qc_round_trip!(round_trip_usize, usize);
qc_round_trip!(round_trip_u64, u64);
qc_round_trip!(round_trip_u8, u8);
qc_round_trip!(round_trip_isize, isize);
qc_round_trip!(round_trip_i64, i64);
qc_round_trip!(round_trip_i8, i8);

//#[quickcheck] fn round_trip_f64(val: f64) -> bool { round_trip_prop(val, false, close_enough::<f64>) }
//#[quickcheck] fn round_trip_f32(val: f32) -> bool { round_trip_prop(val, false, close_enough::<f32>) }

qc_round_trip!(round_trip_char, char);
qc_round_trip!(round_trip_string, String);
qc_round_trip!(round_trip_tuple_u64_u64, (u64, u64));
qc_round_trip!(round_trip_vec_u64, Vec<u64>);
qc_round_trip!(round_trip_option_u64, Option<u64>);

#[quickcheck]
fn serde_round_trip_named_struct(val: Point) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_onetuple_named_struct(val: (Point,)) -> bool {
  round_trip_prop_eq(val, false)
}

#[quickcheck]
fn serde_round_trip_option_point(val: Option<Point>) -> bool {
  round_trip_prop_eq(val, false)
}

#[derive(PartialEq,Eq,Debug,RustcEncodable,RustcDecodable)]
struct NameStruct { x: String, y: String }
qc_round_trip!(round_trip_name_struct, (String, String), (a,b) => NameStruct{ x: a, y: b });

#[derive(Clone,PartialEq,Eq,Debug,RustcEncodable,RustcDecodable)]
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
fn serde_round_trip_someenum(val: SomeEnum) -> bool {
  round_trip_prop_eq(val, false)
}
