#![feature(plugin, custom_attribute, custom_derive)]
#![feature(convert)]
#![feature(alloc)]
#![plugin(quickcheck_macros)]

extern crate spki_sexp;
extern crate quickcheck;

use quickcheck::{Arbitrary, Gen};
use std::{error,fmt};
use std::io::Write;
use std::iter::{Iterator, FromIterator,Peekable};
use std::sync::Arc;
use std::rc::{self, Rc};

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
	let chained = quickcheck::single_shrinker(ASexp::Atom("shrunken-list".to_string().into_bytes()))
		      .chain(l.shrink().map(|l| ASexp::List(Arc::new(l))));
	Box::new(chained)
      }
    }
  }
}


#[quickcheck]
fn wheeeeeee(val : ASexp) -> bool {
  let v = val.desexp();
  // writeln!(std::io::stderr(),"wheee: {:?}", val).unwrap();
  true
}
