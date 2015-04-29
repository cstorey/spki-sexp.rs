#![feature(plugin, custom_attribute, custom_derive)]
#![feature(convert)]
#![plugin(quickcheck_macros)]

extern crate spki_sexp;
extern crate quickcheck;

use quickcheck::{Arbitrary, Gen};
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
