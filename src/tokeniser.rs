use std::iter::Iterator;
use std::collections::VecDeque;
use std::io;
use std::mem;
use std::cmp;
use super::SexpToken;

quick_error! {
    #[derive(Debug)]
    pub enum TokenError {
        /// IO Error
        Io(err: io::Error) {
            from()
            cause(err)
            description(err.description())
        }
        /// Arbitrary system error
        BadChar(char: u8) {
            description("Invalid character")
        }
    }
}



pub fn encode<'a, I>(it : I) -> Vec<u8> where I : Iterator<Item=&'a SexpToken> {
  let mut out = vec![];
  for t in it {
    match t {
      &SexpToken::OpenParen => out.push('(' as u8),
      &SexpToken::CloseParen => out.push(')' as u8),
      &SexpToken::Atom(ref bytes) => {
        out.extend(format!("{}", bytes.len()).into_bytes().as_slice());
        out.push(':' as u8);
        out.extend(bytes.as_slice())
      }
    }
  }
  out
}

pub struct TokenisingIterator<I: Iterator<Item=Result<u8, io::Error>>> {
  iter: I
}

const OPEN_PAREN : u8 = '(' as u8;
const CLOSE_PAREN : u8 = ')' as u8;
const COLON : u8 = ':' as u8;
const ZERO : u8 = '0' as u8;
const NINE : u8 = '9' as u8;

impl<'a, I> Iterator for TokenisingIterator<I> where I :Iterator<Item=Result<u8, io::Error>> {
  type Item = Result<SexpToken, TokenError>;
  fn next(&mut self) -> Option<Result<SexpToken, TokenError>> {
    use std::io::Write;
    let current = self.iter.next();
//    writeln!(::std::io::stderr(),"TokenisingIterator::next: read: {:?}", current);
    let r = match current {
      Some(Ok(c)) if c == OPEN_PAREN => { Some(Ok(SexpToken::OpenParen)) }
    , Some(Ok(c)) if c == CLOSE_PAREN => { Some(Ok(SexpToken::CloseParen)) }
    , Some(Ok(c)) if c >= ZERO && c <= NINE => {
        let s = match read_atom(c, &mut self.iter) { Ok(s) => s, Err(e) => return Some(Err(e)), };
        Some(Ok(SexpToken::Atom(s)))
    }
    , Some(Err(e)) => Some(Err(e.into()))
    , None => None
    , Some(Ok(other)) => {
//        writeln!(::std::io::stderr(),"TokenisingIterator::next: not {:?}, or {:?}, wat: {:?}({:?})", [OPEN_PAREN, CLOSE_PAREN], ZERO..NINE, other as char, other);
        Some(Err(TokenError::BadChar(other)))
    }
    };
    // writeln!(std::io::stderr(),"TokenisingIterator::next: peek:{:?} => {:?}" , current.map(|c| c as char), r);
    r
  }
}

fn read_atom<'a, I>(first_char: u8, it : &mut I) -> Result<Vec<u8>, TokenError> where I : Iterator<Item=Result<u8, io::Error>> {
    use std::io::Write;
//  writeln!(::std::io::stderr(),"read_atom: {:?}" , first_char as char);
  let mut len = first_char as usize  - ZERO as usize;
  while let Some(c) = it.next() {
    let c = try!(c);
    if c >= ZERO && c <= NINE {
      let digit = c - ZERO;
      len = len * 10 + digit as usize;
//      writeln!(::std::io::stderr(),"read_atom! {:?}; len:{:?}" , c as char, len).unwrap();
    } else if c == COLON {
//      writeln!(::std::io::stderr(),"read_atom! Colon!");
      break;
    } else {
//      writeln!(::std::io::stderr(),"read_atom! wat: {:?}", c);
      return Err(TokenError::BadChar(c))
    }
  }

//  writeln!(::std::io::stderr(),"read_atom! len: {:?}", len);
  let bytes : Vec<u8> = try!(it.take(len).collect());
//  writeln!(::std::io::stderr(),"read_atom! {:?}", s);
  Ok(bytes)
}



pub fn tokenise<I:Iterator<Item=Result<u8, io::Error>>>(it : I) -> TokenisingIterator<I> {
  TokenisingIterator { iter: it }
}

enum TokState {
    Start,
    ReadingLen(usize),
    ReadingAtomBody(usize, Vec<u8>),
}

pub struct Tokeniser {
    inbuf : VecDeque<u8>,
    state: TokState,
}

impl Tokeniser {
    pub fn new() -> Tokeniser {
        Tokeniser { inbuf: VecDeque::new(), state: TokState::Start }
    }

    pub fn feed(&mut self, buf: &[u8]) {
        self.inbuf.extend(buf)
    }

    pub fn take(&mut self) -> Option<Result<SexpToken, TokenError>> {
        if let TokState::Start = self.state {
            match self.inbuf.pop_front() {
              Some(c) if c == OPEN_PAREN => return Some(Ok(SexpToken::OpenParen))
            , Some(c) if c == CLOSE_PAREN => return Some(Ok(SexpToken::CloseParen))
            , Some(c) if c >= ZERO && c <= NINE => { self.state = TokState::ReadingLen((c - ZERO) as usize); }
            , None => return None
            , Some(other) => return Some(Err(TokenError::BadChar(other)))
            };
        }

        if let TokState::ReadingLen(n) = self.state {
            match self.inbuf.pop_front() {
              Some(c) if c >= ZERO && c <= NINE => { self.state = TokState::ReadingLen(n * 10 + (c - ZERO) as usize); }
            , Some(c) if c == COLON => { self.state = TokState::ReadingAtomBody(n, Vec::new()) }
            , Some(other) => return Some(Err(TokenError::BadChar(other)))
            , None => return None
            };
        }

        if let TokState::ReadingAtomBody(len, mut buf) = mem::replace(&mut self.state, TokState::Start) {
            // let remaining = ...
            let remaining = cmp::min(self.inbuf.len(), len - buf.len());
            buf.extend(self.inbuf.drain(..remaining));

            if buf.len() == len {
                return Some(Ok(SexpToken::Atom(buf)))
            } else {
                self.state = TokState::ReadingAtomBody(len, buf)
            }
        }
        None
    }
}
