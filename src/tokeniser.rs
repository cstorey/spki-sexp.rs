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

const OPEN_PAREN : u8 = '(' as u8;
const CLOSE_PAREN : u8 = ')' as u8;
const COLON : u8 = ':' as u8;
const ZERO : u8 = '0' as u8;
const NINE : u8 = '9' as u8;


#[derive(Debug)]
enum TokState {
    Start,
    ReadingLen(usize),
    ReadingAtomBody(usize, Vec<u8>),
}

#[derive(Debug)]
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

    pub fn take(&mut self) -> Result<Option<SexpToken>, TokenError> {
        // writeln!(::std::io::stderr(),"Pre-start: {:?}", self);
        while let TokState::Start = self.state {
            match self.inbuf.pop_front() {
              Some(c) if c == OPEN_PAREN => return Ok(Some(SexpToken::OpenParen))
            , Some(c) if c == CLOSE_PAREN => return Ok(Some(SexpToken::CloseParen))
            , Some(c) if c >= ZERO && c <= NINE => { self.state = TokState::ReadingLen((c - ZERO) as usize); }
            , None => return Ok(None)
            , Some(other) => return Err(TokenError::BadChar(other))
            };
        }
        // writeln!(::std::io::stderr(),"Post-start: {:?}", self);

        while let TokState::ReadingLen(n) = self.state {
            match self.inbuf.pop_front() {
              Some(c) if c >= ZERO && c <= NINE => { self.state = TokState::ReadingLen(n * 10 + (c - ZERO) as usize); }
            , Some(c) if c == COLON => { self.state = TokState::ReadingAtomBody(n, Vec::new()) }
            , Some(other) => return Err(TokenError::BadChar(other))
            , None => return Ok(None)
            };
        }
        // writeln!(::std::io::stderr(),"Post-len: {:?}", self);

        if let TokState::ReadingAtomBody(len, mut buf) = mem::replace(&mut self.state, TokState::Start) {
            // let remaining = ...
            let remaining = cmp::min(self.inbuf.len(), len - buf.len());
            buf.extend(self.inbuf.drain(..remaining));

            // writeln!(::std::io::stderr(),"Reading body: {}/{}", buf.len(), len);
            if buf.len() == len {
                // writeln!(::std::io::stderr(),"Body complete: {:?}", buf);
                return Ok(Some(SexpToken::Atom(buf)));
            } else {
                self.state = TokState::ReadingAtomBody(len, buf);
                // writeln!(::std::io::stderr(),"Body partial: {:?}", self);
            }
        }
        // writeln!(::std::io::stderr(),"Exit: {:?}", self);
        Ok(None)
    }
}

const BUF_SIZE : usize = 4096;
pub struct TokenisingIterator<I, E> where I : Iterator<Item=Result<u8, E>>, E: Into<TokenError> {
    iter: I,
    toks: Tokeniser,
}

pub fn tokenise<I:Iterator<Item=Result<u8, E>>, E: Into<TokenError>>(it : I) -> TokenisingIterator<I, E> {
  TokenisingIterator { iter: it, toks: Tokeniser::new() }
}

impl<'a, I: Iterator<Item=Result<u8, E>>, E: Into<TokenError>> Iterator for TokenisingIterator<I, E> {
    type Item = Result<SexpToken, TokenError>;
    fn next(&mut self) -> Option<Result<SexpToken, TokenError>> {
        loop {
            match self.toks.take() {
                Ok(Some(it)) => return Some(Ok(it)),
                Err(e) => return Some(Err(e)),
                Ok(None) => (),
            }

            match (&mut self.iter).take(BUF_SIZE).collect::<Result<Vec<u8>, E>>() {
                Ok(ref buf) if buf.len() == 0 => return None,
                Ok(buf) => self.toks.feed(&buf),
                Err(e) => return Some(Err(e.into())),
            }
        }
    }
}


