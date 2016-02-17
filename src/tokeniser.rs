use std::iter::Iterator;
use std::io;
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
      &SexpToken::Str(ref s) => {
        let bytes = s.clone().into_bytes();
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
        let s : String = match read_string(c, &mut self.iter) { Ok(s) => s, Err(e) => return Some(Err(e)), };
        Some(Ok(SexpToken::Str(s)))
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

fn read_string<'a, I>(first_char: u8, it : &mut I) -> Result<String, TokenError> where I : Iterator<Item=Result<u8, io::Error>> {
    use std::io::Write;
//  writeln!(::std::io::stderr(),"read_string: {:?}" , first_char as char);
  let mut len = first_char as usize  - ZERO as usize;
  while let Some(c) = it.next() {
    let c = try!(c);
    if c >= ZERO && c <= NINE {
      let digit = c - ZERO;
      len = len * 10 + digit as usize;
//      writeln!(::std::io::stderr(),"read_string! {:?}; len:{:?}" , c as char, len).unwrap();
    } else if c == ':' as u8 {
//      writeln!(::std::io::stderr(),"read_string! Colon!");
      break;
    } else {
//      writeln!(::std::io::stderr(),"read_string! wat: {:?}", c);
      return Err(TokenError::BadChar(c))
    }
  }

//  writeln!(::std::io::stderr(),"read_string! len: {:?}", len);
  let bytes : Vec<u8> = try!(it.take(len).collect());
  let s = String::from_utf8_lossy(bytes.as_slice());
//  writeln!(::std::io::stderr(),"read_string! {:?}", s);
  Ok(s.into_owned())
}



pub fn tokenise<I:Iterator<Item=Result<u8, io::Error>>>(it : I) -> TokenisingIterator<I> {
  TokenisingIterator { iter: it }
}
