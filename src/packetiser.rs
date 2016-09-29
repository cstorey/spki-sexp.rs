use std::collections::VecDeque;
use std::mem;
use serde::de;

use super::{SexpToken, Error};
use reader::Reader;
use tokeniser::Tokeniser;

#[derive(Debug)]
pub struct Packetiser {
    tok: Tokeniser,
    buf: VecDeque<SexpToken>,
    parens_open: usize,
}

impl Packetiser {
    pub fn new() -> Packetiser {
        Packetiser {
            tok: Tokeniser::new(),
            buf: VecDeque::new(),
            parens_open: 0,
        }
    }
    pub fn feed(&mut self, buf: &[u8]) {
        self.tok.feed(buf);
    }
    pub fn take<A: de::Deserialize>(&mut self) -> Result<Option<A>, super::Error> {
        // use std::io::Write;
        while let Some(tok) = try!(self.tok.take()) {
            match tok {
                SexpToken::OpenParen => self.parens_open += 1,
                SexpToken::CloseParen => self.parens_open -= 1,
                _ => (),
            };
            self.buf.push_back(tok);
            // writeln!(::std::io::stderr(),"state: {:?}", self);
            if self.parens_open == 0 {
                let packet = mem::replace(&mut self.buf, VecDeque::new());
                let mut de = Reader::of_tokens(packet.into_iter()
                    .map(|v| Ok(v).map_err(|e: Error| e)));
                let value = try!(de::Deserialize::deserialize(&mut de));
                try!(de.end());
                return Ok(Some(value));
            }
        }
        Ok(None)
    }
}
