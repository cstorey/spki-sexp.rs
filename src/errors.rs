use std::io;
use std::{num, str, string};
use super::SexpToken;
use tokeniser;

error_chain! { 
    links {
        tokeniser::TokenError, tokeniser::ErrorKind, BadToken;
    }
    foreign_links {
        io::Error,IoError;
        num::ParseIntError, InvalidInt;
        num::ParseFloatError, InvalidFloat;
        str::ParseBoolError, InvalidBool;
        string::FromUtf8Error, InvalidUtf8;
    }

    errors {
        SyntaxError(s: String) {
            description("syntax error")
        }
        UnexpectedTokenError(t: SexpToken, expected: Vec<SexpToken>) {
            description("unexpected token")
        }
        BadChar(s: Vec<u8>) {
            description("invalid character")
        }
        EofError {
            description("end of file")
        }
        UnknownField(name: String) {
            description("unknown fiele")
        }
        MissingField(name: String) {
            description("missing field")
        }
    }

}
