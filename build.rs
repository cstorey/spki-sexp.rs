extern crate serde_codegen;
extern crate env_logger;
extern crate tempfile;
#[macro_use]
extern crate log;

use std::env;
use std::path::{Path,PathBuf};
use std::fs::{self, File};
use std::io::{Read, ErrorKind};
use tempfile::NamedTempFile;

fn process() {
    env_logger::init().unwrap();

    let out_dir = env::var_os("OUT_DIR").unwrap();

    for m in ["lib_types"].iter() {
        let src = PathBuf::from(format!("tests/{}.in.rs", m));
        let dst = Path::new(&out_dir).join(format!("{}.rs", m));
        let tmp_dst = NamedTempFile::new_in(&out_dir).expect("temp file");
        debug!("Src: {:?}; dst:{:?}, tmp:{:?}", src, dst, tmp_dst);

        serde_codegen::expand(Path::new(&src), tmp_dst.path()).expect("expanding");
        let should_update = match File::open(&dst) {
            Ok(f) => {
                let left = bytes_of(&f);
                let right = bytes_of(&tmp_dst);
                left.ne(right)
            },
            Err(ref e) if e.kind() == ErrorKind::NotFound => true,
            Err(e) => panic!("Unexpected error inspecting destination {:?}: {:?}", dst, e),
        };

        info!("Updating? {:?}", should_update);
        if should_update {
            fs::rename(tmp_dst.path(), dst).expect("rename");
        }
    }
}

fn bytes_of<'a, R: Read + 'a>(f: R) -> Box<Iterator<Item=u8> + 'a> {
    Box::new(f.bytes().map(|it| it.expect("error reading file")))
}

pub fn main() {
    process()
}
