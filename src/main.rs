use rlang::{smart_print,Rlang};
use std::env;
use std::fs;



fn main() {
    let mut args = env::args();
    args.next();
    if args.len() == 0 {
        println!("Usage: rlang <filename>");
        return;
    }
    let filename = args.next().unwrap();
    println!("{}",env::current_dir().unwrap().display());
    let file = fs::read_to_string(filename).unwrap();
    let mut rlang = Rlang::new(file);
    rlang.parse();
    println!("{:#?}",rlang.program);
    rlang.run();
}
