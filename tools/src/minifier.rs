extern crate clap;
extern crate minifier;

use clap::{App, Arg};
use std::collections::HashSet;

// Recursively find all HTML files in a directory `dir`
fn visit_dir_recursively(dir: &std::path::PathBuf) -> HashSet<std::path::PathBuf> {
    let mut files = HashSet::new();

    for entry in std::fs::read_dir(dir).unwrap().filter_map(Result::ok) {
        if entry.metadata().is_err() {
            continue;
        }

        if entry.metadata().unwrap().is_dir() {
            files.union(&visit_dir_recursively(&entry.path()));
        } else {
            files.insert(entry.path());
        }
    }
    return files;
}

// Minify CSS in-place
fn minify(path: &std::path::PathBuf) -> std::io::Result<()> {
    let css = std::fs::read_to_string(path)?;
    let minified = minifier::css::minify(&css);
    std::fs::write(&path, minified.unwrap())?;
    Ok(())
}

fn main () {
    let matches = App::new("minifier")
        .version("v1.0")
        .author("Laurent P. René de Cotret")
        .about("Minify CSS files")
        .arg(
            Arg::with_name("INPUT")
                .help("File / directory to check")
                .required(true)
                .index(1),
        )
        .get_matches();

        let cwd = std::env::current_dir().expect("Current working directory is invalid");
        let path = std::path::PathBuf::from(matches.value_of("INPUT").unwrap());
        let fullpath = if path.is_relative() {
            cwd.join(path)
        } else {
            path
        };

        let files = if fullpath.metadata().unwrap().is_dir() {
            visit_dir_recursively(&fullpath)
        } else {
            let mut f = HashSet::new();
            f.insert(fullpath);
            f
        };
        
        for path in files.iter().filter(|p| p.extension() == Some(std::ffi::OsStr::new("css"))) {
            minify(path);
            println!("Minifying {}...", path);
        }
}