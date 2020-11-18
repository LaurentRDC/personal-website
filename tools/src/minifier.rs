extern crate clap;
extern crate minifier;
extern crate minify;

use clap::{App, Arg};
use std::collections::HashSet;
use std::ffi::OsStr;
use minifier::css;
use minify::html;

// Recursively find all files in a directory `dir`
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

// Minify CSS and HTML in-place
// Any other type of file is left unchanged.
fn minify(path: &std::path::PathBuf) -> std::io::Result<()> {
    let content = std::fs::read_to_string(path)?;

    let minified = match path.extension().and_then(OsStr::to_str) {
        Some("css") => Some(css::minify(&content).unwrap()),
        Some("html") => Some(html::minify(&content)), 
        _other => None ,
    };
    
    minified.map(|m| std::fs::write(&path, m));
    Ok(())
}

fn main () {
    let matches = App::new("minifier")
        .version("v1.0")
        .author("Laurent P. René de Cotret")
        .about("Minify CSS and HTML files")
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
        
        let allowed_ext = vec![Some("css"), Some("html")];
        for path in files.iter() {
            if allowed_ext.contains(&path.extension().and_then(OsStr::to_str)) {
                minify(path);
                println!("Minifying {}", path.display());
            }
        }
}