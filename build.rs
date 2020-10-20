use std::env;
use std::error::Error;
use std::fmt::Write;
use std::fs;
use std::path::Path;

pub fn main() -> Result<(), Box<dyn Error>> {
    gen("pass")?;
    gen("fail")
}

// Create a file named {tag}.rs with a integration
// test for each python file in tests/tag/
// See how the tests are run in src/integration.rs
pub fn gen(tag: &str) -> Result<(), Box<dyn Error>> {
    let mut content = String::new();
    for file in fs::read_dir(Path::new("tests").join(tag))? {
        let file = file?.path();
        if file.extension().map(|s| s != "py").unwrap_or(true) {
            continue;
        }
        let filename = file.file_stem().unwrap();
        writeln!(
            &mut content,
            "#[test] fn {}_{}() {{ {}({:?}) }}",
            tag,
            filename.to_string_lossy(),
            tag,
            file,
        )?;
    }
    let out = Path::new(&env::var("OUT_DIR")?)
        .join(tag)
        .with_extension("rs");
    fs::write(out, content)?;
    Ok(())
}
