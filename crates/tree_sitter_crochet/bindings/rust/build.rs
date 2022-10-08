use std::fs;
use std::process::{Command, Output};
use std::str;

fn main() {
    println!("cargo:rerun-if-changed=package.json");
    let yarn_install_output = Command::new("yarn")
        .args(["install", "--frozen-lockfile"])
        .output();
    match yarn_install_output {
        Ok(Output {
            status,
            stdout,
            stderr,
        }) => {
            let stdout = str::from_utf8(&stdout).unwrap();
            println!("stdout = {stdout}");

            if !status.success() {
                let stderr = str::from_utf8(&stderr).unwrap();
                println!("stderr = {stderr}");
                panic!("'yarn install' exited with non-zero status");
            }
        }
        Err(_) => {
            panic!("'yarn install' failed to run");
        }
    }

    println!("cargo:rerun-if-changed=grammar.js");
    let yarn_generate_output = Command::new("yarn").args(["generate"]).output();
    match yarn_generate_output {
        Ok(Output {
            status,
            stdout,
            stderr,
        }) => {
            let stdout = str::from_utf8(&stdout).unwrap();
            println!("stdout = {stdout}");

            if !status.success() {
                let stderr = str::from_utf8(&stderr).unwrap();
                println!("stderr = {stderr}");
                panic!("'yarn generate' exited with non-zero status");
            } else {
                let result = fs::copy("./real-binding.gyp", "./binding.gyp");
                match result {
                    Ok(count) => println!("{count} bytes copied"),
                    Err(_) => println!("copy of real-binding.gyp to binding.gyp failed"),
                }
            }
        }
        Err(_) => {
            panic!("'yarn generate' failed to run");
        }
    }

    let src_dir = std::path::Path::new("src");

    let mut c_config = cc::Build::new();
    c_config.include(&src_dir);
    c_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs");
    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);

    // If your language uses an external scanner written in C,
    // then include this block of code:

    let scanner_path = src_dir.join("scanner.c");
    c_config.file(&scanner_path);
    println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());

    c_config.compile("parser");
    println!("cargo:rerun-if-changed={}", parser_path.to_str().unwrap());

    // If your language uses an external scanner written in C++,
    // then include this block of code:

    /*
    let mut cpp_config = cc::Build::new();
    cpp_config.cpp(true);
    cpp_config.include(&src_dir);
    cpp_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable");
    let scanner_path = src_dir.join("scanner.cc");
    cpp_config.file(&scanner_path);
    cpp_config.compile("scanner");
    println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());
    */
}
