/**
 * index.mjs
 *
 * Test program to experiment with wasm/wasi.
 *
 * To run:
 * - cd crates/crochet && cargo build --target wasm32-wasi
 * - node --experimental-wasi-unstable-preview1 index.mjs
 */
import fs from "fs";
import { WASI } from "wasi";

const wasi = new WASI();
const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

const encodeString = (str) => {
  const encoded = new TextEncoder().encode(str);

  const len = encoded.byteLength + 1; // make room from the null terminating byte
  const buf = new Uint8Array(new ArrayBuffer(len));

  buf.set(encoded, 0);
  buf[len - 1] = 0; // set the last byte to be null

  return buf;
};

const decodeString = (memory, offsetAndLength) => {
  const view = new DataView(memory.buffer);
  const offset = view.getUint32(offsetAndLength, true);
  const length = view.getUint32(offsetAndLength + 4, true);

  const stringView = new DataView(memory.buffer, offset, length);
  return new TextDecoder().decode(stringView);
};

const stringToCString = (memory, allocate, str) => {
  const buf = encodeString(str);
  const ptr = allocate(buf.length);
  const mem = new Uint8Array(memory.buffer);
  mem.set(buf, ptr);
  return ptr;
};

const main = async () => {
  const wasm = await WebAssembly.compile(
    fs.readFileSync("./target/wasm32-wasi/release/crochet.wasm")
  );
  const instance = await WebAssembly.instantiate(wasm, importObject);
  // WASI requires use to call start()
  wasi.start(instance);

  const { parse, print_hello, echo, allocate, compile, memory } =
    instance.exports;

  // Return a string from rust using the C ABI
  print_hello();

  // Parse a string using a tree-sitter parser
  const codeStr = stringToCString(memory, allocate, "x := 1 * 2 + 5 - 4");
  const offsetAndLength = parse(codeStr);
  const str = decodeString(memory, offsetAndLength);
  console.log("str = " + str);

  // Echo a string but encode it first
  const ptr = stringToCString(memory, allocate, "Hello, world!");
  echo(ptr);
  // TODO: use an object to store the point and length so we can dealloc later
  // deallocate(ptr, buf.length);

  // Compile and typecheck some Crochet code
  let inputPtr = stringToCString(memory, allocate, "let add = (a, b) => a + b");
  let libPtr = stringToCString(memory, allocate, "");
  let outPtr = compile(inputPtr, libPtr);

  let jsStr = decodeString(memory, outPtr);
  let dtsStr = decodeString(memory, outPtr + 8);

  console.log("jsStr = " + jsStr);
  console.log("dtsStr = " + dtsStr);
};

main();
