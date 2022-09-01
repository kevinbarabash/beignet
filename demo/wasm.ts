import { init, WASI } from "@wasmer/wasi";

const encodeString = (str: string) => {
  const encoded = new TextEncoder().encode(str);

  const len = encoded.byteLength + 1; // make room from the null terminating byte
  const buf = new Uint8Array(new ArrayBuffer(len));

  buf.set(encoded, 0);
  buf[len - 1] = 0; // set the last byte to be null

  return buf;
};

const decodeString = (memory: WebAssembly.Memory, offsetAndLength: number) => {
  const view = new DataView(memory.buffer);
  const offset = view.getUint32(offsetAndLength, true);
  const length = view.getUint32(offsetAndLength + 4, true);

  const stringView = new DataView(memory.buffer, offset, length);
  return new TextDecoder().decode(stringView);
};

const stringToCString = (
  memory: WebAssembly.Memory,
  allocate: Function,
  str: string
): { ptr: any; size: number } => {
  const buf = encodeString(str);
  const ptr = allocate(buf.length);
  const mem = new Uint8Array(memory.buffer);
  mem.set(buf, ptr);
  return { ptr, size: buf.length };
};

export type Result<TData, TError> =
  | { type: "ok"; data: TData }
  | { type: "err"; error: TError };

export type CompilerResult = Result<{ js: string; dts: string }, string>;

export interface Compiler {
  compile(input: string): CompilerResult;
}

export const loadWasm = async (url: string, lib: string): Promise<Compiler> => {
  await init(null);

  const wasi = new WASI({});

  const response = await fetch(url);
  const buffer = await response.arrayBuffer();
  const wasm = await WebAssembly.compile(buffer);

  const imports = wasi.getImports(wasm);
  const instance = await WebAssembly.instantiate(wasm, imports);
  wasi.start(instance);

  const { parse, print_hello, echo } = instance.exports;

  const memory = instance.exports.memory as WebAssembly.Memory;
  const allocate = instance.exports.allocate as (size: number) => any;
  const deallocate = instance.exports.allocate as (
    ptr: any,
    size: number
  ) => void;
  const compile = instance.exports.compile as Function;

  let libPtr = stringToCString(memory, allocate, lib);

  return {
    compile: (code) => {
      // TODO: deallocate inputPtr before returning
      let input = stringToCString(memory, allocate, code);
      let outPtr = compile(input.ptr, libPtr);

      let js = decodeString(memory, outPtr);
      let dts = decodeString(memory, outPtr + 8);
      let error = decodeString(memory, outPtr + 16);

      deallocate(input.ptr, input.size);

      if (error) {
        return { type: "err", error };
      }

      return { type: "ok", data: { js, dts } };
    },
  };
};
