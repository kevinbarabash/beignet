import { init, WASI } from "@wasmer/wasi";

const encodeString = (str: string) => {
  const encoded = new TextEncoder().encode(str);

  const len = encoded.byteLength + 1; // make room from the null terminating byte
  const buf = new Uint8Array(new ArrayBuffer(len));

  buf.set(encoded, 0);
  buf[len - 1] = 0; // set the last byte to be null

  return buf;
};

const decodeString = (memory: WebAssembly.Memory, offsetThenLength: number) => {
  const view = new DataView(memory.buffer);
  const offset = view.getUint32(offsetThenLength, true);
  const length = view.getUint32(offsetThenLength + 4, true);

  const stringView = new DataView(memory.buffer, offset, length);
  return new TextDecoder().decode(stringView);
};

const stringToCString = (
  memory: WebAssembly.Memory,
  allocate: (size: number) => number,
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

export const loadWasm = async (
  url: string,
  libCode: string
): Promise<Compiler> => {
  await init(null);

  const wasi = new WASI({});

  const response = await fetch(url);
  const buffer = await response.arrayBuffer();
  const wasm = await WebAssembly.compile(buffer);

  const imports = wasi.getImports(wasm);
  const instance = await WebAssembly.instantiate(wasm, {
    ...imports,
    my_custom_module: {
      _log: (value: any) => {
        console.log(decodeString(memory, value));
      },
    },
  });
  wasi.start(instance);

  const memory = instance.exports.memory as WebAssembly.Memory;
  const allocate = instance.exports.allocate as (size: number) => number;
  const deallocate = instance.exports.allocate as (
    ptr: number,
    size: number
  ) => void;

  const parse = instance.exports.parse as (inputPtr: number) => number;
  const input = stringToCString(memory, allocate, "const x = 1 * 2 + 5 - 4");
  const output = decodeString(memory, parse(input.ptr));
  console.log(`parse("const x = 1 * 2 + 5 - 4") = ${output}`);
  deallocate(input.ptr, input.size);

  const compile = instance.exports.compile as (
    inputPtr: number,
    libPtr: number
  ) => number;

  const lib = stringToCString(memory, allocate, libCode);

  return {
    compile: (code) => {
      const input = stringToCString(memory, allocate, code);
      const outPtr = compile(input.ptr, lib.ptr);

      const js = decodeString(memory, outPtr);
      const dts = decodeString(memory, outPtr + 8);
      const error = decodeString(memory, outPtr + 16);

      deallocate(input.ptr, input.size);

      if (error) {
        return { type: "err", error };
      }

      return { type: "ok", data: { js, dts } };
    },
  };
};
