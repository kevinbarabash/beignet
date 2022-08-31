import { init, WASI } from "@wasmer/wasi";

export const loadWasm = async (url: string): Promise<WebAssembly.Module> => {
  await init();

  const wasi = new WASI({});

  const response = await fetch(url);
  const buffer = await response.arrayBuffer();
  const wasm = await WebAssembly.compile(buffer);

  const imports = wasi.get_imports(wasm);
  const instance = await WebAssembly.instantiate(wasm, imports);
  wasi.start(instance);

  return instance;
};
