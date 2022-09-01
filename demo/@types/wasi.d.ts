declare module "@wasmer/wasi" {
  /**
   */
  export class JSVirtualFile {
    static __wrap(ptr: any): any;
    __destroy_into_raw(): number;
    ptr: number;
    free(): void;
    /**
     * @returns {BigInt}
     */
    lastAccessed(): BigInt;
    /**
     * @returns {BigInt}
     */
    lastModified(): BigInt;
    /**
     * @returns {BigInt}
     */
    createdTime(): BigInt;
    /**
     * @returns {BigInt}
     */
    size(): BigInt;
    /**
     * @param {BigInt} new_size
     */
    setLength(new_size: BigInt): void;
    /**
     * @returns {Uint8Array}
     */
    read(): Uint8Array;
    /**
     * @returns {string}
     */
    readString(): string;
    /**
     * @param {Uint8Array} buf
     * @returns {number}
     */
    write(buf: Uint8Array): number;
    /**
     * @param {string} buf
     * @returns {number}
     */
    writeString(buf: string): number;
    /**
     */
    flush(): void;
    /**
     * @param {number} position
     * @returns {number}
     */
    seek(position: number): number;
  }
  /**
   */
  export class MemFS {
    static __wrap(ptr: any): any;
    __destroy_into_raw(): number;
    ptr: number;
    free(): void;
    /**
     * @param {string} path
     * @returns {Array<any>}
     */
    readDir(path: string): Array<any>;
    /**
     * @param {string} path
     */
    createDir(path: string): void;
    /**
     * @param {string} path
     */
    removeDir(path: string): void;
    /**
     * @param {string} path
     */
    removeFile(path: string): void;
    /**
     * @param {string} path
     * @param {string} to
     */
    rename(path: string, to: string): void;
    /**
     * @param {string} path
     * @returns {object}
     */
    metadata(path: string): object;
    /**
     * @param {string} path
     * @param {any} options
     * @returns {JSVirtualFile}
     */
    open(path: string, options: any): JSVirtualFile;
  }
  /**
   */
  export class WASI {
    static __wrap(ptr: any): any;
    /**
     * @param {any} config
     */
    constructor(config: any);
    __destroy_into_raw(): number;
    ptr: number;
    free(): void;
    /**
     * @returns {MemFS}
     */
    get fs(): MemFS;
    /**
     * @param {WebAssembly.Module} module
     * @returns {object}
     */
    getImports(module: WebAssembly.Module): WebAssembly.Imports;
    /**
     * @param {any} module
     * @param {object | undefined} imports
     * @returns {WebAssembly.Instance}
     */
    instantiate(module: any, imports: object | undefined): WebAssembly.Instance;
    /**
     * Start the WASI Instance, it returns the status code when calling the start
     * function
     * @param {WebAssembly.Instance} instance
     * @returns {number}
     */
    start(instance: WebAssembly.Instance): number;
    /**
     * Get the stdout buffer
     * Note: this method flushes the stdout
     * @returns {Uint8Array}
     */
    getStdoutBuffer(): Uint8Array;
    /**
     * Get the stdout data as a string
     * Note: this method flushes the stdout
     * @returns {string}
     */
    getStdoutString(): string;
    /**
     * Get the stderr buffer
     * Note: this method flushes the stderr
     * @returns {Uint8Array}
     */
    getStderrBuffer(): Uint8Array;
    /**
     * Get the stderr data as a string
     * Note: this method flushes the stderr
     * @returns {string}
     */
    getStderrString(): string;
    /**
     * Set the stdin buffer
     * @param {Uint8Array} buf
     */
    setStdinBuffer(buf: Uint8Array): void;
    /**
     * Set the stdin data as a string
     * @param {string} input
     */
    setStdinString(input: string): void;
  }
  /**
   * A struct representing an aborted instruction execution, with a message
   * indicating the cause.
   */
  export class WasmerRuntimeError {
    static __wrap(ptr: any): any;
    __destroy_into_raw(): number;
    ptr: number;
    free(): void;
  }
  export default init;
  export function init(input: any): Promise<WebAssembly.Exports>;
}
