import * as React from "react";

import libPath from "../node_modules/typescript/lib/lib.es5.d.ts";
import crochetWasmPath from "../target/wasm32-wasi/release/crochet.wasm";

import Dropdown from "./dropdown";
import { getPermalinkHref } from "./util";
import { loadWasm, Compiler, CompilerResult } from "./wasm";

const DEFAULT_CODE = `
// Welcome to the Crochet Playground!
let add = (a, b) => a + b;
let add5 = (b) => add(5, b);
let sum = add5(10);
`;

export const App = () => {
  let [source, setSource] = React.useState(() => {
    const url = new URL(window.location.href);
    const code = url.searchParams.get("code");
    return code
      ? window.atob(window.decodeURIComponent(code))
      : DEFAULT_CODE.trim();
  });
  let [crochet, setCrochet] = React.useState<Compiler | null>(null);
  let [outputTab, setOutputTab] = React.useState<"js" | "dts">("js");

  React.useEffect(() => {
    fetch(libPath)
      .then((res) => res.text())
      .then((lib) => {
        // TODO: fetch crochet code in parallel with fetch lib src
        loadWasm(crochetWasmPath, lib).then((compiler) => {
          setCrochet(compiler);
        });
      });
  }, []);

  let output = React.useMemo<CompilerResult>(() => {
    try {
      if (crochet) {
        return crochet.compile(source);
      } else {
        return { type: "ok", data: { js: "", dts: "" } };
      }
    } catch (e) {
      return { type: "err", error: (e as Error).message };
    }
  }, [source, crochet]);

  const updateSource = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    setSource(e.target.value);
  };

  const styles = {
    grid: {
      display: "grid",
      gridTemplateColumns: "1fr 1fr",
      gridTemplateRows: "min-content 1fr",
      height: "100%",
    },
    label: {
      fontFamily: "sans-serif",
      fontWeight: "bold",
    },
    editor: {
      fontFamily: "monospace",
      fontSize: 14,
      margin: 0,
      border: "none",
    },
    header: {
      fontFamily: "sans-serif",
      margin: 0,
      fontSize: 22,
      lineHeight: "32px",
      fontWeight: "bold",
      marginRight: 24,
    },
  };

  const activeTabStyle = {
    borderBottom: "solid 2px blue",
    paddingTop: 2,
  };

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          height: 32,
          backgroundColor: "#EEE",
        }}
      >
        <div style={styles.header}>🧣Crochet</div>
        <a
          className="header-link"
          href="https://github.com/crochet-lang/crochet"
          target="_blank"
        >
          Source
        </a>
        <a
          className="header-link"
          href="https://github.com/crochet-lang/crochet/issues"
          target="_blank"
        >
          Issues
        </a>
      </div>
      <div style={styles.grid}>
        <div className="menu-bar">
          <Dropdown />
          <a className="menu-link" href={getPermalinkHref(source)}>
            Permalink
          </a>
        </div>
        <div className="menu-bar">
          <button
            style={outputTab === "js" ? activeTabStyle : {}}
            className="button-reset tab"
            onClick={() => setOutputTab("js")}
          >
            .js
          </button>
          <button
            style={outputTab === "dts" ? activeTabStyle : {}}
            className="button-reset tab"
            onClick={() => setOutputTab("dts")}
          >
            .d.ts
          </button>
        </div>
        <textarea
          style={{ ...styles.editor, borderRight: "solid 1px #CCC" }}
          value={source}
          onChange={updateSource}
        />
        <textarea
          style={styles.editor}
          value={output.type === "ok" ? output.data[outputTab] : output.error}
          readOnly={true}
        />
      </div>
    </div>
  );
};
