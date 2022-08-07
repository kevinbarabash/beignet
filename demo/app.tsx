import * as React from "react";

import * as m from "../crates/crochet/pkg";

import Dropdown from "./dropdown";
import { getPermalinkHref } from "./util";

import githubMark from "./GitHub-Mark-32px.png";

const DEFAULT_CODE = `
// Welcome to the Crochet Playground!
let add = (a, b) => a + b
let add5 = add(5)
let sum = add5(10)
`;

export const App = () => {
  let [source, setSource] = React.useState(() => {
    const url = new URL(window.location.href);
    const code = url.searchParams.get("code");
    debugger;
    return code
      ? window.atob(window.decodeURIComponent(code))
      : DEFAULT_CODE.trim();
  });
  let [outputTab, setOutputTab] = React.useState<"js" | "dts">("js");

  let output = React.useMemo(() => {
    try {
      return m.compile(source);
    } catch (e) {
      console.log(e);
      return { js: "", dts: "" };
    }
  }, [source]);

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
          value={output[outputTab]}
          readOnly={true}
        />
      </div>
    </div>
  );
};
