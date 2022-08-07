import * as React from "react";

import * as m from "../crates/crochet/pkg";

import * as examples from "./examples";
import githubMark from "./GitHub-Mark-32px.png";

console.log(m);
// @ts-expect-error
window.m = m;

export const App = () => {
  let [example, setExample] = React.useState<keyof typeof examples>("basics");
  let [source, setSource] = React.useState(() => examples[example].trim());

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
      gridTemplateRows: "min-content min-content min-content 1fr",
      height: "100%",
    },
    label: {
      fontFamily: "sans-serif",
      fontWeight: "bold",
    },
    editor: {
      fontFamily: "monospace",
      fontSize: 14,
    },
    header: {
      fontFamily: "sans-serif",
      gridColumnStart: 1,
      gridColumnEnd: 3,
      margin: 0,
    },
    links: {
      gridColumnStart: 1,
      gridColumnEnd: 3,
      alignSelf: "center",
      marginBottom: 12,
    },
  };

  const changeExample = (event: React.ChangeEvent<HTMLSelectElement>) => {
    let value = event.target.value;
    switch (value) {
      case "disjointUnionPatternMatching":
      case "basicPatternMatching":
      case "ifLetElse":
      case "ifLet":
      case "functionOverloading":
      case "asyncAwait":
      case "jsx":
      case "fibonacci":
      case "basics": {
        setExample(value);
        setSource(examples[value].trim());
      }
    }
  };

  const changeOutputTab = (event: React.ChangeEvent<HTMLSelectElement>) => {
    let value = event.target.value;
    switch (value) {
      case "js":
      case "dts": {
        setOutputTab(value);
      }
    }
  };

  const activeTabStyle = {
    borderBottom: "solid 2px blue",
  };

  return (
    <div style={styles.grid}>
      <h1 style={styles.header}>crochet 🧣</h1>
      <div style={styles.links}>
        <a href="https://github.com/crochet-lang/crochet" target="_blank">
          <img
            src={githubMark}
            width={16}
            height={16}
            style={{ opacity: 0.7 }}
          />
        </a>
      </div>
      <div>
        <span style={styles.label}>Example:</span>
        <select onChange={changeExample} value={example}>
          <option value="basics">Basics</option>
          <option value="asyncAwait">Async/Await</option>
          <option value="jsx">JSX</option>
          <option value="fibonacci">Fibonacci</option>
          <option value="functionOverloading">Function Overloading</option>
          <option value="ifLet">if let</option>
          <option value="ifLetElse">if let (with else)</option>
          <option value="basicPatternMatching">Basic Pattern Matching</option>
          <option value="disjointUnionPatternMatching">Disjoint Union Pattern Matching</option>
        </select>
      </div>
      <div>
        <button
          style={outputTab === "js" ? activeTabStyle : {}}
          className="tab"
          onClick={() => setOutputTab("js")}
        >
          .js
        </button>
        <button
          style={outputTab === "dts" ? activeTabStyle : {}}
          className="tab"
          onClick={() => setOutputTab("dts")}
        >
          .d.ts
        </button>
      </div>
      <textarea style={styles.editor} value={source} onChange={updateSource} />
      <textarea style={styles.editor} value={output[outputTab]} />
    </div>
  );
};
