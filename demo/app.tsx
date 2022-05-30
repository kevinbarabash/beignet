import * as React from "react";

import * as m from "../pkg";

console.log(m);
// @ts-expect-error
window.m = m;

const styles = {
  app: {
    display: "flex",
    height: "100%",
    "flex-direction": "column",
  },
  textArea: {
    flexGrow: 1,
  },
};

export const App = () => {
  let [source, setSource] = React.useState(() => {
    return [
      "let add = (a, b) => a + b",
      "let sub = (a, b) => a - b",
      "let foo = (f, x) => f(x) + x",
      "",
      "let baz = if (true) {",
      "  let x = 5;",
      "  let y = 10;",
      "  x + y",
      "} else {",
      "  10",
      "}",
      "",
      "type Point = {x: number, y: number}",
      "let point: Point = {x: 5, y: 10}",
      "",
      "let add = async (a, b) => await a() + await b()",
      "",
      "let msg = \"world\"",
      "let elem = <div point={point} id=\"point\">Hello, {msg}</div>",
      "",
      "let rec fib = (n) => if (n == 0) {",
      "  0",
      "} else {",
      "  if (n == 1) {",
      "      1",
      "  } else {",
      "      fib(n - 1) + fib(n - 2)",
      "  }",
      "}",
    ].join("\n");
  });

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

  // TODO: use CSS Grid for this
  return (
    <div style={styles.app}>
      <h1>crochet demo</h1>
      <div style={{ width: "100%", height: "100%", display: "flex" }}>
        <textarea
          style={styles.textArea}
          value={source}
          onChange={updateSource}
        />
        <textarea style={styles.textArea} value={output.js} />
        <textarea style={styles.textArea} value={output.dts} />
      </div>
    </div>
  );
};
