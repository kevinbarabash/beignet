import * as React from "react";

import libPath from "../node_modules/typescript/lib/lib.es5.d.ts";

import Dropdown from "./dropdown";
import { getPermalinkHref } from "./util";

const DEFAULT_CODE = `
// Welcome to the Escalier Playground!
let add = fn (a, b) => a + b
let add5 = fn (b) => add(5, b)
let sum = add5(10)
`;

const loadEscalier = async () => {
  const lib = await fetch(libPath).then((res) => res.text());
  const pkg = await import("../crates/escalier/pkg");
  return {
    compile: (code: string): CompilerResult => {
      try {
        const result = pkg.compile(code, lib);
        return { type: "ok", data: result };
      } catch (error: any) {
        return { type: "err", error };
      }
    },
  };
};

type CompilerResult =
  | {
      type: "ok";
      data: {
        js: string;
        dts: string;
        srcmap: string;
        ast: string;
      };
    }
  | {
      type: "err";
      error: string;
    };

export const App = () => {
  let [escalier, setEscalier] = React.useState<{
    compile: (input: string) => CompilerResult;
  } | null>(null);

  React.useEffect(() => {
    loadEscalier().then(setEscalier);
  }, []);

  let [source, setSource] = React.useState(() => {
    const url = new URL(window.location.href);
    const hash = url.hash.slice(1);
    return hash ? window.atob(hash) : DEFAULT_CODE.trim();
  });

  const updateSource = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    setSource(e.target.value);
  };

  let output = React.useMemo<CompilerResult>((): CompilerResult => {
    if (escalier) {
      return escalier.compile(source);
    } else {
      return { type: "ok", data: { js: "", dts: "", srcmap: "", ast: "" } };
    }
  }, [source, escalier]);

  let [outputTab, setOutputTab] = React.useState<
    "js" | "dts" | "srcmap" | "ast"
  >("js");

  React.useEffect(() => {
    const listener = (e: HashChangeEvent) => {
      const url = new URL(e.newURL);
      const hash = url.hash.slice(1);
      const source = window.atob(hash);
      setSource(source);
    };

    window.addEventListener("hashchange", listener);

    return () => {
      window.removeEventListener("hashchange", listener);
    };
  }, []);

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
      color: "var(--text-color)",
    },
  };

  const activeTabStyle = {
    borderBottom: "solid 2px var(--active-color)",
    paddingTop: 2,
  };

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          height: 32,
          backgroundColor: "var(--header)",
        }}
      >
        <div style={styles.header}>Escalier</div>
        <a
          className="header-link"
          href="https://github.com/escalier-lang/escalier"
          target="_blank"
        >
          Source {"\u2197"}
        </a>
        <a
          className="header-link"
          href="https://github.com/escalier-lang/escalier/issues"
          target="_blank"
        >
          Issues {"\u2197"}
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
            style={outputTab === "srcmap" ? activeTabStyle : {}}
            className="button-reset tab"
            onClick={() => setOutputTab("srcmap")}
          >
            .js.map
          </button>
          <button
            style={outputTab === "dts" ? activeTabStyle : {}}
            className="button-reset tab"
            onClick={() => setOutputTab("dts")}
          >
            .d.ts
          </button>
          <button
            style={outputTab === "ast" ? activeTabStyle : {}}
            className="button-reset tab"
            onClick={() => setOutputTab("ast")}
          >
            AST
          </button>
        </div>
        <textarea
          style={{ ...styles.editor, borderRight: "solid 1px var(--menu)" }}
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

  // return <h1>Hello, world</h1>;
};

// export const App = () => {
//   let [source, setSource] = React.useState(() => {
//     const url = new URL(window.location.href);
//     const hash = url.hash.slice(1);
//     return hash ? window.atob(hash) : DEFAULT_CODE.trim();
//   });
//   let [escalier, setEscalier] = React.useState<Compiler | null>(null);
//   let [outputTab, setOutputTab] = React.useState<
//     "js" | "dts" | "srcmap" | "ast"
//   >("js");

//   React.useEffect(() => {
//     fetch(libPath)
//       .then((res) => res.text())
//       .then((lib) => {
//         // TODO: fetch escalier code in parallel with fetch lib src
//         loadWasm(escalierWasmPath, lib).then((compiler) => {
//           setEscalier(compiler);
//         });
//       });
//   }, []);

//   React.useEffect(() => {
//     const listener = (e: HashChangeEvent) => {
//       const url = new URL(e.newURL);
//       const hash = url.hash.slice(1);
//       const source = window.atob(hash);
//       setSource(source);
//     };

//     window.addEventListener("hashchange", listener);

//     return () => {
//       window.removeEventListener("hashchange", listener);
//     };
//   }, []);

//   let output = React.useMemo<CompilerResult>(() => {
//     try {
//       if (escalier) {
//         return escalier.compile(source);
//       } else {
//         return { type: "ok", data: { js: "", dts: "", srcmap: "", ast: "" } };
//       }
//     } catch (e) {
//       return { type: "err", error: (e as Error).message };
//     }
//   }, [source, escalier]);

//   const updateSource = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
//     setSource(e.target.value);
//   };

//   const styles = {
//     grid: {
//       display: "grid",
//       gridTemplateColumns: "1fr 1fr",
//       gridTemplateRows: "min-content 1fr",
//       height: "100%",
//     },
//     label: {
//       fontFamily: "sans-serif",
//       fontWeight: "bold",
//     },
//     editor: {
//       fontFamily: "monospace",
//       fontSize: 14,
//       margin: 0,
//       border: "none",
//     },
//     header: {
//       fontFamily: "sans-serif",
//       margin: 0,
//       fontSize: 22,
//       lineHeight: "32px",
//       fontWeight: "bold",
//       marginRight: 24,
//       color: "var(--text-color)",
//     },
//   };

//   const activeTabStyle = {
//     borderBottom: "solid 2px var(--active-color)",
//     paddingTop: 2,
//   };

//   return (
//     <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
//       <div
//         style={{
//           display: "flex",
//           flexDirection: "row",
//           height: 32,
//           backgroundColor: "var(--header)",
//         }}
//       >
//         <div style={styles.header}>Escalier</div>
//         <a
//           className="header-link"
//           href="https://github.com/escalier-lang/escalier"
//           target="_blank"
//         >
//           Source {"\u2197"}
//         </a>
//         <a
//           className="header-link"
//           href="https://github.com/escalier-lang/escalier/issues"
//           target="_blank"
//         >
//           Issues {"\u2197"}
//         </a>
//       </div>
//       <div style={styles.grid}>
//         <div className="menu-bar">
//           <Dropdown />
//           <a className="menu-link" href={getPermalinkHref(source)}>
//             Permalink
//           </a>
//         </div>
//         <div className="menu-bar">
//           <button
//             style={outputTab === "js" ? activeTabStyle : {}}
//             className="button-reset tab"
//             onClick={() => setOutputTab("js")}
//           >
//             .js
//           </button>
//           <button
//             style={outputTab === "srcmap" ? activeTabStyle : {}}
//             className="button-reset tab"
//             onClick={() => setOutputTab("srcmap")}
//           >
//             .js.map
//           </button>
//           <button
//             style={outputTab === "dts" ? activeTabStyle : {}}
//             className="button-reset tab"
//             onClick={() => setOutputTab("dts")}
//           >
//             .d.ts
//           </button>
//           <button
//             style={outputTab === "ast" ? activeTabStyle : {}}
//             className="button-reset tab"
//             onClick={() => setOutputTab("ast")}
//           >
//             AST
//           </button>
//         </div>
//         <textarea
//           style={{ ...styles.editor, borderRight: "solid 1px var(--menu)" }}
//           value={source}
//           onChange={updateSource}
//         />
//         <textarea
//           style={styles.editor}
//           value={output.type === "ok" ? output.data[outputTab] : output.error}
//           readOnly={true}
//         />
//       </div>
//     </div>
//   );
// };
