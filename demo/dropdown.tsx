import * as React from "react";

import * as examples from "./examples";
import { getPermalinkHref } from "./util";

const Dropdown = () => {
  const [open, setOpen] = React.useState(false);
  const buttonRef = React.useRef<HTMLButtonElement | null>(null);

  // TODO: close dropdown if the user clicks outside the dropdown
  const toggleOpen = () => {
    setOpen((value) => !value);
  };

  React.useEffect(() => {
    const listener = (e: MouseEvent) => {
      let { target } = e;

      if (
        target instanceof Element &&
        buttonRef.current &&
        buttonRef.current.contains(target)
      ) {
        return;
      }

      setOpen(false);
    };

    document.addEventListener("click", listener);

    return () => {
      document.removeEventListener("click", listener);
    };
  }, []);

  return (
    <button
      style={open ? { backgroundColor: "var(--header)" } : {}}
      className="button-reset dropdown"
      onClick={toggleOpen}
      ref={(node) => (buttonRef.current = node)}
    >
      Examples {open ? "\u25B2" : "\u25BC"}
      <div
        style={{ display: open ? "flex" : "none" }}
        className="dropdown-contents"
      >
        <a href={getPermalinkHref(examples.basics)}>Basics</a>
        <a href={getPermalinkHref(examples.asyncAwait)}>Async/Await</a>
        <a href={getPermalinkHref(examples.jsxReact)}>React + JSX</a>
        <a href={getPermalinkHref(examples.fibonacci)}>Fibonacci</a>
        <a href={getPermalinkHref(examples.functionOverloading)}>
          Function Overloading
        </a>
        <a href={getPermalinkHref(examples.ifLetElse)}>If Let (with Else)</a>
        <a href={getPermalinkHref(examples.basicPatternMatching)}>
          Basic Pattern Matching
        </a>
        <a href={getPermalinkHref(examples.disjointUnionPatternMatching)}>
          Disjoint Union Pattern Matching
        </a>
      </div>
    </button>
  );
};

export default Dropdown;
