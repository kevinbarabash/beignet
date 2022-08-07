import * as React from "react";
import * as examples from "./examples";
import { getPermalinkHref } from "./util";

const Dropdown = () => {
  const [open, setOpen] = React.useState(false);

  // TODO: close dropdown if the user clicks outside the dropdown
  const toggleOpen = () => {
    setOpen((value) => !value);
  };

  return (
    <button
      style={open ? { backgroundColor: "#EEE" } : {}}
      className="button-reset dropdown"
      onClick={toggleOpen}
    >
      Examples
      <span style={open ? {transform: "scaleY(-1)"} : {}} className="caret" />
      <div
        style={{ display: open ? "flex" : "none" }}
        className="dropdown-contents"
      >
        <a href={getPermalinkHref(examples.basics)}>Basics</a>
        <a href={getPermalinkHref(examples.asyncAwait)}>Async/Await</a>
        <a href={getPermalinkHref(examples.jsx)}>JSX</a>
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
