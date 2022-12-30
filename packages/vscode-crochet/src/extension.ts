import * as path from "path";
import * as vscode from "vscode";
import * as Parser from "web-tree-sitter";
// import Crochet from "tree-sitter-crochet";

// See https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers
const tokenTypes = ["variable", "keyword", "class", "parameter", "type"];
const tokenModifiers = ["declaration"];
const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

const getProvider = (parser: Parser): vscode.DocumentSemanticTokensProvider => {
  const paramQuery = `
  [
    "if"
    "else"
    "return"
    "try"
    "catch"
    "let"
    "async"
] @keyword

  (required_parameter pattern: (binding_identifier name: (identifier) @parameter))
  (required_parameter pattern: (rest_pattern (binding_identifier name: (identifier) @parameter)))
  (optional_parameter pattern: (binding_identifier name: (identifier) @parameter))

  (type_annotation (type_identifier) @type)
  (type_annotation (predefined_type) @type)
  (generic_type name: (type_identifier) @type)
  (type_arguments (type_identifier) @type)
  `;

  return {
    provideDocumentSemanticTokens(
      document: vscode.TextDocument
    ): vscode.ProviderResult<vscode.SemanticTokens> {
      let start, elapsed;

      start = Date.now();
      const sourceCode = document.getText();
      elapsed = Date.now() - start;
      console.log(`get text: ${elapsed}ms`);

      start = Date.now();
      const tree = parser.parse(sourceCode);
      elapsed = Date.now() - start;
      console.log(`parse: ${elapsed}ms`);

      const tokensBuilder = new vscode.SemanticTokensBuilder(legend);

      start = Date.now();
      let query = parser.getLanguage().query(paramQuery);
      let matches = query.matches(tree.rootNode);
      elapsed = Date.now() - start;
      console.log(`query: ${elapsed}ms`);

      start = Date.now();
      for (const match of matches) {
        for (const { name, node } of match.captures) {
          // console.log(`name = ${name},`, node);
          const startPosition = new vscode.Position(
            node.startPosition.row,
            node.startPosition.column
          );
          const endPosition = new vscode.Position(
            node.endPosition.row,
            node.endPosition.column
          );

          tokensBuilder.push(
            new vscode.Range(startPosition, endPosition),
            name,
            [] // modifiers
          );
        }
      }
      elapsed = Date.now() - start;
      console.log(`process matches: ${elapsed}ms`);

      start = Date.now();
      const result = tokensBuilder.build();
      elapsed = Date.now() - start;
      console.log(`build tokens: ${elapsed}ms`);

      return result;
    },
  };
};

export function activate(context: vscode.ExtensionContext) {
  console.log("hello, world");

  const selector = { language: "crochet" };

  const action = async () => {
    await Parser.init();

    const parser = new Parser();
    const filename = path.join(
      __dirname,
      "..",
      "..",
      "..",
      "crates",
      "tree_sitter_crochet",
      "tree-sitter-crochet.wasm"
    );

    const Crochet = await Parser.Language.load(filename);
    parser.setLanguage(Crochet);

    vscode.languages.registerDocumentSemanticTokensProvider(
      selector,
      getProvider(parser),
      legend
    );
  };

  action();
}
