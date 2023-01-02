import * as path from "path";
import * as vscode from "vscode";
import * as Parser from "web-tree-sitter";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

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

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  console.log("hello, world");

  const selector = { scheme: "file", language: "crochet" };

  const command = path.join(
    __dirname,
    "..",
    "..",
    "..",
    "target",
    "debug",
    "crochet_lsp"
  );

  const serverOptions: ServerOptions = {
    run: { command, transport: TransportKind.stdio },
    debug: { command, transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [selector],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      // fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  client = new LanguageClient(
    "languageServerExample",
    "Language Server Example",
    serverOptions,
    clientOptions
  );

  // Output `eprintln!()` messages from the crochet_lsp in the `output` tab.
  client.outputChannel.show(true);

  // Start the client. This will also launch the server
  client.start();

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

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
