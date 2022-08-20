const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

module.exports = {
  entry: "./demo/index.tsx",
  output: {
    path: path.resolve(__dirname, "build"),
    filename: "index.js",
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "index.html",
    }),
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "./crates/crochet"),
      watchDirectories: [
        path.resolve(__dirname, "./crates/crochet_ast/src"),
        path.resolve(__dirname, "./crates/crochet_codegen/src"),
        path.resolve(__dirname, "./crates/crochet_dst/src"),
        path.resolve(__dirname, "./crates/crochet_infer/src"),
        path.resolve(__dirname, "./crates/crochet_parser/src"),
      ],
    }),
  ],
  module: {
    rules: [
      {
        test: /\.d\.ts$/i,
        type: "asset/resource",
      },
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
      },
    ],
  },
  resolve: {
    extensions: [".tsx", ".ts", ".js"],
  },
  mode: "production", // wasm-pack can't handle source map section 0x20
  experiments: {
    asyncWebAssembly: true,
  },
};
