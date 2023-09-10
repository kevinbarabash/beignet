const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

module.exports = {
  entry: "./demo/index.tsx",
  output: {
    path: path.resolve(__dirname, "build"),
    filename: "index.js",
    clean: true,
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "index.html",
    }),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "./crates/escalier"),
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
  experiments: {
    asyncWebAssembly: true,
  },
  resolve: {
    extensions: [".tsx", ".ts", ".js"],
    fallback: {
      buffer: require.resolve("buffer/"),
    },
  },
  mode: "production",
};
