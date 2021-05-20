const path = require("path");

module.exports = {
  mode: process.env.NODE_ENV === "production" ? "production" : "development",
  entry: "./src/index.ts",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "public"),
  },
  resolve: { extensions: [".ts", ".tsx", ".js"] },
  module: { rules: [{ test: /\.tsx?$/, loader: "ts-loader" }] },
};
