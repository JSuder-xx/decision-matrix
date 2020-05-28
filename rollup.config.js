import resolve from "@rollup/plugin-node-resolve";
import { terser } from "rollup-plugin-terser";

export default {
  input: "./src/main.bs.js",
  output: [
    {
      file: "./dist/main.js",
      format: "iife",
      name: "starter",
      plugins: [terser()],
    },
  ],
  plugins: [resolve()],
};
