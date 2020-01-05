import resolve from '@rollup/plugin-node-resolve'
import {terser} from 'rollup-plugin-terser';

export default {
    input: './src/Main.bs.js',
    output: [
        {
            file: './release/main.js',
            format: 'iife',
            name: 'starter',
            plugins: [terser()]
        }
    ],
    plugins: [resolve()]
}