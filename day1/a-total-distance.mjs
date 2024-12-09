import { readFileSync } from "fs";
import { join } from "path";

/**
 * Task:
 * 1. Read input, where columns = xs, ys
 * 2. Calculate difference of smallest x to smallest y, 2nd smallest x to 2nd smallest y etc.
 * 3. Sum and output differences
 */

// line shape: \d+\s+\d+\n ...
const input = readFileSync(join(import.meta.dirname, "./input.txt"), "utf8");
const lines = input.trim().split("\n");
const xs = lines.map((x) => parseInt(x.match(/^\d+/))).sort();
const ys = lines.map((x) => parseInt(x.match(/(?<=\d+\s+)\d+/g))).sort();

console.log(
  xs.map((x, i) => Math.abs(x - ys[i])).reduce((acc, cur) => acc + cur, 0),
);
