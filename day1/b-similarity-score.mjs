import { readFileSync } from "fs";
import { join } from "path";
import { performance } from "perf_hooks";

/**
 * Task:
 * 1. Read input, where columns = xs, ys
 * 2. Count how often each item in xs occurs in ys
 * 3. Multiply x's by counts
 * 3. Sum products
 */
const start = performance.now();
// line shape: \d+\s+\d+\n ...
const input = readFileSync(join(import.meta.dirname, "./input.txt"), "utf8");
const lines = input.trim().split("\n");
const xs = lines.map((x) => parseInt(x.match(/^\d+/))).sort();
const ys = lines.map((x) => parseInt(x.match(/(?<=\d+\s+)\d+/g))).sort();

// Create a lookup table to avoid O(n^2)
const counts = new Map();
for (let y of ys) {
  if (counts[y]) {
    counts[y]++;
  } else {
    counts[y] = 1;
  }
}

const solution = xs
  .map((x) => x * (counts[x] ?? 0))
  .reduce((acc, x) => acc + x, 0);

console.log(solution, `\n${(performance.now() - start).toFixed(2)}ms`);
