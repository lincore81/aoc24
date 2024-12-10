import { readFileSync } from "fs";

/**
 * Task:
 * 1. Read the input file.
 * 2. Interpret the contents as a word search grid.
 * 3. Count these patterns:
 *  M.S | S.M | M.M | S.S
 *  .A. | .A. | .A. | .A.
 *  M.S | S.M | S.S | M.M
 * 4. Output the result.
 */

/**
 * @returns {string[][]} word search grid
 */
export const readInput = () =>
  readFileSync("./input.txt", "utf8")
    .trim()
    .split("\n")
    .map((line) => line.split(""));

/**
 * @param {string[][]} grid
 * @param {string} char
 * @yields {[number, number]} x, y
 */
export function* findChar(grid, char) {
  const height = grid.length;
  const width = height ? grid[0].length : 0;
  if (!width) return;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (grid[y][x] === char) {
        yield [x, y];
      }
    }
  }
}

// prettier-ignore
export const patterns = [
  [ 
    "M.S", 
    ".A.", 
    "M.S"
  ],
  [
    "S.M", 
    ".A.", 
    "S.M"
  ],
  [
    "M.M", 
    ".A.", 
    "S.S"
  ],
  [
    "S.S", 
    ".A.", 
    "M.M"
  ],
].map((pattern) => pattern.map((row) => row.split("")));

export const patternPivot = [1, 1];

/**
 * @param {string[][]} grid
 * @param {[number, number]} position on the grid
 * @param {string[][]} pattern
 * @returns {boolean}
 */
export const matchesPattern = (grid, [x, y], pattern) =>
  [
    [-1, -1],
    [1, -1],
    [1, 1],
    [-1, 1],
  ].every(([dx, dy]) => {
    const [gx, gy] = [x + dx, y + dy];
    const [px, py] = [dx + patternPivot[0], dy + patternPivot[1]];
    const g = grid[gy]?.[gx];
    const p = pattern[py]?.[px];
    console.log([dx, dy], [gx, gy], [px, py], g, p, g === p);
    return g === p;
  });

/**
 * @param {string[][]} grid
 * @param {[number, number]} [x, y]
 * @returns {number} count
 */
export const checkForCrossMas = (grid, [x, y]) => {
  return patterns
    .map((pattern) => matchesPattern(grid, [x, y], pattern))
    .filter(Boolean)
    .reduce((a, b) => a + b, 0);
};

export const solve = () => {
  const data = readInput();
  const as = [...findChar(data, "A")];
  const count = as
    .map((p) => checkForCrossMas(data, p))
    .reduce((a, b) => a + b, 0);
  console.log(count);
};

if (import.meta.url === `file://${process.argv[1]}`) solve();
