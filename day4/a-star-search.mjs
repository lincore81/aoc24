import { readFileSync } from "fs";

/**
 * Task:
 * 1. Read the input file.
 * 2. Interpret the contents as a word search grid.
 * 3. Count the word "XMAS" in horizontal, vertical and diagonal directions
 *    as well as backwards.
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

export const directions = [
  [1, 0],
  [1, 1],
  [0, 1],
  [-1, 1],
  [-1, 0],
  [-1, -1],
  [0, -1],
  [1, -1],
];

/**
 * @param {string[][]} grid
 * @yields {[number, number]} x, y
 */
export function* findX(grid) {
  const height = grid.length;
  const width = height ? grid[0].length : 0;
  if (!width) return;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (grid[y][x] === "X") {
        yield [x, y];
      }
    }
  }
}

export const checkDirForMas = (grid, [x, y], [dx, dy]) => {
  const chars = Array.from({ length: 3 }, (_, i) => {
    const [nx, ny] = [x + dx * (i + 1), y + dy * (i + 1)];
    return grid[ny]?.[nx];
  })
    .filter(Boolean)
    .join("");
  console.log([x, y], [dx, dy], chars);
  return chars === "MAS";
};

export const countXmas = (grid) => {
  let p;
  let ps = [];
  let count = 0;
  const gen = findX(grid);
  while (true) {
    p = gen.next().value;
    if (!p) {
      return count;
    }
    ps.push(p);
    count += directions
      .map((d) => checkDirForMas(grid, p, d))
      .filter(Boolean).length;
  }
};

export const solve = () => {
  const data = readInput();
  console.log(countXmas(data));
};

if (import.meta.url === `file://${process.argv[1]}`) solve();
