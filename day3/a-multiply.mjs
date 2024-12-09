import { readFileSync } from "fs";

/**
 * Task:
 * 1. Read the input file.
 * 2. Consider only the following syntax valid: mul(\d{1, 3},\d{1, 3})
 * 3. For each correct mul expression, multiply the two numbers in parens.
 * 4. Output the sum of all the results.
 */

const getInput = () => readFileSync("input.txt", "utf8");

const gatherParams = (data) => {
  const mulRegex = /mul\((\d{1,3}),(\d{1,3})\)/g;
  return [...data.matchAll(mulRegex)].map((match) =>
    match.slice(1, 3).map((x) => parseInt(x)),
  );
};

const execute = (params) =>
  params.map(([a, b]) => a * b).reduce((acc, x) => acc + x, 0);

console.log(execute(gatherParams(getInput())));
