import { readFileSync } from "fs";

/**
 * Task:
 * 1. Read the input file.
 * 2. NEW: Consider the following expressions valid:
 *  a: mul(\d{1, 3},\d{1, 3})
 *  b: do()
 *  c: don't()
 * 3. For each correct mul expression, multiply the two numbers in parens.
 * 4. When encountering a don't() expression, ignore subsequent mul expressions.
 * 5. When encountering a do() expression, consider subsequent mul expressions.
 * 6. Initially consider mul expressions.
 * 7. Output the sum of all the results.
 */

const getInput = () => readFileSync("input.txt", "utf8");

const gatherValidExpressions = (data) => {
  const mulRegex = /(?:mul\((\d{1,3}),(\d{1,3})\)|(don't\(\))|(do\(\)))/g;
  return [...data.matchAll(mulRegex)].map(([expr, p1, p2]) => {
    if (expr.startsWith("mul")) {
      const [n1, n2] = [parseInt(p1), parseInt(p2)];
      return { expr: "mul", n1, n2 };
    } else {
      return { expr: expr.replace(/[()]/g, "") };
    }
  });
};

const execute = (params) =>
  params.reduce(
    ({ sum, doMul }, { expr, n1, n2 }) =>
      doMul && expr === "mul"
        ? { sum: sum + n1 * n2, doMul }
        : expr === "do"
          ? { sum, doMul: true }
          : expr === "don't"
            ? { sum, doMul: false }
            : { sum, doMul },
    { sum: 0, doMul: true },
  );

console.log(execute(gatherValidExpressions(getInput())).sum);
