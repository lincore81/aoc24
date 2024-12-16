import { readFileSync } from "fs";

/*
 Task:
 - Read input and split content into two parts at the blank line
 - The first part is a list of order rules in the shape
   "a|b", where a and b are integers. The rule is that a must occur before b.
 - The second part is a line-separated list of comma-separated numeric sequences.
 - For each sequence, if all order rules apply, the sequence is valid.
 - The middle value of each valid sequence should be added up and printed.
   All sequences seem to be of odd length.

 Challenges:
 - Seems trivial to implement, but avoiding exponential complexity requires some thought.
*/

export const readInput = () => {
  const content = readFileSync("./input.txt", "utf8");
  const [part1, part2] = content.split("\n\n");
  const rulesArray = part1.split("\n").map((row) => row.split("|"));
  const rules = {};
  rulesArray.forEach(([a, b]) =>
    rules?.[a] ? (rules[a][b] = true) : (rules[a] = { [b]: true }),
  );

  const sequences = part2.split("\n").map((row) => row.split(","));
  return { rules, sequences };
};

export const mkSeqMap = (seq) => {
  const result = {};
  seq.forEach((x, i) => {
    if (result[x]) throw "Sequence is not unique.";
    result[x] = i;
  });
  return result;
};

export const matchesRule = (seqMap, a, rule) => {
  console.log("???");
  const aIdx = seqMap[a];
  if (!aIdx) return true;
  for (let b of Object.keys(rule)) {
    const bIdx = seqMap[b];
    if (!bIdx) continue;
    if (bIdx <= aIdx) return false;
  }
  return true;
};

export const validateSeq = (seq, rules) => {
  const seqMap = mkSeqMap(seq);
  console.log(seq);
  for (let x of seq) {
    const rule = rules?.[x];
    if (!rule) continue;
    const match = matchesRule(seqMap, rule);
    console.log(rule, match);
    if (!match) return false;
  }
  return true;
};

export const { rules, sequences } = readInput();

console.log(sequences.map((seq) => validateSeq(seq, rules)));
console.log(rules);
