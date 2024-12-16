import { readFileSync } from "fs";
import readline from "readline/promises";
import { stdin } from "process";

const getInput = () => {
  const content = readFileSync("./input.txt", "utf-8")
    .split("\n")
    .slice(0, 500)
    .map((line) => {
      const { px, py, vx, vy } =
        line.match(/p=(?<px>-?\d+),(?<py>-?\d+) v=(?<vx>-?\d+),(?<vy>-?\d+)/)
          ?.groups ?? {};
      return {
        px: parseInt(px),
        py: parseInt(py),
        vx: parseInt(vx),
        vy: parseInt(vy),
      };
    });
  return content;
};

const WIDTH = 101;
const HEIGHT = 103;
const ITERATIONS = 100;

const calcEndpoint = (n, { px, py, vx, vy }) => {
  let [x, y] = [(px + vx * n) % WIDTH, (py + vy * n) % HEIGHT];
  if (x < 0) x = WIDTH + x;
  if (y < 0) y = HEIGHT + y;
  return [x, y];
};

const quadrant = ([x, y]) => {
  const HW = (WIDTH - 1) / 2;
  const HH = (HEIGHT - 1) / 2;
  if (x === HW || y === HH) return false;
  const qx = Math.min(1, Math.floor(x / HW));
  const qy = Math.min(1, Math.floor(y / HH));
  const q = Math.abs(2 * qy + qx);
  // console.log([x, y], [qx, qy], q);
  return q;
};

const group = (xs) => {
  const groups = {};
  xs.forEach((n) => {
    if (n === false) return;
    if (groups[n]) {
      groups[n]++;
    } else {
      groups[n] = 1;
    }
  });
  return groups;
};

const showBoard = (bots, rotate) => {
  const [w, h] = rotate ? [HEIGHT, WIDTH] : [WIDTH, HEIGHT];
  const board = Array.from({ length: h }, () => Array.from({ length: w }));
  bots.forEach((b) => {
    const [px, py] = rotate ? [b.py, b.px] : [b.px, b.py];
    if (!(0 <= px < w && 0 <= py < h))
      throw new Error(`out of bounds: ${[px, py]}`);
    const n = board[py][px];
    board[py][px] = n ? n + 1 : 1;
  });
  return board.map((row) => row.map((x) => (x ? x : " ")).join(" ")).join("\n");
};

const data = getInput();

const step = (bot) => {
  const [px, py] = calcEndpoint(1, bot);
  return { ...bot, px, py };
};

/**
 * Group robots by quadrant after 100s and multiply the results into one number.
 */
const part1 = (data) => {
  const solutions = data.map((d) => quadrant(calcEndpoint(ITERATIONS, d)));
  const grouped = group(solutions);
  console.log(grouped);
  return Object.values(grouped).reduce((acc, x) => acc * x, 1);
};

/**
 * Step through the simulation until the robots form a christmas tree.
 * Return the number of turns (aka seconds) it takes.
 *
 * It turns out that the first turn where all robots occupy unique positions is
 * the turn when a christmas tree is shown.
 */
const part2 = (data) => {
  let state = data;
  const uniqueCoords = (state) => {
    const kv = {};
    return !state.find(({ px, py }) => {
      const k = `${px},${py}`;
      if (kv[k]) {
        return true;
      }
      kv[k] = true;
      return false;
    });
  };
  let n = 1;
  while (true) {
    state = state.map(step);
    if (uniqueCoords(state)) {
      console.log(showBoard(state));
      console.log(`#${n}`);
      return;
    }
    console.log(n);
    n++;
  }
};

const part2Manual = async (data) => {
  let state = data;
  const rl = readline.createInterface(stdin);
  let inp;
  let n = 1;
  do {
    state = state.map(step);
    console.log(showBoard(state, true), `\n#${n++}`);
    inp = await rl.question("?");
  } while (!inp);
};

part2(data);
