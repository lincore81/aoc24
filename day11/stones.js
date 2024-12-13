/**
 * @param {number} n
 */
const splitDigits = (n) => {
  const nstr = String(n);
  const halfLen = Math.floor(nstr.length / 2);
  const result = [
    parseInt(nstr.slice(0, halfLen)) ?? 0,
    parseInt(nstr.slice(halfLen)) ?? 0,
  ];
  return result;
};

const applyRules = (n, cache) => {
  const cached = cache[n];
  if (cached) return cached;
  const uncached = !n ? 1 : evenDigits(n) ? splitDigits(n) : n * 2024;
  cache[n] = uncached;
  return uncached;
};

const evenDigits = (n) => Math.floor(1 + Math.log10(n)) % 2 === 0;

const iterate = (map) => {
  const result = new Map();
  const addCount = (n, amount) => result.set(n, (result.get(n) ?? 0) + amount);
  map.forEach((amount, n) => {
    if (!n) {
      addCount(1, amount);
    } else if (!evenDigits(n)) {
      addCount(n * 2024, amount);
    } else {
      const [p, q] = splitDigits(n);
      console.log(`${n} -> ${p}, ${q}`);
      addCount(p, amount);
      addCount(q, amount);
    }
  });
  return result;
};

const initial = new Map(
  [5688, 62084, 2, 3248809, 179, 79, 0, 172169].map((x) => [x, 1]),
);

let xs = initial;
for (let i = 0; i < 75; i++) {
  xs = iterate(xs);
}
const result = [...xs.values()].reduce((acc, x) => acc + x, 0);
console.log(result);
