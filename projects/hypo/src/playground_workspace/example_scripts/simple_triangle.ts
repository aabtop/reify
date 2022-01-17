import * as h from 'hypo';

// For the Squares cbprng our state is 64-bit integers.
type u64 = [number, number];
type u32 = number;

type CbprngState = u64;

function mult(a: u64, b: u32 | u64): u64 {
  if (typeof b === "object") {

  } else {

  }
}

// "Squares" counter-based pseudo random number generator:
//   https://arxiv.org/pdf/2004.06278.pdf
function squares_cbprng(counter: u64, key: u64): u32 {
  const y = counter * key;
  const z = y + key;
  let x = y;

  x = x * x + y; x = (x >>> 32) | (x << 32); /* round 1 */
  x = x * x + z; x = (x >>> 32) | (x << 32); /* round 2 */
  x = x * x + y; x = (x >>> 32) | (x << 32); /* round 3 */
  return (x * x + z) >> 32; /* round 4 */
}

export function random_i32(counter: number): number {
  // Arbitrarily chosen.
  const key = 0x44f13206d9c2c417;
  return squares_cbprng(counter, key);
}

export function random_01(counter: number): number {
  return random_i32(counter) / 2147483648.0;
}

export function Main() {
  return h.Polygon({ path: [[random_01(11212), random_01(1)], [random_01(12), random_01(3)], [0, 0]] });
}

