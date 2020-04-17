let build (n, l) = tabulate n l
let exp__af = f64.exp
let log__af = f64.log
let sin__af = f64.sin
let cos__af = f64.cos
let tanh__af = const 1f64 -- FIXME
let sum = f64.sum
let to_float__ai = r64
let neg (x: f64) = -x
let lgamma__af = f64.lgamma
let digamma__af = const 1f64 -- FIXME
let fwd__lgamma__a__dff__b = const 1f64 -- FIXME
let rev__lgamma__a__dff__b = const 1f64 -- FIXME
let dot__a__dvfvf__b (xs, ys) = map2 (*) xs ys |> f64.sum
let mul__Mat__Vec__a__dvvfvf__b (xss, ys) = map (\xs -> dot__a__dvfvf__b (ys, xs)) xss
let constVec (n, x) = replicate n x
let size = length
let gt__a__dii__b (x: i32, y: i32) = x > y
let gt__a__dff__b (x: f64, y: f64) = x > y
let lt__a__dii__b (x: i32, y: i32) = x < y
let lt__a__dff__b (x: f64, y: f64) = x < y
let gte__a__dii__b (x: i32, y: i32) = x >= y
let gte__a__dff__b (x: f64, y: f64) = x >= y
let lte__a__dii__b (x: i32, y: i32) = x <= y
let lte__a__dff__b (x: f64, y: f64) = x <= y
let add__a__dii__b (x: i32, y: i32) = x + y
let add__a__dff__b (x: f64, y: f64) = x + y
let sub__a__dii__b (x: i32, y: i32) = x - y
let sub__a__dff__b (x: f64, y: f64) = x - y
let div__a__dii__b (x: i32, y: i32) = x / y
let div__a__dff__b (x: f64, y: f64) = x / y
let mul__a__dii__b (x: i32, y: i32) = x * y
let mul__a__dff__b (x: f64, y: f64) = x * y
let neg__ai (x: i32) = -x
let neg__af (x: f64) = -x
let or__a__dbb__b (x, y)  = x || y
let and__a__dbb__b (x, y) = x && y


let deltaVec 't (zero: t) (n: i32) i (v: t) : [n]t =
  tabulate n (\j -> if j == i then v else zero)

let delta 't (zero: t) (i: i32) (j: i32) (v: t) =
  if i == j then v else zero

let rev__mul__Mat__Vec__a__d__dvvfvf__bvf__b [r][c] (M_v: ([r][c]f64, [c]f64), dr: [r]f64): ([r][c]f64, [c]f64) =
  let (M, v) = M_v
  in (map (\x -> map (*x) v) dr,
      map (\col -> f64.sum (map2 (*) col dr)) (transpose M))

let upper_tri_to_linear (D: i32) (v: [D][D]f64) =
  tabulate_2d D D (\i j -> j >= i)
  |> flatten
  |> zip (flatten v)
  |> filter (.2)
  |> map (.1)

let sumbuild plus zero xs = reduce plus zero xs

-- ranhash functions from
--
-- https://mathoverflow.net/questions/104915/pseudo-random-algorithm-allowing-o1-computation-of-nth-element

let u__ranhash (v: u64): u64 =
  let v = v * 3935559000370003845
  let v = v + 2691343689449507681
  let v = v ^ (v >> 21)
  let v = v ^ (v << 37)
  let v = v ^ (v >> 4)
  let v = v * 4768777513237032717
  let v = v ^ (v << 20)
  let v = v ^ (v >> 41)
  let v = v ^ (v << 5)
  in v

let u__ranhashdoub__ai (v: i32): f64 =
  5.42101086242752217E-20 * f64.u64(u__ranhash (u64.i32 v))
