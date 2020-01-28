let build = tabulate
let exp = f64.exp
let log = f64.log
let sin = f64.sin
let cos = f64.cos
let sum = f64.sum
let to_float = r64
let neg (x: f64) = -x
let lgamma = f64.lgamma
let digamma = const 1f64 -- FIXME
let fwd__lgamma = const (const 1f64) -- FIXME
let rev__lgamma = const (const 1f64) -- FIXME
let dotv xs ys = map2 (*) xs ys |> f64.sum
let mul__Mat__Vec xss ys = map (dotv ys) xss
let constVec = replicate
let size = length
let gt__aii (x: i32) (y: i32) = x > y
let gt__aff (x: f64) (y: f64) = x > y
let lt__aii (x: i32) (y: i32) = x < y
let lt__aff (x: f64) (y: f64) = x < y
let gte__aii (x: i32) (y: i32) = x >= y
let gte__aff (x: f64) (y: f64) = x >= y
let lte__aii (x: i32) (y: i32) = x <= y
let lte__aff (x: f64) (y: f64) = x <= y
let add__aii (x: i32) (y: i32) = x + y
let sub__aii (x: i32) (y: i32) = x - y
let sub__aff (x: f64) (y: f64) = x - y
let div__aii (x: i32) (y: i32) = x / y
let div__aff (x: f64) (y: f64) = x / y
let mul__aii (x: i32) (y: i32) = x * y
let mul__aff (x: f64) (y: f64) = x * y


let deltaVec 't (zero: t) (n: i32) i (v: t) : [n]t =
  tabulate n (\j -> if j == i then v else zero)

let delta 't (zero: t) (i: i32) (j: i32) (v: t) =
  if i == j then v else zero

let rev__mul__Mat__Vec [r][c] (M: [r][c]f64) (v: [c]f64) (dr: [r]f64): ([r][c]f64, [c]f64) =
  (map (\x -> map (*x) v) dr,
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

let u__ranhashdoub (v: i32): f64 =
  5.42101086242752217E-20 * f64.u64(u__ranhash (u64.i32 v))
