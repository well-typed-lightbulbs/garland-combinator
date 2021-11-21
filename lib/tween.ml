type t = float -> float

let lerp x0 x1 t =
  if t < 0.0
  then x0
  else if t > 1.0
  then x1
  else x0 +. (x1 -. x0) *. t

let tau = 8.0 *. atan 1.0

let sin t = 0.5 +. 0.5 *. sin (t *. tau)

let square t = t *. t

let out_bounce x =
  let n1 = 7.5625
  and d1 = 2.75 in
  if x < 0.0
  then 0.0
  else if x < 1. /. d1
  then n1 *. x *. x
  else if x < 2. /. d1
  then n1 *. square (x -. 1.5 /. d1) +. 0.75
  else if x < 2.5 /. d1
  then n1 *. square (x -. 2.25 /. d1) +. 0.9375
  else if x < 1.0
  then n1 *. square (x -. 2.625 /. d1) +. 0.984375
  else 1.0
