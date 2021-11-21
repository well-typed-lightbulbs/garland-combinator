open Garland_jsoo

let rec test n =
  if n = 0
  then instant cyan
  else let t = test (n - 1) in
       instant cyan ==> speed 1.5 (t --- black --- t)

let test = weight 0.0 (instant cyan) ==> test 6

let test = black --- test --- black
  
let test = speed 1.5 test
  
let () = run ~nb_leds:60 test
