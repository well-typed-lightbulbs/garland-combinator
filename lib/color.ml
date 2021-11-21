type t = { h : float ; s : float ; l : float ; a : float }

let mod_float x m =
  let y = mod_float x m in
  if y < 0.0
  then y +. m
  else y

let black = { h = 0.0 ; s = 0.0 ; l = 0.0 ; a = 1.0 }
let white = { h = 0.0 ; s = 0.0 ; l = 1.0 ; a = 1.0 }

let v ~h ?(s = 1.0) ?(l = 0.5) ?(a = 1.0) () =
  { h = h ; s ; l ; a }

let transparent = { h = 0.0 ; s = 0.0 ; l = 1.0 ; a = 0.0 }

let red = v ~h:0.0 ()
let yellow = v ~h:(1. /. 6.) ()
let green = v ~h:(2. /. 6.) ()
let cyan = v ~h:(3. /. 6.) ()
let blue = v ~h:(4. /. 6.) ()
let pink = v ~h:(5. /. 6.) ()

let vec_of_hue h s =
  let h = Tween.tau *. h in
  s *. cos h, s *. sin h

let hue_of_vec x y =
  atan2 y x /. Tween.tau,
  sqrt (x *. x +. y *. y)

let to_vec t =
  let h, s = vec_of_hue t.h t.s in
  h, s, t.l, t.a

let of_vec (h, s, l, a) =
  let h, s = hue_of_vec h s in
  { h ; s ; l ; a }

let average = function
  | [] -> transparent
  | t::ts ->
      let n = 1 + List.length ts in
      let n = float n in
      let (h, s, l, a) =
        List.fold_left
          (fun (h, s, l, a) t ->
            let (h', s', l', a') = to_vec t in
            (h +. h', s +. s', l +. l', a +. a')
          )
          (to_vec t)
          ts
      in
      of_vec (h /. n, s /. n, l /. n, a /. n)

(* https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB_alternative *)
let to_rgb t =
  let { h ; s ; l ; a = _ } = t in
  let h = h *. 360.0 in
  let h30 = h /. 30.0 in
  let a = s *. min l (1.0 -. l) in
  let f n =
    let k = mod_float (n +. h30) 12.0 in
    let r = l -. a *. max (-1.0) (min (min (k -. 3.0) (9.0 -. k)) 1.0) in
    (255.0 *. r)
  in
  (f 0., f 8., f 4., t.a)

let of_rgba (r, g, b, a) =
  let r, g, b = r /. 255., g /. 255., b /. 255. in
  let xmax = max (max r g) b in
  let xmin = min (min r g) b in
  let c = xmax -. xmin in
  let l = (xmax +. xmin) /. 2. in
  let h =
    if c = 0.0 then 0.0
    else if xmax = r then (g -. b) /. (6. *. c)
    else if xmax = g then (2.0 +. (b -. r) /. c) /. 6.
    else if xmax = b then (4.0 +. (r -. g) /. c) /. 6.
    else 0.0
  in
  let s =
    if l = 0.0 || l = 1.0
    then 0.0
    else c /. (1.0 -. abs_float (2. *. xmax -. c -. 1.0))
  in
  { h ; s ; l ; a }

let of_rgba (r, g, b, a) = of_rgba (float r, float g, float b, a)

let of_hex ?(a = 1.0) hex =
  let b =          hex land 255 in
  let g = (hex lsr  8) land 255 in
  let r = (hex lsr 16) land 255 in
  of_rgba (r, g, b, a)

let over a b =
  let alpha = a.a +. b.a *. (1.0 -. a.a) in
  let c x y = x *. a.a +. y *. b.a *. (1.0 -. a.a) /. alpha in
  let ahx, ahy = vec_of_hue a.h a.s in
  let bhx, bhy = vec_of_hue b.h b.s in
  let h, s = hue_of_vec (c ahx bhx) (c ahy bhy) in
  { h ; s ; l = c a.l b.l ; a = alpha }

let map_hue f t = { t with l = f t.h }
let map_luminosity f t = { t with l = f t.l }
let map_saturation f t = { t with l = f t.s }
let map_alpha f t = { t with l = f t.a }

let lerp a b t =
  let ahx, ahy = vec_of_hue a.h a.s in
  let bhx, bhy = vec_of_hue b.h b.s in
  let h, s = hue_of_vec (Tween.lerp ahx bhx t) (Tween.lerp ahy bhy t) in
  { h ; s ; l = Tween.lerp a.l b.l t ; a = Tween.lerp a.a b.a t }
