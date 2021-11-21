module Color = Color
module Tween = Tween
module Anim = Anim

type color = Color.t

type 'a t = 'a Anim.t Anim.t

type garland = color t

let const v = Anim.(const (const v))

let speed factor t = Anim.speed factor t

let time = Anim.map Anim.const Anim.time

let ( +++ ) a b = Anim.map2 (Anim.map2_align Color.over) a b

let ( --- ) a b = Anim.map2 Anim.seq a b

let ( === ) a b = Anim.seq a b

let loop t = Anim.loop t

let ( --> ) c0 c1 =
  let open Anim in
  Anim.const (fun c0 c1 -> Anim.with_time (fun t c0 c1 -> Color.lerp c0 c1 t) <*> c0 <*> c1)
    <*> c0
    <*> c1

let right t = Anim.map Anim.right t
let left  t = Anim.map Anim.left t

let ( --> ) c0 c1 =
  c0 --- (right c0 --> left c1) --- c1

let ( ==> ) xx yy =
  Anim.(
    with_time Anim.lerp
    <*> xx
    <*> yy
  )

let first t = Anim.left  t
let last  t = Anim.right t

let ( ==> ) c0 c1 =
  c0 === (last c0 ==> first c1) === c1

let rotate t =
  Anim.(with_time (fun dt anim -> rotate (dt *. anim.duration) anim) <*> t)

let map f t = Anim.map (Anim.map f) t
let map2 f t r = Anim.map2 (Anim.map2 f) t r

let weight w t = Anim.map (Anim.duration w) t
let duration w t = Anim.duration w t

let ( <*> ) f x = Anim.map2 (fun f x -> Anim.( <*> ) f x) f x

let rev t = Anim.speed (-1.0) t
let flip t = Anim.map (Anim.speed (-1.0)) t

let offset dx t = Anim.rotate dx t

let truncate d t = Anim.truncate d t

let tween tw t = Anim.tween tw t

let delay dt t = duration dt (first t) === t

let noloop t = Anim.noloop t

let instant t = Anim.moment t
let point t = Anim.map Anim.moment t

let transparent = const Color.transparent
let red = const Color.red
let yellow = const Color.yellow
let green = const Color.green
let cyan = const Color.cyan
let blue = const Color.blue
let pink = const Color.pink
let black = const Color.black
let white = const Color.white

let gradient = Anim.const (Anim.map (fun h -> Color.v ~h ()) Anim.time)

let map_luminosity f t =
  Anim.map (Anim.map (Color.map_luminosity f)) t

let map_alpha f t =
  Anim.map (Anim.map (Color.map_alpha f)) t

let map_saturation f t =
  Anim.map (Anim.map (Color.map_saturation f)) t

let hex ?a rgb = const (Color.of_hex ?a rgb)
let of_rgb ?(a = 1.0) r g b = const (Color.of_rgba (r, g, b, a))




let nb_samples = 10

let sample f i j =
  Color.average
  @@
  List.init nb_samples
    (fun k -> f (i +. float k *. j /. float nb_samples))

let sample ~nb_leds f =
  List.init nb_leds
    (fun i ->
      sample f.Anim.f (f.Anim.duration *. float i /. float nb_leds) (f.Anim.duration /. float nb_leds)
    )

let mod_float' x m =
  let x = mod_float x m in
  if x < 0.0
  then x +. m
  else x

let render ~nb_leds img time =
  List.map
    (fun c -> Color.to_rgb (Color.over c Color.black))
    (sample ~nb_leds (img.Anim.f (mod_float' time img.Anim.duration)))
