include Garland

(* TODO! *)
module Color = struct include Garland.Color end
module Tween = struct include Garland.Tween end
module Anim = struct include Garland.Anim end

open Js_of_ocaml

let start_time = ref None

let jsoo_anim = ref None

let anims = ref []

let stop () =
  match !jsoo_anim with
  | None -> ()
  | Some eid ->
      jsoo_anim := None ;
      start_time := None ;
      anims := [] ;
      Dom_html.window##cancelAnimationFrame eid

let refresh time (garland, leds) =
  let nb_leds = List.length leds in
  let lst = render ~nb_leds garland time in
  let js_style = Js.string "style" in
  List.iter2
    (fun led (r, g, b, _) ->
      led##setAttribute
        js_style
        (Js.string (Printf.sprintf "background-color: rgb(%.0f, %.0f, %.0f)" r g b))
    )
    leds
    lst

let refresh_all time = List.iter (refresh time) !anims

let get_elapsed time = match !start_time with
  | None -> start_time := Some time ; 0.0
  | Some t0 -> (time -. t0) /. 1000.0

let rec request_refresh () =
  let anim =
    Dom_html.window##requestAnimationFrame
      (Js.wrap_callback (fun time ->
        refresh_all (get_elapsed time) ;
        if !anims <> [] then request_refresh ()
      ))
  in
  jsoo_anim := Some anim

let anim_register garland leds =
  anims := (garland, leds) :: !anims ;
  match !jsoo_anim with
  | None -> request_refresh ()
  | Some _ -> ()

let run ~nb_leds garland =
  let target = Dom_html.getElementById "output" in
  let div_leds = Dom_html.document##createElement (Js.string "div") in
  div_leds##setAttribute (Js.string "class") (Js.string "garland") ;
  let leds =
    List.init nb_leds
      (fun _ ->
        let led = Dom_html.document##createElement (Js.string "div") in
        led##setAttribute (Js.string "class") (Js.string "led") ;
        let _ = div_leds##appendChild (led :> Dom.node Js.t) in
        led
        )
  in
  let _ = target##appendChild (div_leds :> Dom.node Js.t) in
  anim_register garland leds

let () =
  let btn_stop = Dom_html.getElementById "stop" in
  btn_stop##.onclick := Dom.handler (fun _ -> stop () ; Js._true)
