let int = int_of_float

module G = Graphics

let render_horz ~nb_leds time imgs =
  let w, h = G.size_x (), G.size_y () in
  let nb_imgs = List.length imgs in
  let ts = List.map (fun img -> Garland.render ~nb_leds img time) imgs in
  let nb = nb_leds in
  G.set_color G.black ;
  G.fill_rect 0 0 w h ;
  let sx = (w / nb) in
  let x_offset = (w - nb * sx) / 2 in
  let y_offset = (h - nb_imgs * sx) / (nb_imgs + 1) in
  List.iteri
    (fun j t ->
      let y = (j + 1) * (y_offset + sx) - sx in
      List.iteri
        (fun i (r, g, b, _a) ->
          let x = x_offset + i * sx in
          let color = G.rgb (int r) (int g) (int b) in
          G.set_color color ;
          G.fill_rect x y sx sx
        )
        t
    )
    (List.rev ts) ;
  G.synchronize ()

let render_vert ~nb_leds time imgs =
  let w, h = G.size_x (), G.size_y () in
  let nb_imgs = List.length imgs in
  let ts = List.map (fun img -> Garland.render ~nb_leds img time) imgs in
  let nb = nb_leds in
  G.set_color (G.rgb 20 20 20) ;
  G.fill_rect 0 0 w h ;
  let sy = h / nb in
  let y_offset = (h - nb * sy) / 2 in
  let x_offset = (w - nb_imgs * sy) / (nb_imgs + 1) in
  List.iteri
    (fun j t ->
      let x = (j + 1) * (x_offset + sy) - sy in
      List.iteri
        (fun i (r, g, b, _) ->
          let y = y_offset + i * sy in
          let color = G.rgb (int r) (int g) (int b) in
          G.set_color color ;
          G.fill_rect x y sy sy
        )
        t
    )
    ts ;
  G.synchronize ()


let run ~nb_leds anims =
  G.open_graph " " ;
  G.auto_synchronize false ;

  let start = Unix.gettimeofday () in

  let rec go () =
    let now = Unix.gettimeofday () in
    render_vert ~nb_leds (now -. start) anims ;
    let t1 = Unix.gettimeofday () in
    Thread.delay (max 0.0 (1. /. 24. -. (t1 -. now))) ;
    let event = G.wait_next_event [G.Poll ; G.Key_pressed] in
    if event.keypressed
    then ()
    else go ()
  in

  Thread.join (Thread.create go ()) ;

  ignore (G.read_key ());
  G.close_graph ()
