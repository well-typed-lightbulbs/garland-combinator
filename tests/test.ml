open Garland

let test = rotate (red --> black)

let () = Garland_graphics.run ~nb_leds:60 [ test ]
