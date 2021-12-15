let test5 = speed 1.5 (flip cyan --> pink --> red --> cyan --> pink ==> blue --> red --> cyan --> pink --> blue --> pink --> red)

let () = run ~nb_leds:60 test5 ;;

let test6 = test5 === rev test5

let () = run ~nb_leds:60 test6 ;;

