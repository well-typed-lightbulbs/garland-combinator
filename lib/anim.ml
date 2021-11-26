type 'a t =
  { f : float -> 'a
  ; duration : float
  }

let const v = { f = (fun _ -> v) ; duration = 1.0 }
let empty v = { f = (fun _ -> v) ; duration = 0.0 }
let with_time f = { f = f ; duration = 1.0 }

let time = { f = (fun t -> t) ; duration = 1.0 }

let seq a b =
  { duration = a.duration +. b.duration
  ; f = (fun t ->
           if t < a.duration
           then a.f t
           else b.f (t -. a.duration))
  }

let mod_float' x m =
  let x = mod_float x m in
  if x < 0.0
  then x +. m
  else x

let moment t =
  { duration = 0.0
  ; f = (fun _ -> t.f 0.0)
  }

let speed factor x =
  if factor = 0.0
  then moment x
  else
    { duration = abs_float (x.duration /. factor)
    ; f = (fun t -> x.f (if factor < 0.0 then x.duration +. factor *. t else factor *. t))
    }

let duration duration x =
  if x.duration = 0.0
  then x
  else speed (x.duration /. duration) x

let map f t =
  { duration = t.duration
  ; f = (fun x -> f (t.f x))
  }

let map2 f t0 t1 =
  { duration = max t0.duration t1.duration
  ; f = (fun x -> f (t0.f x) (t1.f x))
  }

let map2_align f t0 t1 =
  let duration = max t0.duration t1.duration in
  { duration
  ; f = (fun x -> f (t0.f (x *. t0.duration /. duration)) (t1.f (x *. t1.duration /. duration)))
  }

let loop t =
  { duration = t.duration
  ; f = (fun x -> t.f (mod_float' x t.duration))
  }

let rotate dx t =
  { duration = t.duration
  ; f = (fun x -> t.f (mod_float' (x +. dx) t.duration))
  }

let truncate duration t =
  { duration
  ; f = t.f
  }

let ( <*> ) f x = map2 (fun f x -> f x) f x

let ( let+ ) f t = map t f
let ( and+ ) a b = map2 (fun x y -> x, y) a b

let at x t = t.f x

let right t =
  { duration = 1.0
  ; f = (fun x -> t.f (max t.duration (x +. t.duration)))
  }

let left t =
  { duration = 1.0
  ; f = (fun x -> t.f (min 0.0 (x -. 1.0)))
  }

let tween tw t =
  { duration = t.duration
  ; f = (fun x -> t.f (t.duration *. (tw (x /. t.duration))))
  }

let noloop t =
  { duration = t.duration
  ; f = (fun x -> t.f (max 0.0 (min x t.duration)))
  }

let lerp t a b =
  let duration = Tween.lerp a.duration b.duration t in
  { duration
  ; f = (fun x -> Color.lerp (a.f (a.duration *. x /. duration)) (b.f (b.duration *. x /. duration)) t)
  }
