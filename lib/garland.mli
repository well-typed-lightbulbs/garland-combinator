module Color : sig
  type t
  val v : h:float -> ?s:float -> ?l:float -> ?a:float -> unit -> t
  (** See https://en.wikipedia.org/wiki/HSL_and_HSV
      All parameters should be from [0.0] to [1.0].
      - The [h]ue varies from [0.0] to [1.0]
      - The saturation defaults value is [1.0]. A value of [0.0] is grey.
      - The luminosity defaults value is [0.5]. A value of [0.0] is pitch black, while [1.0] is full white.
      - The alpha defauls value is [1.0] A value of [0.0] is transparent.
  *)

  val red : t
  val yellow : t
  val green : t
  val cyan : t
  val blue : t
  val pink : t
  val black : t
  val white : t

  val transparent : t

  val map_hue : (float -> float) -> t -> t
  val map_luminosity : (float -> float) -> t -> t
  val map_saturation : (float -> float) -> t -> t
  val map_alpha : (float -> float) -> t -> t

  val of_hex : ?a:float -> int -> t
  (** [of_hex 0xFF0000] is red for example. *)

  val of_rgba : int * int * int * float -> t
end
module Tween : sig
  type t = float -> float
  (** A tween is a function from [0;1.] to [0;1.] *)

  val lerp : float -> float -> t
  val sin : t
  val square : t
  val out_bounce : t
end

module Anim : sig
  type 'a t

  val const : 'a -> 'a t
  val empty : 'a -> 'a t
  val time : float t

  val seq : 'a t -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val map2_align : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val loop : 'a t -> 'a t
  val noloop : 'a t -> 'a t

  val rotate : float -> 'a t -> 'a t
  val offset : float -> 'a t -> 'a t
  val speed : float -> 'a t -> 'a t
  val duration : float -> 'a t -> 'a t
  val truncate : float -> 'a t -> 'a t

  val at : float -> 'a t -> 'a

  val left : 'a t -> 'a t
  val right : 'a t -> 'a t

  val tween : Tween.t -> 'a t -> 'a t

  val lerp : float -> Color.t t -> Color.t t -> Color.t t
end

type 'a t = 'a Anim.t Anim.t

type color = Color.t

type garland = color t

(** {1 Colors}

    The primitive garlands are static colors: *)

val red : garland
val yellow : garland
val green : garland
val cyan : garland
val blue : garland
val pink : garland
val black : garland
val white : garland
val transparent : garland

val gradient : garland

val hex : ?a:float -> int -> garland
val of_rgb : ?a:float -> int -> int -> int -> garland

val map_luminosity : (float -> float) -> garland -> garland
val map_alpha : (float -> float) -> garland -> garland
val map_saturation : (float -> float) -> garland -> garland

(** {1 Composition} *)

val ( --- ) : 'a t -> 'a t -> 'a t
(** [a --- b] is the garland [a] drawn on the left of the garland [b].
    The operator is associative and will distribute the space available
    to each of the garland.
*)

val flip : 'a t -> 'a t
(** [flip a] reverses the garland from right to left.

    [flip (a --- b) === flip b --- flip a]
*)

val weight : float -> 'a t -> 'a t
(** [weight size a] defines the width of the garland [a].
    For example, [weight 2.0 red == red --- red]
 *)

val point : 'a t -> 'a t
(** Set the weight to 0.0 *)

val ( --> ) : garland -> garland -> garland
(** [a --> b] is the gradient from the garland [a] to the garland [b]. *)

val ( +++ ) : garland -> garland -> garland
(** [a +++ b] is the garland [a] drawn on top of the garland [b], with alpha blending. *)

val ( === ) : 'a t -> 'a t -> 'a t
(** [a === b] sequences the garland [a], followed by the garland [b]. *)

val ( ==> ) : garland -> garland -> garland
(** [a ==> b] plays [a], then morph it into [b]. *)

val tween : Tween.t -> 'a t -> 'a t
(** Transform the time with the given tween. *)

val rev : 'a t -> 'a t
(** [rev a] reverses the flow of time.

    [rev (a === b)  =  rev b === rev a]

    [rev t  =  speed (-1.0) t]
*)

val speed : float -> 'a t -> 'a t
(** Speed up (>1.0), or slow down (<1.0) or reverses time. *)

val duration : float -> 'a t -> 'a t
(** Set the duration of the animation (by speeding or slowing down). *)

val instant : 'a t -> 'a t
(** Set the animation to have a duration of 0.0 *)

(** {1 Applicative functor} *)

val const : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val time : float t

val loop : 'a t -> 'a t
val noloop : 'a t -> 'a t

val offset : float -> garland -> garland

val delay : float -> garland -> garland

val truncate : float -> 'a t -> 'a t

val rotate : garland -> garland
(** [rotate a] plays an animation that rotates the garland from right to left. *)


val render : nb_leds:int -> garland -> float -> (float * float * float * float) list
