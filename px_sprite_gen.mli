(** Pixel Sprite Generator *)

type mask

val create_mask :
  data:int array ->
  width:int -> height:int -> mirrorX:bool -> mirrorY:bool -> mask

type options = {
  colored : bool;
  edgeBrightness : float;     (** value from 0.0 to 1.0 *)
  colorVariations : float;    (** value from 0.0 to 1.0 *)
  brightnessNoise : float;    (** value from 0.0 to 1.0 *)
  saturation : float;         (** value from 0.0 to 1.0 *)
}

val default_options : options
(** {[
  let default_options = {
    colored         = true;
    edgeBrightness  = 0.3;
    colorVariations = 0.2;
    brightnessNoise = 0.3;
    saturation      = 0.5;
  }
]} *)

type sprite = {
  width : int;
  height : int;
  options : options;
  mask : mask;
  mutable data : int array;
  mutable pixels : (int * int * int) array;
}

val create_sprite : mask -> ?options:options -> unit -> sprite

