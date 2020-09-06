type mask

val create_mask :
  data:int array ->
  width:int -> height:int -> mirrorX:bool -> mirrorY:bool -> mask

type options = {
  colored : bool;
  edgeBrightness : float;
  colorVariations : float;
  brightnessNoise : float;
  saturation : float;
}

val default_options : options

type sprite = {
  width : int;
  height : int;
  options : options;
  mask : mask;
  mutable data : int array;
  mutable pixels : (int * int * int) array;
}

val create_sprite : mask -> ?options:options -> unit -> sprite

