(* A Minimalist Shmup Game
 Copyright (C) 2019 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

type foe = {
  foe_pos: int * int;
  foe_last_shot: int;
  foe_shoot_freq: int;
  foe_texture: Texture.t;
}

type foe_bullet = {
  bullet_pos: int * int;
  bullet_line: (int * int) * (int * int);
  bullet_birth: int;
}

type player_dir = {
  left: bool;
  right: bool;
  up: bool;
  down: bool;
}

type player = {
  p_pos: int * int;
  p_last_shot: int;
  p_shoot_freq: int;
  p_shooting: bool;
  p_dir: player_dir;
  p_texture: Texture.t;
}

let width, height = (640, 480)

let red    = (255, 0, 0)
let blue   = (0, 0, 255)
let green  = (0, 255, 0)
let yellow = (255, 255, 0)
let orange = (255, 127, 0)
let grey   = (100, 100, 100)
let alpha  = 255

let score = ref 0

let letters = [
  '0', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '1', [|
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '2', [|
    [| 1; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  '3', [|
    [| 1; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 1; 1; 1; 0; 0 |];
  |];
  '4', [|
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 1; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
  |];
  '5', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 0; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  '6', [|
    [| 0; 0; 1; 1; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '7', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  '8', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '9', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 1 |];
    [| 0; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  ' ', [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |];
  ':', [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |];
  'a', [|
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'b', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  'c', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'd', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  'e', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 1 |];
  |];
  'f', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  'g', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'h', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'i', [|
    [| 0; 1; 1; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'j', [|
    [| 0; 0; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 1; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 0 |];
  |];
  'k', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 1; 0 |];
    [| 1; 1; 1; 0; 0 |];
    [| 1; 0; 0; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'l', [|
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  'm', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 0; 1; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'n', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'o', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'p', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  'q', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 1 |];
  |];
  'r', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 1; 0; 0 |];
    [| 1; 0; 0; 1; 0 |];
  |];
  's', [|
    [| 0; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
    [| 0; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  't', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
  |];
  'u', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'v', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
  |];
  'w', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
  |];
  'x', [|
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'y', [|
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
  |];
  'z', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 1 |];
  |];
]

let src_rect = Rect.make4 0 0 5 5


let fill_rect40 renderer color x y =
  let rect = Rect.make4 x y 40 40 in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let make_background () =
  Array.init 12 (fun y ->
    Array.init 16 (fun x ->
      let v = 90 + Random.int 20 in
      (v, v, v)))


let background = make_background ()


let display_background renderer playing =
  for y = 0 to pred 12 do
    for x = 0 to pred 16 do
      let _x = x * 40
      and _y = y * 40 in
      let rgb = background.(y).(x) in
      if playing
      then fill_rect40 renderer rgb _x _y
      else
        let r, g, b = rgb in
        fill_rect40 renderer (r + 40, g / 2, b / 3) _x _y
    done
  done


let display  renderer playing player f_bullets p_bullets foes
      f_bullet_tex p_bullet_tex letters_tex =

  display_background renderer playing;

  let draw_letter texture x y =
    let dst_rect = Rect.make4 x y 15 15 in
    Render.copy renderer ~texture ~src_rect ~dst_rect ();
  in
  (*
  List.iteri (fun i (c, tex) ->
    let x = (i mod 20) * 19 + 10 in
    let y = (i / 20) * 19 + 10 in
    draw_letter tex x y;
  ) letters_tex;
  *)
  let s = Printf.sprintf "score: %d" !score in
  String.iteri (fun i c ->
    let tex = List.assoc c letters_tex in
    let x = i * 20 + 10 in
    let y = 10 in
    draw_letter tex x y;
  ) s;

  List.iter (fun bullet ->
    let x, y = bullet.bullet_pos in
    let dst_rect = Rect.make4 x y 20 20 in
    Render.copy renderer ~texture:f_bullet_tex ~src_rect ~dst_rect ();
  ) f_bullets;

  List.iter (fun foe ->
    let x, y = foe.foe_pos in
    let dst_rect = Rect.make4 x y 20 20 in
    Render.copy renderer ~texture:foe.foe_texture ~src_rect ~dst_rect ();
  ) foes;

  List.iter (fun pos ->
    let x, y = pos in
    let dst_rect = Rect.make4 x y 20 20 in
    Render.copy renderer ~texture:p_bullet_tex ~src_rect ~dst_rect ();
  ) p_bullets;

  begin
    let x, y = player.p_pos in
    let dst_rect = Rect.make4 x y 20 20 in
    Render.copy renderer ~texture:player.p_texture ~src_rect ~dst_rect ();
  end;

  Render.render_present renderer;
;;


let proc_events player = function
  | Event.KeyDown { Event.keycode = Keycode.Left } ->
      { player with p_dir = { player.p_dir with left = true } }
  | Event.KeyDown { Event.keycode = Keycode.Right } ->
      { player with p_dir = { player.p_dir with right = true } }
  | Event.KeyDown { Event.keycode = Keycode.Up } ->
      { player with p_dir = { player.p_dir with up = true } }
  | Event.KeyDown { Event.keycode = Keycode.Down } ->
      { player with p_dir = { player.p_dir with down = true } }

  | Event.KeyUp { Event.keycode = Keycode.Left } ->
      { player with p_dir = { player.p_dir with left = false } }
  | Event.KeyUp { Event.keycode = Keycode.Right } ->
      { player with p_dir = { player.p_dir with right = false } }
  | Event.KeyUp { Event.keycode = Keycode.Up } ->
      { player with p_dir = { player.p_dir with up = false } }
  | Event.KeyUp { Event.keycode = Keycode.Down } ->
      { player with p_dir = { player.p_dir with down = false } }

  | Event.KeyDown { Event.keycode = Keycode.Z } ->
      { player with p_shooting = true }
  | Event.KeyUp { Event.keycode = Keycode.Z } ->
      { player with p_shooting = false }

  | Event.KeyDown { Event.keycode = Keycode.F } ->
      Gc.full_major (); player

  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0

  | _ -> player


let rec event_loop player =
  match Event.poll_event () with
  | None -> player
  | Some ev ->
      let player = proc_events player ev in
      event_loop player


let pixel_for_surface ~surface ~rgb =
  let fmt = Surface.get_pixelformat_t surface in
  let pixel_format = Pixel.alloc_format fmt in
  let pixel = Pixel.map_RGB pixel_format rgb in
  Pixel.free_format pixel_format;
  (pixel)


let make_avatar renderer ?color () =
  let surface = Surface.create_rgb ~width:5 ~height:5 ~depth:32 in
  let rgb = (255, 255, 255) in
  let key = pixel_for_surface ~surface ~rgb in
  Surface.set_color_key surface ~enable:true ~key;
  let rgb =
    match color with
    | Some rgb -> rgb
    | None ->
        (155 + Random.int 100,
         155 + Random.int 100,
         155 + Random.int 100)
  in
  let color = pixel_for_surface ~surface ~rgb in
  for x1 = 0 to pred 3 do
    for y = 0 to pred 5 do
      let x2 = (pred 5) - x1 in
      if Random.bool ()
      then begin
        Surface.fill_rect surface (Rect.make4 x1 y 1 1) color;
        Surface.fill_rect surface (Rect.make4 x2 y 1 1) color;
      end else begin
        Surface.fill_rect surface (Rect.make4 x1 y 1 1) 0xFFFFFFl;
        Surface.fill_rect surface (Rect.make4 x2 y 1 1) 0xFFFFFFl;
      end
    done
  done;
  let texture = Texture.create_from_surface renderer surface in
  Surface.free surface;
  (texture)


let make_bullet_tex renderer pattern ~color =
  let surface = Surface.create_rgb ~width:5 ~height:5 ~depth:32 in
  let rgb = (255, 255, 255) in
  let key = pixel_for_surface ~surface ~rgb in
  Surface.set_color_key surface ~enable:true ~key;
  let color = pixel_for_surface ~surface ~rgb:color in
  Array.iteri (fun y row ->
    Array.iteri (fun x v ->
      if v = 1
      then Surface.fill_rect surface (Rect.make4 x y 1 1) color
      else Surface.fill_rect surface (Rect.make4 x y 1 1) 0xFFFFFFl
    ) row
  ) pattern;
  let texture = Texture.create_from_surface renderer surface in
  Surface.free surface;
  (texture)


let f_bullet_inside bullet =
  let x, y = bullet.bullet_pos in
  (y < height) &&
  (x < width) &&
  (y > -20) &&
  (x > -20)


let vec_mul (x, y) k =
  (x * k,
   y * k)

let vec_div (x, y) k =
  (x / k,
   y / k)

let vec_add (ax, ay) (bx, by) =
  (ax + bx,
   ay + by)

let point_on_line (p1, p2) i t =
  let ti = i - t in
  vec_div (
      vec_add
        (vec_mul p1 ti)
        (vec_mul p2 t)
    ) i


let step_foes_bullets f_bullets t =
  let step_bullet bullet =
    let dt = t - bullet.bullet_birth in
    let p = point_on_line bullet.bullet_line 6000 dt in
    { bullet with bullet_pos = p }
  in
  let f_bullets = List.map step_bullet f_bullets in
  let f_bullets = List.filter f_bullet_inside f_bullets in
  (f_bullets)


let new_foe renderer t =
  let foe_texture = make_avatar renderer () in
  let foe_pos = (20 * Random.int (width / 20), -20) in
  let foe_last_shot = t in
  let foe_shoot_freq = 1600 + Random.int 1800 in
  { foe_texture; foe_pos; foe_last_shot; foe_shoot_freq }


let new_foes_opt foes renderer t =
  if Random.int 100 > 2
  then foes
  else
    let new_foe = new_foe renderer t in
    new_foe :: foes


let gun_new_f_bullets f_bullets foes player t =
  let rec aux acc1 acc2 foes =
    match foes with
    | [] -> (acc1, acc2)
    | foe :: foes ->
        if t - foe.foe_last_shot < foe.foe_shoot_freq
        then aux acc1 (foe :: acc2) foes
        else
          let updated_foe = { foe with foe_last_shot = t } in
          let bullet =
            { bullet_pos = foe.foe_pos;
              bullet_line = (foe.foe_pos, player.p_pos);
              bullet_birth = t; }
          in
          aux (bullet :: acc1) (updated_foe :: acc2) foes
  in
  let new_f_bullets, foes = aux [] [] foes in
  let f_bullets = List.rev_append new_f_bullets f_bullets in
  let foes = List.rev foes in
  (f_bullets, foes)


let foe_inside foe =
  let (x, y) = foe.foe_pos in
  (y < height)


let foe_touched p_bullets foe =
  let x, y = foe.foe_pos in
  let foe_rect = Rect.make4 x y 20 20 in
  List.exists (fun (x, y) ->
    let bullet_rect = Rect.make4 x y 20 20 in
    Rect.has_intersection foe_rect bullet_rect
  ) p_bullets


let step_foes  renderer foes player f_bullets p_bullets t =
  let step_foe foe =
    let (x, y) = foe.foe_pos in
    let new_pos = (x, y + 2) in
    { foe with foe_pos = new_pos }
  in
  let foes = new_foes_opt foes renderer t in
  let f_bullets, foes = gun_new_f_bullets f_bullets foes player t in
  let foes = List.map step_foe foes in
  let foes = List.filter foe_inside foes in
  let foes =
    List.filter (fun foe ->
      if foe_touched p_bullets foe
      then (incr score; Texture.destroy foe.foe_texture; false)
      else true
    ) foes
  in
  (foes, f_bullets)


let player_touched player f_bullets =
  let x, y = player.p_pos in
  let player_rect = Rect.make4 x y 20 20 in
  List.exists (fun bullet ->
    let x, y = bullet.bullet_pos in
    let x, y = x + 4, y + 4 in
    let bullet_rect = Rect.make4 x y 12 12 in
    Rect.has_intersection player_rect bullet_rect
  ) f_bullets


let player_moving player =
  let x, y = player.p_pos in
  { player with p_pos =
    match player.p_dir with
    | { left = true; right = false; up = false; down = false } -> (x - 10, y)
    | { left = false; right = true; up = false; down = false } -> (x + 10, y)
    | { left = false; right = false; up = true; down = false } -> (x, y - 10)
    | { left = false; right = false; up = false; down = true } -> (x, y + 10)

    | { left = true; right = false; up = true; down = false } -> (x - 7, y - 7)
    | { left = true; right = false; up = false; down = true } -> (x - 7, y + 7)
    | { left = false; right = true; up = true; down = false } -> (x + 7, y - 7)
    | { left = false; right = true; up = false; down = true } -> (x + 7, y + 7)

    | _ -> (x, y)
  }


let step_player_bullets p_bullets =
  p_bullets |>
    List.map (fun (x, y) -> (x, y - 8)) |>
    List.filter (fun (x, y) -> y > -20)


let player_shooting  player p_bullets t =
  if player.p_shooting
  && t - player.p_last_shot > player.p_shoot_freq
  then (* shoot *)
    let bullet = player.p_pos in
    let player = { player with p_last_shot = t } in
    (player, bullet :: p_bullets)
  else
    (player, p_bullets)


let step_player  player p_bullets t =
  let player = player_moving player in
  let player, p_bullets = player_shooting  player p_bullets t in
  (player, p_bullets)


let rec game_over renderer player f_bullets p_bullets foes
      f_bullet_tex p_bullet_tex letters_tex =
  let _ = event_loop player in
  display  renderer false player f_bullets p_bullets foes
      f_bullet_tex p_bullet_tex letters_tex;
  Timer.delay 200;
  game_over renderer player f_bullets p_bullets foes
      f_bullet_tex p_bullet_tex letters_tex


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  let player_texture = make_avatar renderer ~color:blue () in
  let player = {
    p_pos = (width / 2, height - 60);
    p_last_shot = Timer.get_ticks ();
    p_shoot_freq = 400;
    p_shooting = false;
    p_dir =
      { left = false;
        right = false;
        up = false;
        down = false;
      };
    p_texture = player_texture;
  } in
  let foes = [] in
  let p_bullets = [] in
  let f_bullets = [] in

  let fb_pattern = [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |] in
  let pb_pattern = [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |] in

  let f_bullet_tex = make_bullet_tex renderer fb_pattern ~color:yellow in
  let p_bullet_tex = make_bullet_tex renderer pb_pattern ~color:green in

  let letters_tex =
    List.map (fun (c, pat) ->
      let tex = make_bullet_tex renderer pat ~color:green in
      (c, tex)
    ) letters
  in

  let rec main_loop ~player ~f_bullets ~p_bullets ~foes =
    let player = event_loop player in
    let t = Timer.get_ticks () in

    let foes, f_bullets = step_foes  renderer foes player f_bullets p_bullets t in
    let f_bullets = step_foes_bullets  f_bullets t in
    let p_bullets = step_player_bullets  p_bullets in
    let player, p_bullets = step_player  player p_bullets t in

    display  renderer true player f_bullets p_bullets foes
      f_bullet_tex p_bullet_tex letters_tex;

    Timer.delay 50;

    if player_touched  player f_bullets
    then begin
      Printf.printf "# score: %d\n%!" !score;
      game_over renderer player f_bullets p_bullets foes
          f_bullet_tex p_bullet_tex letters_tex
    end
    else main_loop ~player ~f_bullets ~p_bullets ~foes
  in
  main_loop ~player ~f_bullets ~p_bullets ~foes
