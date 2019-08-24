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
  foe_texture: Texture.t;
  foe_pos: int * int;
  foe_last_shot: int;
  foe_shoot_freq: int;
  foe_gc: string;
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
}

let width, height = (640, 480)

let red    = (255, 0, 0)
let blue   = (0, 0, 255)
let green  = (0, 255, 0)
let yellow = (255, 255, 0)
let orange = (255, 127, 0)
let grey   = (100, 100, 100)
let alpha  = 255


let fill_rect renderer color (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let display  renderer bg_color foes player f_bullets p_bullets =
  Render.set_draw_color renderer bg_color alpha;
  Render.clear renderer;
  List.iter (fun bullet -> fill_rect renderer yellow bullet.bullet_pos) f_bullets;
  List.iter (fun foe ->
    let x, y = foe.foe_pos in
    let src_rect = Rect.make4 0 0 5 5 in
    let dst_rect = Rect.make4 x y 20 20 in
    Render.copy renderer ~texture:foe.foe_texture ~src_rect ~dst_rect ();
  ) foes;
  List.iter (fun pos -> fill_rect renderer green pos) p_bullets;
  fill_rect renderer blue player.p_pos;
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


let make_avatar renderer =
  let surface = Surface.create_rgb ~width:5 ~height:5 ~depth:32 in
  let rgb = (255, 255, 255) in
  let key = pixel_for_surface ~surface ~rgb in
  Surface.set_color_key surface ~enable:true ~key;
  let rgb =
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
  let foe_texture = make_avatar renderer in
  let foe_pos = (20 * Random.int (width / 20), -20) in
  let foe_last_shot = t in
  let foe_shoot_freq = 1600 + Random.int 1800 in
  let foe_gc = String.make 1 ' ' in
  let foe = { foe_texture; foe_pos; foe_last_shot; foe_shoot_freq; foe_gc } in
  Gc.finalise (fun foe ->
    Printf.printf "# finalising texture\n%!";
    Texture.destroy foe.foe_texture) foe;
  (foe)


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


let foe_not_touched p_bullets foe =
  let x, y = foe.foe_pos in
  let foe_rect = Rect.make4 x y 20 20 in
  not (
    List.exists (fun (x, y) ->
      let bullet_rect = Rect.make4 x y 20 20 in
      Rect.has_intersection foe_rect bullet_rect
    ) p_bullets)


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
  let foes = List.filter (foe_not_touched p_bullets) foes in
  (foes, f_bullets)


let player_touched player f_bullets =
  let x, y = player.p_pos in
  let player_rect = Rect.make4 x y 20 20 in
  List.exists (fun bullet ->
    let x, y = bullet.bullet_pos in
    let bullet_rect = Rect.make4 x y 20 20 in
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


let rec game_over  renderer foes player f_bullets p_bullets =
  let _ = event_loop player in
  display  renderer orange foes player f_bullets p_bullets;
  Timer.delay 200;
  game_over  renderer foes player f_bullets p_bullets


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
        ~width ~height ~flags:[]
  in
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
  } in
  let foes = [] in
  let p_bullets = [] in
  let f_bullets = [] in

  let rec main_loop ~foes ~player ~f_bullets ~p_bullets =
    let player = event_loop player in
    let t = Timer.get_ticks () in

    let foes, f_bullets = step_foes  renderer foes player f_bullets p_bullets t in
    let f_bullets = step_foes_bullets  f_bullets t in
    let p_bullets = step_player_bullets  p_bullets in
    let player, p_bullets = step_player  player p_bullets t in

    display  renderer grey foes player f_bullets p_bullets;
    Timer.delay 60;

    if player_touched  player f_bullets
    then game_over renderer foes player f_bullets p_bullets
    else main_loop ~foes ~player ~f_bullets ~p_bullets
  in
  main_loop ~foes ~player ~f_bullets ~p_bullets
