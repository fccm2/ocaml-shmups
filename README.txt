These are four simple abstract shmup games.

You can play with the keyboard or with a joystick.
With the keyboard shoot with Z.

In Shmup-tc switch the shooting color with key X or
on the joystick with button 2.

In Shmup-te shoot with keys S, D and F for repectively
green, red, and blue, to kill enemies of the same colors.
(These keys should be the same on azerty and qwerty keyboards)

In Shmup-bg you are a bodyguard protecting a vip ship (the white
ship). You can absorb enemies's bullets before they touch the vip.
Game is over when the VIP is touched by a bullet.

In Shmup-ci, the player bullets are rotating arround him.
You lose one bullet each time you get touched by an enemy's bullet.

These games are released under a restrictionless Zlib license,
see the file LICENSE.txt for details.

You need ocamlsdl2 to run and/or compile:
https://github.com/fccm/OCamlSDL2

These games are known to work with:
- SDL2 version 2.0.10
- OCaml version 4.09.0 until 4.11.1
- OCamlSDL2 version 0.03 or 0.04

After installing ocamlsdl2 you can run the games with:
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_av.ml
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_tc.ml
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_te.ml
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_bg.ml
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_ci.ml

If you just compiled ocamlsdl2 without installing it:
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_av.ml
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_tc.ml
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_te.ml
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_bg.ml
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_ci.ml

Executable binaries for Windows are available here:
https://blueprawn.itch.io/minishmups

(You can also use Windows binaries on Linux with Wine)

Write to me to tell me what you think about these games,
or to tell me what is your higher score:
monnier.florent (at) gmail.com

