These are three simple abstract shmup games.

You can play with the keyboard or with a joystick.
With the keyboard shoot with Z.

In Shmup-tc switch the shooting color with key X or
on the joystick with button 2.

In Shmup-te shoot with keys S, D and F for repectively
green, red, and blue, to kill enemies of the same colors.
(These keys should be the same on azerty and qwerty keyboards)

These games are released under a restrictionless Zlib license,
see the file LICENSE.txt for details.

You need ocamlsdl2 to run and/or compile:
https://github.com/fccm/OCamlSDL2

After installing ocamlsdl2 you can run the games with:
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_av.ml
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_tc.ml
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_te.ml

If you just compiled ocamlsdl2 without installing it:
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_av.ml
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_tc.ml
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_te.ml

Executable binaries for Windows are available here:
https://blueprawn.itch.io/minishmups

(You can also use Windows binaries on Linux with Wine)

Write to me to tell me what you think about these games,
or to tell me what is your higher score:
monnier.florent (at) gmail.com

