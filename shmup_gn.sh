make -f px_sprite_gen.mk px_sprite_gen.cmo
ocaml -I ../OCamlSDL2/src sdl2.cma sdl2-ba.cma px_sprite_gen.cmo shmup_gn.ml
