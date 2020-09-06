all: px_sprite_gen.cmo

px_sprite_gen.cmi: px_sprite_gen.mli
	ocamlc -c $<

px_sprite_gen.cmo: px_sprite_gen.ml px_sprite_gen.cmi
	ocamlc -c $<

px_sprite_gen.cmx: px_sprite_gen.ml
	ocamlopt -c $<

clean:
	$(RM) px_sprite_gen.{o,cmo,cmi,cmx,cma,cmxa}

