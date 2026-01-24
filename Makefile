NAME = crash
MAIN = main
DEPS = raylib raylib-callbacks
WIN_DLLS = libwinpthread-1 libffi-6

default: deps exe

vars:
	@echo 'NAME = $(NAME)'
	@echo 'MAIN = $(MAIN)'
	@echo 'DEPS = $(DEPS)'

deps:
	opam install --yes --deps-only $(DEPS)  # Temporary workaround for Opam Windows bug
	opam install --yes $(DEPS)

exe:
	opam exec -- dune build $(MAIN).exe
	ln -f _build/default/$(MAIN).exe $(NAME).exe

clean:
	dune clean
	rm -rf $(NAME)

distclean: clean
	rm -rf _build
	rm -rf $(NAME).exe
