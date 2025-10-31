NAME := $(shell grep "public_name" dune | sed 's/.*public_name//' | sed 's/[^a-zA-Z0-9]//g')
APPNAME := $(shell grep "let name =" app.ml | sed 's/[^"]*"//' | sed 's/"//')
VERSION := $(shell grep "let version =" app.ml | sed 's/[^"]*"//' | sed 's/"//')

ASSETS = $(glob assets/*)
OPAM_DEPS = dune confero directories raylib
WIN_DLLS = libwinpthread-1

default: deps exe

vars:
	@echo NAME = $(NAME)
	@echo APPNAME = $(APPNAME)
	@echo VERSION = $(VERSION)

deps:
	opam install $(OPAM_DEPS)

exe:
	dune build main.exe
	ln -f _build/default/main.exe $(NAME).exe


mac: exe $(ASSETS)
	mkdir -p $(NAME).app/Contents
	cp -rf platform/mac/* assets $(NAME).exe $(NAME).app/Contents
	chmod +x $(NAME).app/Contents/MacOS/run.sh

mac-debug: mac
	codesign -s - -v -f --entitlements platform/mac-debug/debug.plist $(NAME).exe

$(APPNAME)/%.dll:
	cp `which $(@F)` $@

win: dir $(WIN_DLLS:%=$(APPNAME)/%.dll)

linux: dir

dir: exe $(ASSETS)
	mkdir -p $(APPNAME)
	cp -rf $(NAME).exe assets $(APPNAME)

zip-mac: mac
	zip -r $(APPNAME)-$(VERSION)-mac.zip $(APPNAME).app

zip-win: win
	zip -r $(APPNAME)-$(VERSION)-win.zip $(APPNAME)
	rm -rf $(NAME)

zip-linux: linux
	zip -r $(APPNAME)-$(VERSION)-linux.zip $(APPNAME)
	rm -rf $(NAME)


clean:
	dune clean
	rm -rf $(NAME)

distclean: clean
	rm -rf *.exe *.zip *.app
