# Configuration

APPNAME = $(shell make -s app-name)
VERSION = $(shell make -s app-version)
NAME = $(shell make -s dune-public_name)

NONDEPS = unix audio_file [a-zA-Z0-9_]*[.][a-zA-Z0-9_.]*
DEPS = dune $(shell make -s dune-libraries $(NONDEPS:%=| sed 's/ %//g'))

ASSETS = $(glob assets/*)
WIN_DLLS = libwinpthread-1 libffi-6

ifeq ($(OS),Windows_NT)
  SYSTEM = win
else
  ifeq ($(shell uname -s),Darwin)
    SYSTEM = mac
  endif
  ifeq ($(shell uname -s),Linux)
    SYSTEM = linux
  endif
endif


# Main Targets

default: deps exe
	make $(SYSTEM)

vars:
	@echo 'NAME = $(NAME)'
	@echo 'APPNAME = $(APPNAME)'
	@echo 'VERSION = $(VERSION)'
	@echo 'SYSTEM = $(SYSTEM)'
	@echo 'DEPS = $(DEPS)'

deps:
  # Temporary hack until raylib-callbacks is on official Opam repo
	@if opam show raylib-callbacks | grep "^repository *default$$" >/dev/null; \
	then \
		opam repo remove opam-raylib-1.6.0 >/dev/null; \
	elif ! opam show raylib-callbacks >/dev/null; \
	then \
		opam repo add opam-raylib-1.6.0 file://`pwd`/opam-raylib-1.6.0; \
  fi
	opam install $(DEPS)

exe:
	cd src && dune build main.exe
	ln -f _build/default/src/main.exe $(NAME).exe


# Packaging

mac: deps exe $(ASSETS)
	mkdir -p $(APPNAME).app/Contents
	cp -rf platform/mac/* assets $(NAME).exe $(APPNAME).app/Contents
	chmod +x $(APPNAME).app/Contents/MacOS/run.sh

mac-debug: mac
	codesign -s - -v -f --entitlements platform/mac-debug/debug.plist $(NAME).exe

mac-install: mac
	cp -rf $(APPNAME).app /Applications

win: dir
	@if [ "$(WIN_DLLS)" != '' ]; then cp $(WIN_DLLS:%=`which %.dll`) $(APPNAME); fi

linux: dir

dir: exe $(ASSETS)
	mkdir -p $(APPNAME)
	cp -f $(NAME).exe $(APPNAME)/$(APPNAME).exe
	cp -rf assets $(APPNAME)


# Zips

zip-mac: mac
	zip -r $(APPNAME)-$(VERSION)-mac.zip $(APPNAME).app

zip-win: win
	zip -r $(APPNAME)-$(VERSION)-win.zip $(APPNAME)
	rm -rf $(NAME)

zip-linux: linux
	zip -r $(APPNAME)-$(VERSION)-linux.zip $(APPNAME)
	rm -rf $(NAME)

zip:
	make zip-$(SYSTEM)


# Clean-up

clean:
	dune clean
	rm -rf $(NAME)

distclean: clean
	rm -rf _build
	rm -rf *.exe *.zip *.app


# Dune file access

app-%:
	grep "let $* =" src/app.ml | sed 's/[^"]*"//' | sed 's/"//'

dune-%:
	grep "$*" */dune */*/dune | sed 's/.*$*//' | sed 's/[^a-zA-Z0-9_. -]//g'
