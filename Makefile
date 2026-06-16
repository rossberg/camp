# Configuration

APPNAME = $(strip $(shell make -s app-name))
VERSION = $(strip $(shell make -s app-version))
NAME = $(strip $(shell make -s dune-public_name))
PROJECTNAME = $(strip $(shell make -s project-name))
PROJECTVERSION = $(strip $(shell make -s project-version))
PROJECTDEPS = dune $(strip $(shell grep "=" dune-project | sed 's/[() ]//g'))

MAIN = main
README = README.txt
CHANGES = CHANGES.txt

NONDEPS = unix audio_file [a-zA-Z0-9_]*[.][a-zA-Z0-9_.]*
DEPS = $(strip $(shell make -s dune-libraries $(NONDEPS:%=| sed 's/ %//g')))

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

ASSETS = $(wildcard assets/*)
SYSASSETS = $(wildcard platform/$(SYSTEM)/* platform/$(SYSTEM)/*/* platform/$(SYSTEM)/*/*/*)
WIN_DLLS = libwinpthread-1 libffi-6


# Main Targets

default:
	make $(SYSTEM)

vars:
	@echo 'NAME = $(NAME)'
	@echo 'APPNAME = $(APPNAME)'
	@echo 'VERSION = $(VERSION)'
	@echo 'PROJECTNAME = $(PROJECTNAME)'
	@echo 'PROJECTVERSION = $(PROJECTVERSION)'
	@echo 'PROJECTDEPS = $(PROJECTDEPS)'
	@echo 'SYSTEM = $(SYSTEM)'
	@echo 'MAIN = $(MAIN)'
	@echo 'DEPS = $(DEPS)'
	@echo 'ASSETS = $(ASSETS)'
	@echo 'SYSASSETS = $(SYSASSETS)'

deps: opam
	opam install --yes --deps-only $(PROJECTDEPS:%="%")  # Temporary workaround for Opam Windows bug
	opam install --yes $(PROJECTDEPS:%="%")

upgrade: opam
	opam update
	opam upgrade --yes

exe:
	cd src && opam exec -- dune build $(MAIN).exe
	ln -f _build/default/src/$(MAIN).exe $(NAME).exe

opam: dune-project
	dune build "@opam"

release: check-release zip


# Packaging

prerequisites: check deps exe $(ASSETS) $(SYSASSETS)

dir: prerequisites
	mkdir -p $(APPNAME)
	cp -f $(NAME).exe $(APPNAME)/$(APPNAME).exe
	cp -rf assets $(APPNAME)

win: dir
	@if [ "$(WIN_DLLS)" != '' ]; then cp $(WIN_DLLS:%=`opam exec -- which %.dll`) $(APPNAME); fi

linux: dir

mac: prerequisites
	osacompile -o _build/run.app platform/mac/run.scpt
	mkdir -p $(APPNAME).app/Contents/MacOS
	mkdir -p $(APPNAME).app/Contents/Resources
	cp -rf platform/mac/Contents $(APPNAME).app
	cp -rf $(NAME).exe $(APPNAME).app/Contents/$(APPNAME)
	cp -rf assets $(APPNAME).app/Contents
	cp -rf _build/run.app/Contents/MacOS/droplet $(APPNAME).app/Contents/MacOS/$(APPNAME)Launcher
	cp -rf _build/run.app/Contents/Resources/Scripts $(APPNAME).app/Contents/Resources
	cp $(APPNAME).app/Contents/Info.plist Info.plist.0
	sed "s/[$$]APPNAME/$(APPNAME)/g" Info.plist.0 >Info.plist.1
	sed "s/[$$]VERSION/$(VERSION)/g" Info.plist.1 >Info.plist.2
	sed "s/[$$]NAME/$(NAME)/g" Info.plist.2 >Info.plist.3
	mv -f Info.plist.3 $(APPNAME).app/Contents/Info.plist
	rm Info.plist.*

mac-debug: mac
	codesign -s - -v -f --entitlements platform/mac-debug/debug.plist $(NAME).exe

mac-install: mac
	cp -rf $(APPNAME).app /Applications


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


# Checks

check:
	@ [ "$(PROJECTNAME)" = "$(NAME)" ] || \
	  ! echo "dune-project: name mismatch, $(PROJECTNAME) vs $(NAME)"
	@ [ "$(PROJECTVERSION)" = "$(VERSION)" ] || [ "$(PROJECTVERSION)--" == "$(VERSION)" ] || \
	  ! echo "dune-project: version mismatch, $(PROJECTVERSION) vs $(VERSION)"
	@ grep -q -F "$(PROJECTVERSION)" $(CHANGES) || \
	  ! echo "$(CHANGES): missing entry for version $(PROJECTVERSION)"
	@ for PACKAGE in $(DEPS); do \
	  (echo " $(PROJECTDEPS) " | grep -q " $$PACKAGE[>=0-9.]* ") || \
	    ! echo "dune-project: missing dependency for package $$PACKAGE"; \
	done

check-release: check
	@ [ "$(PROJECTVERSION)" = "$(VERSION)" ] || \
	  ! echo "dune-project: version mismatch, $(PROJECTVERSION) vs $(VERSION)"
	@ grep -q -F "$(PROJECTVERSION)" $(README) || \
	  ! echo "$(README): version mismatch, $(PROJECTVERSION) expected"
	@ grep -q -F "$(PROJECTVERSION).+[0-9]+[.][0-9]+[.][0-9]+" $(CHANGES) || \
	  ! echo "$(CHANGES): missing date for version $(PROJECTVERSION)"


# Clean-up

clean:
	dune clean
	rm -rf $(NAME)
	rm -rf Info.plist.*

distclean: clean
	rm -rf _build $(NAME).opam
	rm -rf *.exe *.zip *.app


# Dune file access

app-%:
	grep "let $* =" src/app.ml | sed 's/[^"]*"//' | sed 's/"//'

dune-%:
	grep "[(]$*" src/dune src/*/dune | sed 's/.*$*//' | sed 's/[^a-zA-Z0-9_. -]//g'

project-%:
	grep "[(]$*" dune-project | sed 's/.*$*//' | sed 's/[^a-zA-Z0-9_. -]//g'
