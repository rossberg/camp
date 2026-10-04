# Configuration
# Names are extracted from dune-project, src/dune, and src/app.ml

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
LINUX_INSTALLDIR = /usr/local


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

deps-try: opam
	opam install --yes --deps-only .

deps:
	make deps-try || \
	(echo Retrying after Opam update... && opam update && make deps-try)

upgrade:
	opam update
	opam upgrade --yes

exe:
	cd src && opam exec -- dune build $(MAIN).exe
	ln -f _build/default/src/$(MAIN).exe $(NAME).exe

opam: dune-project
	opam list --check --installed dune || opam install dune
	opam exec -- dune build "@opam" || opam exec -- dune promote

release: check-release zip


# Packaging

prerequisites: check deps exe $(ASSETS) $(SYSASSETS)

dir: prerequisites
	mkdir -p $(APPNAME)
	cp -f $(NAME).exe $(APPNAME)/$(APPNAME).exe
	cp -rf assets $(APPNAME)

subst/%: %
	cp -f $< subst.0
	sed "s|[$$]NAME|$(NAME)|g" subst.0 >subst.1
	sed "s|[$$]APPNAME|$(APPNAME)|g" subst.1 >subst.2
	sed "s|[$$]VERSION|$(VERSION)|g" subst.2 >subst.3
	sed "s|[$$]INSTALLDIR|$(LINUX_INSTALLDIR)|g" subst.3 >subst.4
	cp -f subst.4 $<
	rm subst.*


win: dir
	@if [ "$(WIN_DLLS)" != '' ]; then cp $(WIN_DLLS:%=`opam exec -- which %.dll`) $(APPNAME); fi

linux: dir
	cp -f platform/linux/* $(APPNAME)
	mv $(APPNAME)/app.desktop $(APPNAME)/$(NAME).desktop
	make subst/$(APPNAME)/$(NAME).desktop

mac: prerequisites
	osacompile -o _build/run.app platform/mac/run.scpt
	mkdir -p $(APPNAME).app/Contents/MacOS
	mkdir -p $(APPNAME).app/Contents/Resources
	cp -rf platform/mac/Contents $(APPNAME).app
	cp -rf $(NAME).exe $(APPNAME).app/Contents/$(APPNAME)
	cp -rf assets $(APPNAME).app/Contents
	cp -rf _build/run.app/Contents/MacOS/droplet $(APPNAME).app/Contents/MacOS/$(APPNAME)Launcher
	cp -rf _build/run.app/Contents/Resources/Scripts $(APPNAME).app/Contents/Resources
	make subst/$(APPNAME).app/Contents/Info.plist

mac-debug: mac
	codesign -s - -v -f --entitlements platform/mac-debug/debug.plist $(NAME).exe


# Installation

install:
	make install-$(SYSTEM)

install-win: win
	sudo cp -rf $(APPNAME) `cygpath -u "$(PROGRAMFILES)"`

install-linux: linux
	sudo cp -rf $(APPNAME) $(LINUX_INSTALLDIR)/$(NAME)
	sudo ln -sf $(LINUX_INSTALLDIR)/$(NAME)/$(APPNAME).exe /usr/local/bin/$(NAME)
	if [ $(XDG_CURRENT_DESKTOP) == "KDE" ]; then \
	  sudo cp -f $(APPNAME)/$(NAME).desktop ~/.local/share/applications/$(NAME).desktop; \
	fi

install-mac: mac
	cp -rf $(APPNAME).app /Applications


# Zipping

zip:
	make zip-$(SYSTEM)

zip-mac: mac
	zip -r $(APPNAME)-$(VERSION)-mac.zip $(APPNAME).app

zip-win: win
	zip -r $(APPNAME)-$(VERSION)-win.zip $(APPNAME)
	rm -rf $(NAME)

zip-linux: linux
	zip -r $(APPNAME)-$(VERSION)-linux.zip $(APPNAME)
	rm -rf $(NAME)


# Checks

check:
	@ [ "$(PROJECTNAME)" = "$(NAME)" ] || \
	  ! echo "dune-project: name mismatch, $(PROJECTNAME) vs $(NAME)"
	@ [ "$(PROJECTVERSION)" = "$(VERSION)" ] || [ "$(PROJECTVERSION)--" = "$(VERSION)" ] || \
	  ! echo "dune-project: version mismatch, $(PROJECTVERSION) vs $(VERSION)"
	@ grep -q -F "$(PROJECTVERSION)" $(CHANGES) || \
	  ! echo "$(CHANGES): missing entry for version $(PROJECTVERSION)"
	@ for PACKAGE in $(DEPS); do \
	  (echo " $(PROJECTDEPS) " | grep -q " $$PACKAGE[>=0-9.]* ") || \
	    ! echo "dune-project: missing dependency for package $$PACKAGE"; \
	done

check-release: check
	@ [ "$(PROJECTVERSION)" = "$(VERSION)" ] || \
	  ! echo "dune-project: release version mismatch, $(PROJECTVERSION) vs $(VERSION)"
	@ grep -q -F "$(PROJECTVERSION)" $(README) || \
	  ! echo "$(README): release version mismatch, $(PROJECTVERSION) expected"
	@ grep -q -E "$(PROJECTVERSION).+[0-9]{4}-[0-9]{2}-[0-9]{2}" $(CHANGES) || \
	  ! echo "$(CHANGES): missing date for release version $(PROJECTVERSION)"


# Clean-up

clean: deps
	opam exec -- dune clean
	rm -rf $(NAME) $(NAME).opam
	rm -rf Info.plist.*

distclean: clean
	rm -rf _build
	rm -rf *.exe *.zip *.app


# Dune file access

app-%:
	grep "let $* =" src/app.ml | sed 's/[^"]*"//' | sed 's/"//'

dune-%:
	grep "[(]$*" src/dune src/*/dune | sed 's/.*$*//' | sed 's/[^a-zA-Z0-9_. -]//g'

project-%:
	grep "[(]$*" dune-project | sed 's/.*$*//' | sed 's/[^a-zA-Z0-9_. -]//g'
