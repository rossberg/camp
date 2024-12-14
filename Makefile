NAME = kamp
APPNAME = Kamp
VERSION = 0.0


default:
	dune build main.exe
	ln -f _build/default/main.exe $(NAME).exe


mac: $(NAME).exe
	mkdir -p $(NAME).app/Contents
	cp -rf mac/* assets $(NAME).exe $(NAME).app/Contents

dir: $(NAME).exe
	mkdir $(APPNAME)
	cp -rf $(NAME).exe assets $(APPNAME)

zip-mac: mac
	zip -r $(APPNAME)-$(VERSION)-mac.zip $(APPNAME).app

zip-win: dir
	cp  `which libwinpthread-1.dll` $(APPNAME)
	zip -r $(APPNAME)-$(VERSION)-win.zip $(APPNAME)
	rm -rf $(NAME)

zip-linux: dir
	zip -r $(APPNAME)-$(VERSION)-linux.zip $(APPNAME)
	rm -rf $(NAME)


clean:
	dune clean

distclean: clean
	rm -rf *.exe *.zip *.app
