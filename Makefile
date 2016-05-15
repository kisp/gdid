DESTDIR=/usr/local/

default: bundle-build

qlot: qlot-build cucumber
bundle: bundle-build cucumber

qlot-build:
	ros --version
	ros config
	ros -l .sbcl-disable-debugger.lisp  -s qlot  -e '(qlot:install)'
	ros -l .sbcl-disable-debugger.lisp  -s qlot -e '(push :standalone *features*)' -e '(qlot:quickload :gdid)'  -e '(gdid::dump)'
	./gdid --version

bundle-build:
	sbcl --version
	tar xfj bundle-libs.tar.bz2
	sbcl --no-userinit --non-interactive --load .sbcl-disable-debugger.lisp --load bundle-libs/bundle.lisp --load gdid.asd --eval '(push :standalone *features*)' --eval '(setq *compile-print* nil)' --eval '(without-warnings (asdf:load-system :gdid))' --eval '(gdid::dump)'
	./gdid --version

cucumber:
	mkdir -p bin
	ln -sf ../gdid bin/gdid
	bundle exec cucumber

install:
	mkdir -p "$(DESTDIR)bin"
	install -m 755 gdid "$(DESTDIR)bin"
