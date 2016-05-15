qlot:
	ros --version
	ros config
	ros -l .sbcl-disable-debugger.lisp  -s qlot  -e '(qlot:install)'
	ros -l .sbcl-disable-debugger.lisp  -s qlot -e '(push :standalone *features*)' -e '(qlot:quickload :gdid)'  -e '(gdid::dump)'
	./gdid --version
	mkdir bin
	mv gdid bin
	bundle exec cucumber

bundle:
	sbcl --version
	tar xfj bundle-libs.tar.bz2
	sbcl --no-userinit --non-interactive --load .sbcl-disable-debugger.lisp --load bundle-libs/bundle.lisp --load gdid.asd --eval '(push :standalone *features*)' --eval '(asdf:load-system :gdid)' --eval '(gdid::dump)'
	mkdir -p bin
	mv gdid bin
	bundle exec cucumber
