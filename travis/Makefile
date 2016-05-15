qlot:
	ros -l .sbcl-disable-debugger.lisp  -s qlot  -e '(qlot:install)'
	ros -l .sbcl-disable-debugger.lisp  -s qlot -e '(push :standalone *features*)' -e '(qlot:quickload :gdid)'  -e '(gdid::dump)'
	./gdid --version
	mkdir bin
	mv gdid bin
	bundle exec cucumber

bundle:
	sbcl --version
