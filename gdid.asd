;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf)

(defsystem :gdid
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :description "gdid. cli version"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :licence "BSD-style"
  :depends-on (:alexandria :hgetopt)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "gdid")
               (:file "commands")))

(defmethod perform ((op test-op)
                    (system (eql (find-system :gdid))))
  (oos 'load-op :gdid-test)
  (funcall (intern "RUN!" "MYAM") :gdid-test))
