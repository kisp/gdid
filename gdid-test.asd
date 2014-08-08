;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf)

(defsystem :gdid-test
  :name "gdid-test"
  :description "Tests for gdid"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :depends-on (:gdid :myam :alexandria))

(defmethod perform ((op test-op)
                    (system (eql (find-system :gdid-test))))
  (perform op (find-system :gdid)))
