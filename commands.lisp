;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:gdid)

(define-command (com-help :cli-name "help")
                ()
                ()
  (show-usage *error-output*)
  (terpri *error-output*)
  (write-string (usage-info "Options:" *options*)
                *error-output*))

(define-command (com-new :cli-name "new")
                (&key (collection *default-collection*))
                (&rest content)
  (format t "this would perform new on ~A and ~A~%"
          collection content))

(define-command (com-list :cli-name "list")
                (&key (collection *default-collection*))
                (&optional query)
  (format t "this would perform list on ~A and ~A~%"
          collection query))

(define-command (com-pdf :cli-name "pdf")
                (&key (collection *default-collection*)
                      multiple)
                (query &optional file)
  (format t "this would perform pdf ~S~%"
          (list collection multiple query file)))

(define-command (com-edit :cli-name "edit")
                (&key (collection *default-collection*)
                      multiple)
                (query)
  (format t "this would perform edit ~S~%"
          (list collection multiple query)))
