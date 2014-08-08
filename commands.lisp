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
  (let ((collection (find-collection collection)))
    (format t "NOT IMPL this would perform new on ~A and ~A~%"
            collection content)))

(define-command (com-list :cli-name "list")
                (&key (collection *default-collection*))
                (&optional query)
  (let ((collection (find-collection collection)))
    (do-collection (item collection query)
      (format t "~@A~12T~A~%"
              (item-index-as-string item)
              (item-short-content item)))))

(define-command (com-pdf :cli-name "pdf")
                (&key (collection *default-collection*)
                      multiple)
                (query &optional file)
  (format t "NOT IMPL this would perform pdf ~S~%"
          (list collection multiple query file)))

(define-command (com-edit :cli-name "edit")
                (&key (collection *default-collection*)
                      multiple)
                (query)
  (flet ((edit-item (item)
           (sb-ext:run-program (sb-ext:posix-getenv "EDITOR")
                               (list (namestring (item-path item)))
                               :search t
                               :input t :output t :error t)))
    (let ((collection (find-collection collection)))
      (collection-single! #'edit-item collection query))))
