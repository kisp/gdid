;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:gdid)

(define-command (com-help :cli-name "help")
                ()
                ()
  (show-usage *error-output*)
  (terpri *error-output*)
  (write-string (usage-info "Options:" *options*)
                *error-output*))

(define-command (com-version :cli-name "version")
                ()
                ()
  (format t "gdid ~A~%"
          #.(asdf:component-version (asdf:find-system "gdid"))))

(define-command (com-new :cli-name "new")
                (&key (collection *default-collection*))
                (&rest content)
  (let ((collection (find-collection collection)))
    (princ
     (collection-new-item
      collection
      (when content
        (format nil "~{~A~^ ~}" content))))
    (terpri)))

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
  (let (items)
    (flet ((add (item)
             (push item items)))
      (let ((collection (find-collection collection)))
        (if multiple
            (map-collection #'add collection query)
            (collection-single! #'add collection query))))
    (generate-pdf (nreverse items) (or file *standard-output*))))

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
      (if multiple
          (map-collection #'edit-item collection query)
          (collection-single! #'edit-item collection query)))))
