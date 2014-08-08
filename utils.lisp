;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:gdid)

(defun lambda-list-parameters (lambda-list)
  (multiple-value-bind (required optional rest keys
                        allow-other-keys aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys keyp))
    (append required (mapcar #'first optional)
            (when rest (list rest))
            (mapcar #'cadar keys)
            (mapcar #'first aux))))

(defun get-opt-append (ordering opt-descr args*)
  (multiple-value-bind (opts rest-args errs)
      (get-opt ordering opt-descr args*)
    (values (apply #'append opts)
            rest-args
            (when errs
              (string-right-trim '(#\newline)
                                 (apply #'concatenate 'string errs))))))
