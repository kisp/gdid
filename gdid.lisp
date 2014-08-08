;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:gdid
  (:use :cl)
  (:export #:gdid))

(in-package #:gdid)

(defun gdid ()
  (format t "hello world~%"))
