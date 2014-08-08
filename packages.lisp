;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:gdid.model
  (:use :cl :alexandria)
  (:export
   #:*root-dir*
   #:find-collection
   #:map-collection
   #:collection-single!
   #:item-collection
   #:item-path
   #:do-collection
   #:item-index-as-string
   #:item-short-content
   #:item-content))

(defpackage #:gdid
  (:use :cl :hgetopt :gdid.model)
  (:export #:gdid))
