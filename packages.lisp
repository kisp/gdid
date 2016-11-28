;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:gdid.model
  (:use :cl :alexandria)
  (:export
   #:find-collection
   #:map-collection
   #:collection-single!
   #:item-collection
   #:item-path
   #:do-collection
   #:item-index-as-string
   #:item-short-content
   #:item-content
   #:list-collection
   #:item-index
   #:collection-max-index
   #:collection-new-item
   #:collection-name))

(defpackage #:gdid
  (:use :cl :hgetopt :gdid.model :alexandria)
  (:export #:gdid))
