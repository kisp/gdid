;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:gdid)

(defun draw-bar-code (gdid-str)
  (pdf:with-saved-state
    ;;(pdf:translate -150 -400)
    ;;(pdf:rotate 90)
    ;;(pdf:translate 500 -300)
    ;;(pdf:rotate 5)
    (pdf::draw-bar-code128 gdid-str 300 100 :height 40 :width 120
                                            :start-stop-factor 0.2 :font-size 6 :show-string t)))
(defun draw-bar-code-and-label (gdid-str)
  (pdf:with-saved-state
    (pdf:translate 0 660)
    (draw-bar-code gdid-str)
    (pdf:in-text-mode
      (pdf:move-text 100 70)
      (pdf:set-font (pdf:get-font "Helvetica") 24)
      (pdf:draw-text gdid-str))))

(defun draw-content (item gdid-str*)
  (let* ((center-block
           (typeset:compile-text ()
             (with-input-from-string (in (item-content item))
               (typeset:paragraph (:font "Helvetica" :font-size 10)
                 (typeset:put-string gdid-str*))
               (loop for line = (read-line in nil nil)
                     while line
                     do (typeset:paragraph (:font "Helvetica" :font-size 10)
                          (if (zerop (length line))
                              (typeset:put-string ".")
                              (typeset:put-string line)))))
             ;; (typeset:verbatim (content item))
             )))
    (pdf:translate 50 600)
    (typeset::draw-block center-block 50 100 400 600)))

(defun draw-page (item)
  (let* ((collection (item-collection item))
         (gdid-str (format nil "~A ~A"
                           (collection-name collection)
                           (item-index-as-string item)))
         (gdid-str* (format nil "~A::~A"
                            (collection-name collection)
                            (item-index-as-string item))))
    (pdf:with-page ()
      (pdf:with-outline-level (gdid-str (pdf:register-page-reference))
        (draw-bar-code-and-label gdid-str)
        (draw-content item gdid-str*)))))

(defun generate-pdf (item-or-items target)
  (pdf:with-document ()
    (dolist (item (ensure-list item-or-items))
      (draw-page item))
    (pdf:write-document target)))
