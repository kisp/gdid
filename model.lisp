;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:gdid.model)

(defun root-dir ()
  (merge-pathnames #p".emacs.d/var/muse/"
                   (user-homedir-pathname)))

(defun relative-directory (name)
  (make-pathname :directory `(:relative ,name)))

(defun wild-name-of-type (type)
  (make-pathname :name :wild :type type))

(defun directory-last-component (pathname)
  (car (last (pathname-directory pathname))))

;;; utils
(defmacro with-slot-cache ((instance slot-name) &body body)
  `(or (slot-value ,instance ',slot-name)
       (setf (slot-value ,instance ',slot-name)
             (progn ,@body))))

;;; item
(defclass item ()
  ((collection     :reader   item-collection :initarg :collection)
   (path           :reader   item-path       :initarg :path)
   (%content       :initform nil)
   (%short-content :initform nil)))

(defun make-item (collection path)
  (make-instance 'item :collection collection :path path))

(defmethod print-object ((item item) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~A ~S"
            (collection-name (item-collection item))
            (enough-namestring (item-path item)
                               (collection-root-dir (item-collection item))))))

(defmethod item-index-as-string ((item item))
  (pathname-name (item-path item)))

(defmethod item-short-content ((item item))
  (with-slot-cache (item %short-content)
    (let ((content (item-content item)))
      (nsubstitute #\space #\newline
                   (subseq content
                           0
                           (min 60 (length content)))))))

(defmethod item-content ((item item))
  (with-slot-cache (item %content)
    (alexandria:read-file-into-string (item-path item) :external-format :utf-8)))

(defmethod item-matched ((item item) matcher)
  (or (ppcre:scan matcher (item-index-as-string item))
      (ppcre:scan matcher (item-content item))))

(defun make-matcher (query)
  (ppcre:create-scanner query :case-insensitive-mode t))

;;; collection
(defun find-collection (name &optional (errorp t))
  (let ((*default-pathname-defaults* (root-dir)))
    (or (probe-file (relative-directory (string-upcase name)))
        (probe-file (relative-directory (string-downcase name)))
        (find name (directory (relative-directory :wild))
              :key #'directory-last-component
              :test #'string-equal)
        (when errorp
          (error "no collection of name ~S" name)))))

(defun collection-name (collection)
  (directory-last-component collection))

(defun collection-root-dir (collection)
  collection)

(defun map-collection (function collection &optional query)
  (let ((*default-pathname-defaults* (collection-root-dir collection)))
    (labels ((list-muse ()
               (directory (wild-name-of-type "muse"))))
      (cond
        (query
         (let ((matcher (make-matcher query)))
           (dolist (path (list-muse))
             (let ((item (make-item collection path)))
               (when (item-matched item matcher)
                 (funcall function item))))))
        (t
         (dolist (path (list-muse))
           (funcall function (make-item collection path))))))))

(defmacro do-collection ((var collection &optional query) &body body)
  `(map-collection (lambda (,var) ,@body) ,collection ,query))

(defun collection-single! (function collection query)
  "For no result, function is not called and collection-single! returns.

For exactly one result, function is called with result.

For more than one result, an error is signaled and function is not called."
  (flet ((error-handler ()
           (error "query ~S on collection ~S returned more than a single match"
                  query (collection-name collection))))
    (let* ((single-result)
           (wrapper (lambda (x)
                      (if single-result
                          (error-handler)
                          (setq single-result x)))))
      (map-collection wrapper collection query)
      (when single-result
        (funcall function single-result)))))
