;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:gdid)

(defvar *default-collection* "GDID")

(defvar *commands* nil)

(defstruct command
  name cli-name args-tester)

(deftype command-designator () '(or symbol command))

(defun find-command (command-designator)
  (declare (command-designator command-designator))
  (typecase command-designator
    (symbol (find command-designator *commands* :key #'command-name))
    (t command-designator)))

(defun find-command-by-cli-name (cli-name)
  (find cli-name *commands* :key #'command-cli-name :test #'equal))

(defun %add-command (command)
  (setq *commands*
        (delete (command-name command) *commands* :key #'command-name))
  (push command *commands*))

(defgeneric funcall-command (command keyword-args args))

(defmethod funcall-command ((command command) keyword-args args)
  (funcall-command (command-name command) keyword-args args))

(defmethod funcall-command ((command symbol) keyword-args args)
  (funcall-command (symbol-function command) keyword-args args))

(defmethod funcall-command ((command function) keyword-args args)
  (apply (apply command keyword-args) args))

(defmacro define-command ((name &key cli-name) keyword-args args &body body)
  `(progn
     (%add-command
      (make-command
       :name ',name
       :cli-name ,cli-name
       :args-tester (lambda ,keyword-args
                      (declare (ignore ,@(lambda-list-parameters keyword-args)))
                      (lambda ,args
                        (declare (ignore ,@(lambda-list-parameters args)))
                        t))))
     (defun ,name ,keyword-args
       (lambda ,args
         ,@body))))

(defun command-accepts-args (command keyword-args args)
  (handler-case
      (funcall-command (command-args-tester command) keyword-args args)
    (error () nil)))

(define-condition quit (error)
  ((code :reader quit-code :initarg :code))
  (:report (lambda (condition stream)
             (format stream "quit with code ~D" (quit-code condition)))))

(defun quit (code)
  (error 'quit :code code))

(defun call-with-quit-handler (thunk)
  #-standalone
  (funcall thunk)
  #+standalone
  (handler-case
      (funcall thunk)
    (quit (c) (sb-ext:exit :code (quit-code c)))))

(defmacro with-quit-handler (&body body)
  `(call-with-quit-handler (lambda () ,@body)))

(define-condition args-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (if (simple-condition-format-control condition)
                 (apply #'format stream
                        (simple-condition-format-control condition)
                        (simple-condition-format-arguments condition))
                 (write-string "args-error w/o message" stream)))))

(defun make-args-error (&optional format-control &rest format-arguments)
  (make-condition 'args-error :format-control format-control
                              :format-arguments format-arguments))

(defun args-error (&optional format-control &rest format-arguments)
  (error (apply #'make-args-error format-control format-arguments)))

(defparameter *usage* "gdid [-c] new <content>...
gdid [-c] list [<query>]
gdid [-cm] pdf <query> [<file>]
gdid [-cm] edit <query>
gdid help | -h | --help
gdid --version")

(defun show-usage (stream)
  (write-line *usage* stream)
  (values))

(defun handle-args-error (args-error)
  (when (simple-condition-format-control args-error)
    (princ args-error *error-output*)
    (terpri *error-output*))
  (show-usage *error-output*))

(defparameter *options*
  (list (make-option '(#\c) '("collection")
                     (req-arg (lambda (x) `(:collection ,x))
                              "COLL")
                     "Collection [default: GDID]")
        (make-option '(#\m) '("multiple")
                     (no-arg (lambda () '(:multiple t)))
                     "Allow batch processing of multiple query matches")
        (make-option '(#\h) '("help")
                     (no-arg (lambda () '(:help t)))
                     "Show this help")
        (make-option nil '("version")
                     (no-arg (lambda () '(:version t)))
                     "Show version")))

(defun get-opt-with-help-and-version (ordering opt-descr args*)
  (multiple-value-bind (keyword-args rest-args errs)
      (get-opt-append ordering opt-descr args*)
    (cond
      ((and (getf keyword-args :help)
            (null errs))
       (values nil '("help") nil))
      ((and (getf keyword-args :version)
            (null errs))
       (values nil '("version") nil))
      (t (values keyword-args rest-args errs)))))

(defun parse-args (args)
  (cond
    ((null args) (args-error))
    (t
     (multiple-value-bind (keyword-args rest-args errs)
         (get-opt-with-help-and-version :permute *options* args)
       (cond
         (errs (args-error errs))
         ((null rest-args) (args-error))
         (t
          (destructuring-bind (cli-name &rest args) rest-args
            (let ((command (find-command-by-cli-name cli-name)))
              (cond
                ((not command)
                 (args-error "unknown command ~S" cli-name))
                ((not (command-accepts-args command keyword-args args))
                 (args-error "bad args for ~S" cli-name))
                (t
                 `(,command ,keyword-args ,args)))))))))))

(defun gdid (&optional (args (cdr sb-ext:*posix-argv*)))
  (with-quit-handler
    (handler-case
        (destructuring-bind (command keyword-args args)
            (parse-args args)
          (funcall-command command keyword-args args)
          (quit 0))
      (args-error (args-error)
        (handle-args-error args-error)
        (quit 1)))))

(defun dump ()
  (asdf:clear-configuration)
  (asdf/system-registry:clear-registered-systems)
  (sb-ext:save-lisp-and-die "gdid"
                          :toplevel #'gdid::gdid
                          :executable t
                          :save-runtime-options t
                          :compression nil))
