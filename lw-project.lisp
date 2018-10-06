(defpackage :lw-project
  (:use :cl)
  (:export :make-project))
(in-package :lw-project)

(defparameter *common-lisp-directory* (merge-pathnames "common-lisp/" (user-homedir-pathname)))
(defparameter *template-directory* (asdf:system-relative-pathname :lw-project "template/"))

(defvar *project-directory*)
(defvar *project-name*)

(define-condition lw-project-condition (simple-condition) ())

(define-condition tag-error (lw-project-condition)
  ((name :initarg :name :reader tag-error-name))
  (:report (lambda (condition stream)
             (format stream "~S does not exist." (tag-error-name condition)))))

(defun make-project (*project-name* &key
                                    ((:template-directory *template-directory*)
                                     *template-directory*))
  (let ((*project-directory* (get-project-directory *project-name*)))
    (ensure-directories-exist *project-directory*)
    (copy-template-directory *template-directory*)))

(defun get-project-directory (name)
  (make-pathname :directory (append (pathname-directory *common-lisp-directory*)
                                    (list name))))

(defun copy-template-directory (pathname)
  (dolist (pathname (directory (uiop:ensure-directory-pathname pathname)))
    (if (uiop:directory-pathname-p pathname)
        (copy-template-directory pathname)
        (copy-template-file pathname))))

(defun copy-template-file (pathname)
  (let* ((template-text (file-string pathname))
         (new-text (expand-template template-text :name *project-name*))
         (dst (template-to-project-pathname pathname)))
    (ensure-directories-exist dst)
    (write-file new-text dst)))

(defun template-to-project-pathname (pathname)
  (let ((relative-path (enough-namestring pathname *template-directory*)))
    (merge-pathnames (if (equal "template" (pathname-name relative-path))
                         (make-pathname :name *project-name*
                                        :type (pathname-type relative-path)
                                        :defaults relative-path)
                         relative-path)
                     *project-directory*)))

(defun expand-template (text &rest tags)
  (with-output-to-string (stream)
    (loop :for start := 0 :then (1+ pos2)
          :for pos1 := (position #\{ text :start start)
          :for pos2 := (when pos1 (position #\} text :start pos1))
          :do (write-string text stream :start start :end pos1)
          :while pos1
          :do (let* ((tag (subseq text (1+ pos1) pos2))
                     (value (find-tag tag tags)))
                (write-string value stream)))))

(defun find-tag (tag tags &optional errorp)
  (loop :for (key value) :on tags :by #'cddr
        :when (string-equal tag key)
        :do (return value)
        :finally (when errorp (error 'tag-error :name tag))))

(defun write-file (string pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :error
                          :if-does-not-exist :create)
    (write-string string stream)))

#+lispworks
(editor:defcommand "Make Project" (p) "" ""
  (declare (ignore p))
  (let ((name (editor:prompt-for-string :prompt "Project Name: ")))
    (when (and name (plusp (length name)))
      (make-project name))))
