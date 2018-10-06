(defpackage :lw-project
  (:add-use-defaults t)
  (:export :*project-directory* :*project-name* :make-project))
(in-package :lw-project)

(defparameter *common-lisp-directory* (merge-pathnames "common-lisp/" (user-homedir-pathname)))
(defparameter *template-directory* (asdf:system-relative-pathname :lw-project "template/"))

(defvar *script-pathname*)
(defvar *project-directory*)
(defvar *project-name*)

(define-condition lw-project-condition (simple-condition) ())

(define-condition tag-error (lw-project-condition)
  ((name :initarg :name :reader tag-error-name)
   (pathname :initarg :pathname :reader tag-error-pathname)
   (start :initarg :start :reader tag-error-start)
   (end :initarg :end :reader tag-error-end))
  (:report (lambda (condition stream)
             (format stream "~s does not exist." (tag-error-name condition)))))

(defun make-project (*project-name* &key
                                    ((:template-directory *template-directory*)
                                     *template-directory*))
  (let ((*project-directory* (get-project-directory *project-name*))
        (*script-pathname* (merge-pathnames "_lw_project.lisp" *template-directory*)))
    (ensure-directories-exist *project-directory*)
    (copy-template-directory *template-directory*)
    (load-script-file *script-pathname*)))

(defun load-script-file (script-pathname)
  (when-let (script-file (probe-file script-pathname))
    (let ((script-text (expand-template script-file (list :name *project-name*))))
      (with-input-from-string (in script-text)
        (let ((*package* (find-package :cl-user))
              (eof '#:eof))
          (loop :for form := (read in nil eof)
                :until (eq form eof)
                :do (eval form)))))))

(defun get-project-directory (name)
  (make-pathname :directory (append (pathname-directory *common-lisp-directory*)
                                    (list name))))

(defun copy-template-directory (pathname)
  (dolist (pathname (directory (uiop:ensure-directory-pathname pathname)))
    (if (uiop:directory-pathname-p pathname)
        (copy-template-directory pathname)
        (copy-template-file pathname))))

(defun copy-template-file (pathname)
  (unless (uiop:pathname-equal pathname *script-pathname*)
    (let ((new-text (expand-template pathname (list :name *project-name*)))
          (dst (template-to-project-pathname pathname)))
      (ensure-directories-exist dst)
      (write-to-file dst new-text))))

(defun template-to-project-pathname (pathname)
  (let ((relative-path (enough-namestring pathname *template-directory*)))
    (merge-pathnames (if (equal "template" (pathname-name relative-path))
                         (make-pathname :name *project-name*
                                        :type (pathname-type relative-path)
                                        :defaults relative-path)
                         relative-path)
                     *project-directory*)))

(defun read-tag ()
  (format t "alternative tag: ")
  (force-output)
  (trim-spaces (read-line)))

(defun expand-template (pathname tags)
  (loop
   (let (tag-error)
     (restart-case (handler-bind ((tag-error (lambda (c) (setq tag-error c))))
                     (return-from expand-template (expand-template-1 pathname tags)))
       (fix-error-position (new-tag)
         :interactive (lambda () (list (read-tag)))
         (edit-on-error-position tag-error new-tag))))))

(defun expand-template-1 (pathname tags)
  (let ((text (file-string pathname)))
    (with-output-to-string (stream)
      (loop :for start := 0 :then (1+ pos2)
            :for pos1 := (position #\{ text :start start)
            :for pos2 := (when pos1 (position #\} text :start pos1))
            :do (write-string text stream :start start :end pos1)
            :while pos1
            :do (let* ((tag (subseq text (1+ pos1) pos2))
                       (value (find-tag tag tags)))
                  (unless value (error 'tag-error
                                       :name tag
                                       :pathname pathname
                                       :start pos1
                                       :end pos2))
                  (write-string value stream))))))

(defun edit-on-error-position (tag-error new-tag)
  (with-slots (start end pathname) tag-error
    (let* ((buffer (editor:make-buffer (namestring pathname) :temporary t))
           (point (editor:buffer-point buffer)))
      (editor:insert-string point (file-string pathname))
      (editor:buffer-start point)
      (editor:character-offset point (1+ start))
      (editor::delete-characters point (- end start 1))
      (editor:insert-string point new-tag)
      (editor:write-region-to-file (editor:buffers-start buffer)
                                   (editor:buffers-end buffer)
                                   pathname))))

(defun find-tag (tag tags)
  (loop :for (key value) :on tags :by #'cddr
        :when (string-equal tag key)
        :do (return value)))

(defun trim-spaces (string)
  (string-trim '(#\space #\newline) string))

(defun write-to-file (pathname string)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :error
                          :if-does-not-exist :create)
    (write-string string stream)))

(editor:defcommand "Make Project" (p) "" ""
  (declare (ignore p))
  (let ((name (editor:prompt-for-string :prompt "Project Name: ")))
    (when (and name (plusp (length name)))
      (make-project name))))
