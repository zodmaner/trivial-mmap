;;;; trivial-mmap.lisp

(in-package #:trivial-mmap)

;;; "trivial-mmap" goes here. Hacks and glory await!

(declaim (optimize (speed 3))
         (inline mmap-read-byte mmap-read-char))

(defstruct mmapped-file
  (pointer (cffi:null-pointer))
  (size 0 :type fixnum)
  (offset 0 :type fixnum))

(defun mmap-file (filename)
  (multiple-value-bind (pointer file-size) (%mmap-file filename)
    (make-mmapped-file :pointer pointer :size file-size)))

(defun munmap-file (mmapped-file)
  (with-accessors ((pointer mmapped-file-pointer) (size mmapped-file-size)) mmapped-file
    (%munmap-file pointer size)))

(defun mmap-read-byte (mmapped-file &optional eof-value)
  (with-accessors ((pointer mmapped-file-pointer)
                   (offset mmapped-file-offset)
                   (file-size mmapped-file-size)) mmapped-file
    (if (< offset file-size)
        (prog1
          (%mmap-read-byte pointer offset)
          (incf offset))
        eof-value)))

(defun mmap-read-char (mmapped-file &optional eof-value)
  (with-accessors ((pointer mmapped-file-pointer)
                   (offset mmapped-file-offset)
                   (file-size mmapped-file-size)) mmapped-file
    (if (< offset file-size)
        (prog1
          (%mmap-read-char pointer offset)
          (incf offset))
        eof-value)))

(defmacro with-mmap-file ((mmapped-file filename) &body body)
  `(let ((,mmapped-file ,(mmap-file filename)))
     (unwind-protect
          (progn ,@body)
       (munmap-file ,mmapped-file))))
