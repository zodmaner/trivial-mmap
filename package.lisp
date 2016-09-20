;;;; package.lisp

(defpackage #:trivial-mmap
  (:use #:cl)
  (:export #:mmap-file
           #:munmap-file
           #:mmap-read-char
           #:with-mmap-file))

