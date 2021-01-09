;;;; package.lisp

(defpackage #:trivial-mmap
  (:use #:cl)
  (:export #:mmap-file
           #:munmap-file
           #:mmap-read-char
           #:mmap-read-byte
           #:mmapped-file-p
           #:mmapped-file-pointer
           #:mmapped-file-size
           #:mmapped-file-offset
           #:with-mmap-file))
