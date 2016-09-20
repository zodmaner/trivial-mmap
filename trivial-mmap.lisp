;;;; trivial-mmap.lisp

(in-package #:trivial-mmap)

;;; "trivial-mmap" goes here. Hacks and glory await!

(declaim (optimize speed)
         (inline mmap-read-char mmap-read-byte))

(defun mmap-file (filename &key length (mapping-type :map-private) (offset 0))
  "Maps a FILENAME into memory."
  (let* ((open-flag osicat-posix:o-rdonly)
         (memory-protection osicat-posix:prot-read)
         (mapping-type-flag (case mapping-type
                              (:map-private osicat-posix:map-private)
                              (:map-shared osicat-posix:map-shared)))
         (fd (osicat-posix:open filename open-flag)))
    (unwind-protect
         (let* ((file-size (if length length (osicat-posix:stat-size (osicat-posix:fstat fd))))
                (pointer-to-mmap-file (osicat-posix:mmap (cffi:null-pointer)
                                                         file-size
                                                         memory-protection
                                                         mapping-type-flag
                                                         fd
                                                         offset)))
           (values pointer-to-mmap-file file-size))
      (osicat-posix:close fd))))

(defun munmap-file (pointer-to-mmap-file file-size)
  "Removes a mapping at the address (with the range of FILE-SIZE) that
the POINTER-TO-MMAP-FILE pointer points to from memory."
  (osicat-posix:munmap pointer-to-mmap-file file-size))

(defun mmap-read-char (pointer-to-mmap-file offset)
  "Reads and returns a character from a memory-mapped file pointed to
by the POINTER-TO-MMAP-FILE pointer, offset by OFFSET bytes."
  (code-char (cffi:mem-aref (cffi:inc-pointer pointer-to-mmap-file offset) :char)))

(defun mmap-read-byte (pointer-to-mmap-file offset)
  "Reads and returns one byte from a memory-mapped file pointed to by
the POINTER-TO-MMAP-FILE pointer, offset by OFFSET bytes."
  (cffi:mem-aref (cffi:inc-pointer pointer-to-mmap-file offset) :uint8))

(defmacro with-mmap-file ((pointer-to-mmap-file
                           file-size filename &key length (mapping-type :map-private) (offset 0))
                          &body body)
  "Uses MMAP-FILE to maps a FILENAME into memory.

POINTER-TO-MMAP-FILE and FILE-SIZE are bound to values that MMAP-FILE
returns after we apply it to FILENAME."
  (let ((original-ptr (gensym "POINTER-"))
        (original-size (gensym "SIZE-")))
    `(multiple-value-bind (,pointer-to-mmap-file ,file-size)
         (mmap-file ,filename :length ,length :mapping-type ,mapping-type :offset ,offset)
       (let ((,original-ptr ,pointer-to-mmap-file)
             (,original-size ,file-size))
         (unwind-protect
              (progn ,@body)
           (munmap-file ,original-ptr ,original-size))))))
