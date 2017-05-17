;;;; trivial-mmap.asd

(asdf:defsystem #:trivial-mmap
  :description "A library providing an easy-to-use API for working with memory-mapped files."
  :author "Smith Dhumbumroong <zodmaner@gmail.com>"
  :license "Public Domain"
  :depends-on (#:alexandria
               #:osicat)
  :serial t
  :components ((:file "package")
               (:file "trivial-mmap")))

