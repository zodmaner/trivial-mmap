# Trivial-MMAP

Trivial-MMAP is a Common Lisp library that aims to provide an
easy-to-use API for working with memory-mapped files.

The library exports the following symbols:

* `mmap-file` a function that maps a file into memory
* `munmap-file` a function that removes a memory-mapped file from
  memory
* `mmap-read-char` a helper function that facilitates reading a
  character from a memory-mapped file
* `mmap-read-byte` a helper function for reading one byte from a
  memory-mapped file
* `with-mmap-file` a helper macro that ensures that a memory-mapped
  file is safely unmapped (using `munmap-file` function) after we are
  done with it

Trivial-MMAP is based on code snippets in a [blog post](https://web.archive.org/web/20120531022645/http://wandrian.net/2012-04-07-1352-mmap-files-in-lisp.html) by Nicolas Martyanoff.

## Getting Started

The following code snippet shows how to use Trivial-MMAP's
`with-mmap-file` and `mmap-read-char` to read one character at a time
from a file containing Twitter social graph (780 MB uncompressed).

````lisp
CL-USER> (time
          (trivial-mmap:with-mmap-file (ptr size "/home/zodmaner/twitter_rv_15066953.net")
            (loop
               :for offset :from 0 :to (1- size) :do
               (characterp (trivial-mmap:mmap-read-char ptr offset)))))
Evaluation took:
  3.400 seconds of real time
  3.400000 seconds of total run time (3.360000 user, 0.040000 system)
  100.00% CPU
  8,479,536,815 processor cycles
  65,952 bytes consed
````

The little toy benchmark above was run on my laptop (an i5 ThinkPad
X230 (Ivy Bridge) with 8 GB of RAM and an SSD). For comparison,
reading the same file using `with-open-file` and `read-char` takes
around 14 seconds of real time on the same machine.

## Dependency

* **osicat**

## Limitations

* Currently, the library only supports reading data from memory-mapped files.

## Author & Maintainer

Smith Dhumbumroong (<zodmaner@gmail.com>)

## License

Public Domain
