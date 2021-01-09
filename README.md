# Trivial-MMAP

Trivial-MMAP is a Common Lisp library that aims to provide a
high-level, easy-to-use API for working with memory-mapped files.

The library exports the following symbols:

* `mmap-file` a function that maps a file into memory and returns an
  object containing a pointer to the memory-mapped file along with
  other meta data;
* `munmap-file` a function that removes a memory-mapped file from
  memory;
* `mmap-read-char` a helper function that facilitates reading a
  character from a memory-mapped file;
* `mmap-read-byte` a helper function for reading one byte from a
  memory-mapped file;
* `with-mmap-file` a helper macro that ensures that a memory-mapped
  file is safely unmapped (using `munmap-file` function) after we are
  done with it.

In additions, the following are the exported symbols of the predicate
and accessor functions for dealing with the memory-mapped file object
returned by the `mmap-file` function:

* `mmapped-file-p` a predicate function that returns true if a
  variable is a memory-mapped file object (and false otherwise);
* `mmapped-file-pointer` an accessor function that, given a
  memory-mapped file object, returns a raw pointer address of the file;
* `mmapped-file-size` an accessor function that, given a memory-mapped
  file object, returns the size of the file;
* `mmapped-file-offset` an accessor function that, given a
  memory-mapped file object, returns the offset into the file.

Trivial-MMAP is based on code snippets in a [blog post](https://web.archive.org/web/20120531022645/http://wandrian.net/2012-04-07-1352-mmap-files-in-lisp.html) by Nicolas Martyanoff.

## Getting Started

The following code snippet shows how to use Trivial-MMAP's
`with-mmap-file` and `mmap-read-char` to read one character at a time
from a file containing Twitter social graph (780 MB uncompressed).

````lisp
CL-USER> (time
          (trivial-mmap:with-mmap-file (mmapped-file "/home/zodmaner/twitter_rv_15066953.net")
                    (loop while (< (trivial-mmap:mmapped-file-offset mmapped-file)
                                   (trivial-mmap:mmapped-file-size mmapped-file)) do
                       (assert (characterp (trivial-mmap:mmap-read-char mmapped-file))))))
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

Of course, you could avoid the `with-mmap-file` macro if you want to.
The following code snippet is equivalent to the above, but only use
the functions provided by Trivial-MMAP:

````lisp
(time
 (let ((mmapped-file (trivial-mmap:mmap-file "/home/zodmaner/twitter_rv_15066953.net")))
  (unwind-protect
       (loop while (< (trivial-mmap:mmapped-file-offset mmapped-file)
                      (trivial-mmap:mmapped-file-size mmapped-file)) do
         (assert (characterp (trivial-mmap:mmap-read-char mmapped-file))))
    (trivial-mmap:munmap-file mmapped-file))))
````

Although from my experiences the version that uses the
`with-mmap-file` macro is usually around 10% to 15% faster compared to
the version that did not use the macro.

## Dependencies

* [Alexandria](https://common-lisp.net/project/alexandria/)
* [Osicat](https://common-lisp.net/project/osicat/)

## Limitations

Of course, this library is far from being completed. Here is the list
of some of the current limitations/missing features:

* Currently the library only supports reading data from memory-mapped files.
* The library only supports Linux. It may work on MacOS, but definitely not on Windows.

As always, patches and bug reports are more then welcome!

## Author & Maintainer

Smith Dhumbumroong (<zodmaner@gmail.com>)

## License

Public Domain
