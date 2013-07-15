charseq
==================
This package provides **charseq** structure which represents an efficient character sequence.  
**charseq** is a wrapper of common-lisp standard string and it has following features:

* Creating substring is very efficient.
  * When creating a substring, **charseq** shares the source string instance and maintains start and end position of new string. Hence it is a constant time (and size) operation.
* It is ensured that **charseq** instance always holds a _(simple-array character *)_ string internally.

