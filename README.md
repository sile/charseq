charseq
====================
This package provides **charseq** structure which represents an efficient character sequence.
**charseq** is a wrapper of common-lisp standard string and it has following features:

* Creating substring is very efficient.
  * When creating a substring, **charseq** shares the source string instance and maintains start and end position of new string. Hence it is a constant time (and size) operation.
* It is ensured that **charseq** instance always holds a _(simple-array character *)_ string as internal representation.
  * _(simple-array character *)_ is one of the most efficient type that represents strings.


Version
---------------
0.1.9


Compatibility
---------------
**chraseq** has no implementation-depended code, but it is highly optimized for SBCL(later 1.0.37) only.


Installation
---------------
Use asdf-install.

```lisp
* (require :asdf)
* (require :asdf-install)
* (asdf-install:install :charseq)
```


API
---------------

### [Types]
--------------------------------------------------------------------------------
#### charseq
A charseq instance.

#### index
The range of available string index.
`(integer 0 #.array-total-size-limit)`


### [Conditions]
--------------------------------------------------------------------------------
#### invalid-index-error
Indicates the specified index is negative or exceeding target string size.

#### bounding-indices-bad-error
Indicates the specified indices (start and end position) are not within range of target string.


### [Functions]
--------------------------------------------------------------------------------
#### (make string &key start end) => charseq
Creates new charseq instance from _string_.

| name    | type            | default value   | description                 |
|:-------:|:---------------:|----------------:|:----------------------------|
| string  | string          |                 | Input source string. <br /> If this type is _(simple-array character *)_, new charseq instance shares it. Otherwise charseq coerce input string to that's type and new instance  holds it. |
| start   | charseq:index   |               0 | Start position of _charseq_ |
| end     | charseq:index   | (length string) | End position of _charseq_   |
| charseq | charseq:charseq |                 | Created charseq instance    |

Example:
```lisp
* (charseq:make "common lisp")
#S(CHARSEQ:CHARSEQ :STR "common lisp" :BEG 0 :END 11)

* (charseq:make "common lisp" :start 1 :end 6)
#S(CHARSEQ:CHARSEQ :STR "common lisp" :BEG 1 :END 6)

* (charseq:to-string *)
"ommon"

* (charseq:= (charseq:make "common lisp" :start 1 :end 6)
             (charseq:make (subseq "common lisp" 1 6)))
T
```

--------------------------------------------------------------------------------
#### (ref charseq index) => character
Accesses the character of _charseq_ specified by _index_.

| name      | type            | description             |
|:---------:|:---------------:|:------------------------|
| charseq   | charseq:charseq | Charseq instance        |
| index     | charseq:index   | An index for the charseq |
| character | character       | Referred character      |

Example:
```lisp
* (charseq:ref (charseq:make "common lisp") 2)
#\m

* (charseq:ref (charseq:make "common lisp" :start 6) 2)
#\i
```

--------------------------------------------------------------------------------
#### (length charseq) => length
Returns the length of _charseq_.

| name    | type                   | description             |
|:-------:|:----------------------:|:------------------------|
| charseq | charseq:charseq        | Charseq instance        |
| length  | integer                | the length of _charseq_ |

Example:
```lisp
* (charseq:length (charseq:make "common lisp" :start 2 :end 5))
3

* (charseq:to-string (charseq:make "common lisp" :start 2 :end 5))
"mmo"
```

--------------------------------------------------------------------------------
#### (sub charseq start &optional end) => sub-charseq
Creates a charseq instance that is a subpart of _charseq_ bounded by _start_ and _end_.
Internal string data is shared between _charseq_ and _sub-charseq_.

| name        | type            | default value    | description                     |
|:-----------:|:---------------:|-----------------:|:--------------------------------|
| charseq     | charseq:charseq |                  | Source charseq instance         |
| start       | charseq:index   |                  | Start position of _sub-charseq_ |
| end         | charseq:index   | (length charseq) | End position of _sub-charseq_   |
| sub-charseq | charseq:charseq |                  | Created charseq instance        |

Example:
```lisp
* (defparameter *c0* (charseq:make "common-lisp"))
*C0*

* (defparameter *c1* (charseq:sub *c0* 1 10))
*C1*

* *c1*
#S(CHARSEQ:CHARSEQ :STR "common-lisp" :BEG 1 :END 10)

* (charseq:to-string *c1*)
"ommon-lis"

* (defparameter *c2* (charseq:sub *c1* 2 5))
*C2*

* *c2*
#S(CHARSEQ:CHARSEQ :STR "common-lisp" :BEG 3 :END 6)

* (charseq:to-string *c2*)
"mon"
```

--------------------------------------------------------------------------------
#### (to-string charseq &optional start end) => string
Creates a string that is a copy of the subpart of _charseq_ bounded by _start_ and _end_.

| name    | type                       | default value    | description                 |
|:-------:|:--------------------------:|-----------------:|:----------------------------|
| charseq | charseq:charseq            |                  | Source charseq instance     |
| start   | charseq:index              |                0 | Start position of _string_  |
| end     | charseq:index              | (length charseq) | End position of _string_    |
| string  | (simple-array character *) |                  | Created string              |

Example:
```lisp
* (charseq:to-string (charseq:make "common-lisp"))
"common-lisp"

* (charseq:to-string (charseq:make "common-lisp") 3)
"mon-lisp"

* (charseq:to-string (charseq:make "common-lisp") 3 6)
"mon"
```

--------------------------------------------------------------------------------
#### (= charseq1 charseq1) => boolean
#### (> charseq1 charseq1) => boolean
#### (< charseq1 charseq1) => boolean
#### (/= charseq1 charseq1) => boolean
#### (<= charseq1 charseq1) => boolean
#### (>= charseq1 charseq1) => boolean
These functions perform lexicographical order comparison of _charseq1_ and _charseq2_.


### [Macros]
--------------------------------------------------------------------------------
#### (each (char-var charseq &optional result-form) &body body) => result
This macro iterates over the character of _charseq_.

| name        | type             | default value | description
|:-----------:|:----------------:|--------------:|:-------------
| char-var    | symbol(variable) |               | In each iteration, the focused character is bound to this variable |
| charseq     | charseq:charseq  |               | Input charseq instance                                             |
| result-form | T                |           nil | After the iteration, this form is evaluated as the result          |
| body        | T*               |               | The body is executed once for each character in the _charseq_      |

Example:
```lisp
* (defparameter *c* (charseq:make "common-lisp" :start 3 :end 8))
*C*

* (charseq:each (char *c* 'done)
    (print (list :char char)))
(:CHAR #\m)
(:CHAR #\o)
(:CHAR #\n)
(:CHAR #\-)
(:CHAR #\l)
DONE
```

--------------------------------------------------------------------------------
#### (as-string (string-var start-var end-var) charseq &body body) => result
This macro binds the internal state of _charseq_ to _string-var_, _start-var_, and _end-var_, then executes _body_.

| name       | type             | description                                                              |
|:----------:|:----------------:|:-------------------------------------------------------------------------|
| string-var | symbol(variable) | The internal string data of _charseq_ is bound to this variable          |
| start-var  | symbol(variable) | The start position of _charseq_ is bound to this variable                |
| end-var    | symbol(variable) | The end position of _charseq_ is bound to this variable                  |
| charseq    | charseq:charseq  | Input charseq instance                                                   |
| body       | T*               | Any expressions. the scope of bound variables is limited in this _body_. |
| result     | T                | Executed result of _body_                                                |

Example:
```lisp
* (defparameter *c* (charseq:make "common-lisp" :start 3 :end 8))
*C*

* (charseq:as-string (str start end) *c*
    (print (list str start end))
    'done)
("common-lisp" 3 8)
DONE
```

--------------------------------------------------------------------------------
#### (with-dynamic-extent (charseq-var string &key start end) &body body) => result

The semantics of this macro is almost equivalent to the following expression:
```lisp
(let ((charseq-var (charseq:make :start start :end end)))
  (declare (dynamic-extent charseq-var))
  ,@body)
```
On SBCL, the value of _charseq-var_ will be allocated on the stack instead of the heap.
In that case, the cost of creating charseq instance (from _(simple-array character *)_ string) will be almost negligible.
