1.1.3
=====

* Remove the upper limit on ppxlib

1.1.2
=====

* Fix the generated code to also emit `Stdlib` instead of `Pervasives`. This
  means you need `stdlib-shims` if you're building on OCaml <4.07.

1.1.1
=====

* Fix dune dependency
* Use `Stdlib` instead of `Pervasives` for forward-compatibility

1.1
===

* Caching can be disabled on a per statement basis.
  (Caching is enabled by default)

1.0
===

* All statements are now cached
* First public release

0.5
===

* Refactor code generation to eliminate internal use of exceptions
* Support custom types wrapped under modules

0.4
===

* Support for lists of input parameters
* Add examples for Lwt and Async

0.3
===

* More useful error messages

0.2
===

* Minor refactoring of the API
* Creation of ppx\_mysql\_identity subpackage

0.1
===

* Initial release
