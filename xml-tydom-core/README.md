# <img src="https://rawgit.com/lancelet/xml-tydom/master/xml-tydom-logo.svg" width="100"/><br/>xml-tydom-core

__End users please note:__ End-users probably don't need to be concerned with
this library. Instead, end-users probably want the library written for a
specific back-end. Currently, that is limited to [xml-tydom-conduit][1].

`xml-tydom-core` provides the core of a typed XML encoding / decoding library in
Haskell. Its main purposes are:

  1. Abstract over different XML DOM implementations,
  2. Define classes which can perform encoding / decoding, and
  3. Provide instances of encoding / decoding classes using GHC Generics.
  
[1]: https://github.com/lancelet/xml-tydom/tree/master/xml-tydom-conduit
