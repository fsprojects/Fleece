#### 0.10.0  - May 25 2022
* Internally using mainly codecs (more efficient and less boilerplate in source code).
* Ability to switch to different implementations, even at the same time, this allows to use several Json implementations at the same time.
* Unify Codec types, no more ConcreteCodec vs tupled codecs, just a simple 4 params Codec type.
* A new and recommended Codec syntax for records and DUs trough an Applicative CE.
* Internal caching of codecs.
* Ability to workaround codecs for interfaces.
* jopt combinator works with all types supporting `zero`, so in addition to option we can use voption, nullable or even list.
* Native support for bigint, vtuple, voption, TimeSpan, NonEmptyList, NonEmptySet, NonEmptyMap, and "Generic Map" (Maps where keys are not strings).
* DateOnly and TimeOnly support for .net6 users

#### 0.9.0  - October 28 2021
* Added Result codec and overload
* Fix problem decoding null values into Options.
* Fix parsing of floats for infinities and nan.
* Fix parsing of Datetime when using newtonsoft.json

#### 0.8.0  - May 17 2020
* Added System.Text.Json implementation
* Support for enums and all tuple sizes
* Fix: error reporting wrong index in 7-uples
* FSharpData use its own type for JsonObject
* Added missing ofJson/toJson support for JsonValue and JsonObject
* Upgrade to FSharpPlus 1.1.1

#### 0.7.0  - September 27 2018
* Json Lens
* Codecs
* Combinators
* Upgrade to System.Json 4.5
* Fix somes issue with Newtonsoft serialization
* Breaking changes: Success and Failure functions moved to Helpers namespace
* Breaking changes from 0.6.1 : encode, decode, mapping, jgetopt, jpairopt, jfieldopt functions

#### 0.6.1  - September 5 2018
* Codec support for Json Objects

#### 0.6.0 August 23 2018
* Breaking change for Newtonsoft and FSharp.Data: use a specific module
* Binary Breaking Change: use Result<_,_> instead of Choice<_,_>

#### 0.5.1  - December 31 2017
* Lock System.Json
* Bug fixes in Newtonsoft implementation

#### 0.5.0  - May 27 2018
* Added FSharp.Data implementation
* Netstandard support
* Friendlier API (non-breaking)
* Null keys are filtered out in JSON objects
* Updated dependencies

#### 0.4.0  - September 9 2014
* Added FSharp.Data implementation
* Support for milliseconds in json dates

#### 0.3.0  - July 31 2014 
* Support for Guid, Dictionary and ResizeArray
* Deserialization added for JsonObject
* Updated dependecies

#### 0.2.0  - April 9 2014
* Support for Map and Nullable
* More Xml docs
* Minor optimizations

#### 0.1.0  - January 20 2014
* Initial release
