## Renatus

Renatus is meant to be a **complete** solution for parsing, serializing,
and deserializing JSON in F#. To that end, it has X main priorities:

* Be easy to use and understand. Renatus understands that you've got
  better things to do than understand the guts of another library.
  Common things should require little code, and difficult things should
  be doable even without detailed internals knowledge. Renatus should
  provide help to get your data into real data structures, not just a blob
  of JSON data.
* Handle errors gracefully. Renatus understands that the real world
  is messy, and that things may not always parse so cleanly. Failed
  parses should always provide useful line and column information,
  and parsing should never fail with an exception.
* Be helpful. Renatus understands that JSON is often the main interface
  to your program, and to that end, it should guide your users in the
  right direction when they make mistakes. When parsing fails, Renatus
  should give information about how to correct the input, and it should
  be easy to extend Renatus' error messages with domain-specific
  guidance.

Right now, Renatus is in a preliminary state. You can help by using it
in your own projects, or contributing!