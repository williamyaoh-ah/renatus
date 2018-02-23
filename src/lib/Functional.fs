// Base-level combinators for JSON encoding/decoding. Think the minimal
// definition you'd need to implement endofunctoriality and applicative
// for decoders.

namespace Renatus

module JSONResult =

  let map f result =
    match result with
      | Success value -> Success <| f value
      | Failure (err, pos) -> Failure (err, pos)

  let mapFail f result =
    match result with
      | Success value -> Success value
      | Failure (err, pos) -> Failure (f err, pos)

  let bimap f g result =
    map f >> mapFail g <| result

[<AutoOpen>]
module Functional =

  let internal mapResult f decoder =
    fun json -> decoder json |> f

  let map f decoder =
    mapResult (JSONResult.map f) decoder

  let mapFail f decoder =
    mapResult (JSONResult.mapFail f) decoder

  let bimap f g decoder =
    mapResult (JSONResult.bimap f g) decoder

  /// Simply create a decoder that immediately returns the given value.
  let just value =
    fun _ -> Success value

  /// Think of this as "normal function application", except generalised
  /// over where both the function and the value *come from*. In normal
  /// function application, the parameter is a parameter in the environment;
  /// here, the parameter gets parsed out of a JSON document.
  let apply next here =
    fun json ->
      match here json with
        | Success f -> map f next json
        | Failure (err, pos) -> Failure (err, pos)

  /// Try the first decoder, then the second if the first one fails.
  /// Return the result of the first decoder which fails.
  ///
  /// Since both decoders might fail, you must provide a way to combine
  /// their errors if that happens. Curry the function with a combiner of
  /// choice to make it more convenient for you.
  let either combine left right =
    fun json ->
      match left json with
        | Success value -> Success value
        | Failure (err1, pos) ->
          match right json with
            | Success value -> Success value
            | Failure (err2, _) -> Failure (combine err1 err2, pos)
              
