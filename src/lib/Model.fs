namespace Renatus

open System

/// The heart of any JSON library; the JSON data itself.
/// Usually you shouldn't need to use this structure directly,
/// but weird problems that need weird solutions tend to sneak up on you.
type JSONValue =
  | Null
  | Bool of Boolean
  | Num of Double  // Technically, JSON doesn't allow infinities and NaNs.
                   // For consistency with JS, we'll use a 64-bit value.
  | Str of String
  | Arr of List<JSONValue>
  | Obj of Map<String, JSONValue>

type Source =
  | Known of String
  | Unknown
  with
  override self.ToString () =
    match self with
      | Known src -> src
      | Unknown -> "<unknown>"

/// Parsing *will* fail sometimes. That's a guaranteed fact. We need
/// to provide useful information when it does.
type Position =
  { lineNum: UInt32
  ; colNum: UInt32
  ; src: Source
  }

type JSONResult<'e, 'a> =
  | Success of 'a
  | Failure of ('e * Position)

/// Our core JSON type, which attaches position information to each
/// element in a JSON document.
type JSON =
  (JSONValue * Position)

/// A value that knows how to turn a `JSON` into a 'a, or returns
/// an error of 'e, if deserializing fails.
type Decoder<'e, 'a> =
  JSON -> JSONResult<'a, 'e>

/// A value that knows how to turn a 'a into a `JSON`.
type Encoder<'a> =
  'a -> JSON

/// A value that knows both how to turn a `JSON` into an 'a, and
/// knows how to turn 'a into a `JSON`. Think of an isomorphism/epimorphism.
type Bicoder<'e, 'a> =
  (Encoder<'a> * Decoder<'e, 'a>)
