module Tests

open FsCheck
open FsCheck.Xunit

open System.Text

open Renatus
open Renatus.JSON.Encode

let private unpack (nonnull : 'a NonNull) : 'a =
  let (NonNull value) = nonnull in value

let private unexpand (chars : char List) : string =
  let folder (sb : StringBuilder) (c : char) = sb.Append(c)
  string <| List.fold folder (StringBuilder()) chars

/// Printable ASCII characters; that is, between 0x20 and 0x7f
let private printableASCII : Gen<char> =
  Gen.choose (0x20, 0x7f) |> Gen.map char

/// Valid JSON characters which are also ASCII.
let private jsonASCII : Gen<char> =
  Gen.frequency
    [ (10, printableASCII)
    ; (1, Gen.elements ['\b'; '\u000C'; '\n'; '\r'; '\t'])
    ]

/// A string which a) contains only printable ASCII characters
///                b) is never null
let asciiString : Gen<string> =
  Gen.listOf printableASCII |> Gen.map unexpand

/// Valid JSON strings which are also ASCII.
let jsonString : Gen<string> =
  Gen.listOf jsonASCII |> Gen.map unexpand

let rec isValidJSON = function
  | Null | Boolean _ | Number _ -> true
  | String s -> s <> null
  | Array inner -> List.forall isValidJSON inner
  | Object fields -> Map.forall (fun k v -> isValidJSON v && k <> null) fields

let flatGen : Gen<Value> =
  Gen.oneof
    [ Gen.constant Null
    ; Arb.generate<bool> |> Gen.map Boolean
    ; Arb.Default.NormalFloat()
      |> Arb.toGen
      |> Gen.map (fun (NormalFloat f) -> Number f)
    ; jsonString |> Gen.map String
    ]
let rec private mapGen () : Gen<Map<string, Value>> =
     Gen.listOf (Gen.zip asciiString (validJSON ()))
  |> Gen.map Map.ofList
and arrGen () : Gen<Value> =
  Gen.listOf (validJSON ()) |> Gen.map Array
and objGen () : Gen<Value> =
     mapGen ()
  |> Gen.map Object
and validJSON () : Gen<Value> =
  let validJSON' size =
    match size with
      | 0 -> flatGen
      | _ -> Gen.frequency
               [ (7,  objGen ())
               ; (3,  arrGen ())
               ; (10, flatGen)
               ]

  Gen.sized validJSON'

type ValidMap =
  static member ValidMap () =
    Arb.fromGen <| mapGen ()

type ValidJSON =
  static member ValidJSON () =
    Arb.fromGen <| validJSON ()

type ValidObject =
  static member ValidObject () =
    Arb.fromGen <| objGen ()

type ASCIIString =
  static member ASCIIString () =
    Arb.fromGen <| asciiString

type JSONString =
  static member JSONString () =
    Arb.fromGen <| jsonString

type ControlCharacter =
  static member ControlCharacter () =
    Arb.fromGen <| Gen.elements ['\b'; '\u000C'; '\n'; '\r'; '\t']

[<Properties( Arbitrary=[| typeof<ValidJSON> |], EndSize=20 )>]
module JSONTests =

  open Renatus.JSON.Encode
  open Renatus.JSON.Parsing

  let private deltaCompare (f1 : float) (f2 : float) : bool =
    let abs = fun (f : float) -> System.Math.Abs(f)
    let diff = abs <| f1 - f2
    diff <= 0.01  // TODO: This sucks, fix this.

  /// Custom comparison for JSON objects, to make sure that we do numerical
  /// comparison *kind of* correctly.
  let rec private jsonCompare (j1 : Value) (j2 : Value) : bool =
    match j1, j2 with
      | Null, Null -> true
      | Boolean b1, Boolean b2 -> b1 = b2
      | Number n1, Number n2 -> deltaCompare n1 n2
      | String s1, String s2 -> s1 = s2
      | Array a1, Array a2 -> List.map2 jsonCompare a1 a2 |> List.forall id
      | Object o1, Object o2 ->
        let fields1 = o1 |> Map.toList |> List.sortBy fst
        let fields2 = o2 |> Map.toList |> List.sortBy fst
        List.map2 (fun (k1, v1) (k2, v2) -> k1 = k2 && jsonCompare v1 v2) fields1 fields2
        |> List.forall id
      | _other -> false

  [<Property( MaxTest=1000, Arbitrary=[| typeof<JSONString> |] )>]
  let ``JSON string escaping works correctly`` string =
    String string |> encode |> tryParseJSON |> function
      | Some (String string') -> string = string'
      | __                    -> false

  [<Property( MaxTest=1000, EndSize=100, Arbitrary=[| typeof<ControlCharacter> |])>]
  let ``JSON string escaping works correctly with control characters embedded`` chars =
    let str = unexpand chars
    let json = String str
    let jsonStr = json |> encode
    match tryParseJSON jsonStr with
      | Some (String str') -> jsonStr <> str && str' = str
      | __                 -> false

  [<Property( MaxTest=1000 )>]
  let ``generator always generates valid JSON`` json =
    isValidJSON json

  [<Property( MaxTest=1000 )>]
  let ``generated JSON can always be deserialized`` json =
    json |> encode |> tryParseJSON |> function
      | Some _ -> true
      | None   -> false

  [<Property( MaxTest=1000 )>]
  let ``generated JSON can always be deserialized into equivalent object`` json =
    json |> encode |> tryParseJSON |> function
      | Some json' -> jsonCompare json json'
      | None       -> false

  [<Property( MaxTest=1000 )>]
  let ``generated JSON can always be deserialized, when concatenated together`` json =
    let jsonStr = json |> encode |> fun s -> sprintf "%s\n%s\n%s\n" s s s
    tryParseJSONs jsonStr |> function
      | Some [json1; json2; json3] ->
        jsonCompare json json1 &&
        jsonCompare json json2 &&
        jsonCompare json json3
      | __                         -> false

  [<Properties( Arbitrary=[| typeof<ValidMap> |] )>]
  module Combining =

    // These tests aren't super important, and shouldn't be taken as indicative
    // of whether Renatus is working as a whole.

    type ObjMap = Map<string, Value>

    [<Property>]
    let ``combining two JSON objects always returns a JSON object`` (map1 : ObjMap) (map2 : ObjMap) =
      combine (Object map1) (Object map2) |> function
        | Object _ -> true
        | _other   -> false

    [<Property>]
    let ``combining JSON objects always has keys from both maps`` (map1 : ObjMap) (map2 : ObjMap) =
      combine (Object map1) (Object map2) |> function
        | Object fields ->
          let inFields = fun found k _v -> found && Map.containsKey k fields
          Map.fold inFields true map1 && Map.fold inFields true map2
        | _other -> false

    [<Property( Arbitrary=[| typeof<ASCIIString> |], MaxTest=2500 )>]
    let ``combining JSON objects never has keys not in either map`` (map1 : ObjMap) (map2 : ObjMap) (key : string) =
      (not (Map.containsKey key map1) && not (Map.containsKey key map2)) ==>
        (combine (Object map1) (Object map2) |> function
          | Object fields -> not <| Map.containsKey key fields
          | _other        -> false)

    [<Property>]
    /// Technically, the previous two properties are enough to guarantee that this
    /// property holds, but it's nice to have for completeness.
    let ``combining with an empty map is the identity`` (map : ObjMap) =
      let keys = Map.toList >> List.map fst
      combine (Object map) (Object Map.empty) |> function
        | Object fields -> keys map = keys fields
        | _other        -> false

  module Decoding =

    // It's not entirely clear to me how to test the decoding functionality in
    // a good way. Maybe in the way that the combinators interact?

    type private TestObject =
      { field1 : int
      ; field2 : string List
      }

    let private toJSON testObject =
      obj [ ("field1", Number (testObject.field1 |> double))
          ; ("field2", Array (List.map String testObject.field2))
          ]

    let private ofJSON : Decode.Decoder<TestObject> =
      Decode.decode (fun f1 f2 -> { field1 = f1; field2 = f2 })
      |> Decode.required "field1" (Decode.num |> Decode.map int)
      |> Decode.required "field2" (Decode.arr Decode.str)

    [<Property( Arbitrary=[| typeof<ASCIIString> |], MaxTest=2500 )>]
    let private ``can always decode from serialized JSON`` (obj : TestObject) =
      obj |> toJSON |> encode |> tryParseJSON |> function
        | Some json -> json |> Decode.run ofJSON |> function
          | Some obj' -> obj = obj'
          | None -> false
        | None -> false
