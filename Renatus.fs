namespace Renatus

#nowarn "58"  // Offside indentation warnings.
#nowarn "62"  // ML compatibility warnings.

[<AutoOpen>]
module JSON =

  /// JSON values. Note that we use IEEE 754 64-bit floating point;
  /// whille the JSON specification doesn't specify a limit on numeric
  /// precision, JavaScript does, so we'll use that limit.
  type Value =
    | Null
    | Boolean of bool
    | Number of double  // JSON does not allow infinities or NaN.
                        // So please no abuserino :(
    | String of string
    | Array of Value List
    | Object of (string, Value) Map



  module Decode =

    /// An object which knows how to decode JSON values into a given type
    /// of value.
    type 'a Decoder = private Decoder of (Value -> 'a Option)

    /// Take a decoder and run it against a JSON value.
    let run (decoder : 'a Decoder) (json : Value) : 'a Option =
      let (Decoder f) = decoder in f json

    /// Transform the return value of a Decoder.
    let map (f : 'a -> 'b) (decoder : 'a Decoder) : 'b Decoder =
      let (Decoder g) = decoder in Decoder (g >> Option.map f)

    let nul : unit Decoder =
      Decoder <| fun json -> match json with
        | Null -> Some ()
        | _other -> None
    let bool : bool Decoder =
      Decoder <| fun json -> match json with
        | Boolean b -> Some b
        | _other -> None
    let num : double Decoder =
      Decoder <| fun json -> match json with
        | Number n -> Some n
        | _other -> None
    let str : string Decoder =
      Decoder <| fun json -> match json with
        | String s -> Some s
        | _other -> None

    /// Don't do anything with the current JSON value, just immediately return it.
    let raw : Value Decoder =
      Decoder <| fun json -> Some json

    /// Always return a given value. Useful if you need a default.
    let value (v : 'a) : 'a Decoder =
      Decoder <| fun _ -> Some v

    /// Always fail to decode.
    let fail<'a> : 'a Decoder =
      Decoder <| fun _ -> None

    let andThen (next : 'a -> 'b Decoder) (here : 'a Decoder) : 'b Decoder =
      Decoder <| fun json -> match run here json with
        | Some result -> run (next result) json
        | None -> None

    /// Create a decoder which returns the result of
    /// the first decoder that succeeds.
    let either (left : 'a Decoder) (right : 'a Decoder) : 'a Decoder =
      Decoder <| fun json -> match run left json with
        | Some result -> Some result
        | None -> run right json

    /// Create a decoder which runs all the given decoders until one
    /// of them succeeds.
    let oneOf (decoders : ('a Decoder) List) : 'a Decoder =
      match decoders with
        | [] -> fail
        | head :: rest -> List.fold either head rest

    /// Create a decoder which deals with null values.
    let nullable (decoder : 'a Decoder) : ('a Option) Decoder =
      oneOf
        [ nul |> map (fun _ -> None)
        ; decoder |> map Some
        ]

    /// Extract a property from a JSON object.
    let field (name : string) (decoder : 'a Decoder) : 'a Decoder =
      Decoder <| fun json -> match json with
        | Object fields -> match Map.tryFind name fields with
          | Some value -> run decoder value
          | None -> None
        | _other -> None

    /// Extract a nested property.
    let rec at (fields : string List) (decoder : 'a Decoder) : 'a Decoder =
      match fields with
        | [] -> decoder
        | f :: fs -> field f <| at fs decoder

    /// Decode a JSON array containing elements of a single type.
    let arr (decoder : 'a Decoder) : ('a List) Decoder =
      let combine listM eleM =
        eleM |> Option.bind (fun ele -> listM |> Option.map (fun list -> ele :: list))

      Decoder <| fun json -> match json with
        | Array elements ->
             elements
          |> List.map (run decoder)
          |> List.fold combine (Some [])
          |> Option.map List.rev
        | _other -> None

    /// Decode a JSON array of exactly two elements.
    let tup2 (decode1 : 'a Decoder) (decode2 : 'b Decoder) : ('a * 'b) Decoder =
      Decoder <| fun json -> match json with
        | Array [ele1; ele2] ->
          run decode1 ele1 |> Option.bind (fun x ->
          run decode2 ele2 |> Option.bind (fun y ->
          Some (x, y)))
        | _other -> None

    /// Decode a JSON array of exactly three elements.
    let tup3 (decode1 : 'a Decoder) (decode2 : 'b Decoder) (decode3 : 'c Decoder) =
      Decoder <| fun json -> match json with
        | Array [ele1; ele2; ele3] ->
          run decode1 ele1 |> Option.bind (fun x ->
          run decode2 ele2 |> Option.bind (fun y ->
          run decode3 ele3 |> Option.bind (fun z ->
          Some (x, y, z))))
        | _other -> None

    /// Alias for 'value', for more readable decoding pipelines.
    let decode (f : 'a) : 'a Decoder = value f

    /// Run the given decoder and insert it into the pipeline.
    let custom (decoder : 'a Decoder) : (('a -> 'b) Decoder -> 'b Decoder) =
      fun ccM -> ccM |> andThen (fun cc -> decoder |> map cc)

    /// Decode a required field, when decoding JSON objects in pipeline.
    let required (name : string) (decoder : 'a Decoder) : (('a -> 'b) Decoder -> 'b Decoder) =
      custom <| field name decoder



  /// How to serialize objects into JSON:
  ///   1) Generate a JSON object using the constructors on the Value union type
  ///      and this module.
  ///   2) Call @encode@ on that JSON object.
  ///   3) You're done!
  module Encode =

    open System.Text

    let private escape : string -> string =
      let escapeChar = fun c -> match c with
        | '"' | '\\' | '/' | '\b' | '\u000C' | '\n' | '\r' | '\t' ->
          sprintf """\%c""" c
        | _ -> string c

      String.collect escapeChar

    // itersperse : (unit -> unit) -> ('a -> unit) -> 'a List -> unit
    // Yes, you read the name of the function correctly.
    let rec private itersperse f g = function
      | [x]   -> g x
      | x::xs -> g x; f (); itersperse f g xs
      | __    -> ()

    /// Encode the JSON into a single line.
    let encode (json : Value) : string =
      let rec encode' (builder : StringBuilder) (json : Value) =
        let add (s : string) = builder.Append(s) |> ignore

        match json with
          | Null -> add "null"
          | Boolean b -> add <| if b then "true" else "false"
          | Number n -> add <| n.ToString()
          | String s -> add (sprintf "%A" <| escape s)
          | Array elements ->
            add "["
            itersperse (fun () -> (add ",")) (encode' builder) elements
            add "]"
          | Object fields ->
            add "{"
            itersperse (fun () -> (add ","))
                       (fun (k, v) -> add (sprintf "%A:" <| escape k); encode' builder v)
                       (Map.toList fields)
            add "}"

      let mutable builder = StringBuilder()
      encode' builder json
      builder.ToString()

    /// Combine the fields of two JSON objects. Consequences are undefined
    /// if one or both parameters are not JSON objects.
    let combine (val1 : Value) (val2 : Value) =
      match val1, val2 with
        | Object fields1, Object fields2 ->
          Object <| Map.fold (fun m k v -> Map.add k v m) fields1 fields2
        | _other -> val1

    /// Directly create a JSON object from a list of key-value pairs.
    let obj pairs =
      Object <| Map.ofList pairs



  /// FParsec parsers for JSON types.
  module Parsing =

    open FParsec

    type Parser<'a> = Parser<'a, unit>

    /// A dummy parser which we initialize later; needed because the
    /// JSON grammar is recursive, so the parsers need to be self-referential.
    /// See: http://www.quanttec.com/fparsec/reference/primitives.html#members.createParserForwardedToRef
    let pvalue, private pvalueRef = createParserForwardedToRef<Value, unit>()

    let pnull : Parser<Value> = stringReturn "null" Null
    let pbool : Parser<Value> =
          stringReturn "true" (Boolean true)
      <|> stringReturn "false" (Boolean false)
    let pnum : Parser<Value> =
      pfloat |>> Number
    let private pstr' : Parser<string> =
      let quote = pstring "\""

      let normalChars = manySatisfy (fun c -> c <> '"' && c <> '\\')
      let hexEscape = pstring "\\u" >>. anyString 4 >>= (fun hexStr ->
        let (succeed, x) = System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null)
        if succeed then char x |> string |> preturn
        else fail "expected a valid Unicode hex literal")
      let charEscape =
        pstring "\\" >>. anyOf "\"\\/bfnrt" |>> function
          | 'b' -> "\b"
          | 'f' -> "\u000C"
          | 'n' -> "\n"
          | 'r' -> "\r"
          | 't' -> "\t"
          | c   -> string c
      let escaped = charEscape <|> hexEscape

      between quote quote (stringsSepBy normalChars escaped)
    let pstr : Parser<Value> = pstr' |>> String
    let parr : Parser<Value> =
      between (pstring "[" >>. spaces) (pstring "]") (sepBy (pvalue .>> spaces) (pstring "," >>. spaces))
      |>> Array
    let pobj : Parser<Value> =
      let pproperty =
        pstr' >>= (fun key ->
        spaces >>.
        pstring ":" >>.
        spaces >>.
        pvalue >>= (fun value ->
        preturn (key, value)))

      between (pstring "{" >>. spaces) (pstring "}")
              (sepBy (pproperty .>> spaces) (pstring "," >>. spaces))
      |>> Map.ofList
      |>> Object

    do pvalueRef := choice [ pnull; pbool; pnum; pstr; parr; pobj ]

    /// Parser for extracting a JSON value out of a string. Ignores any trailing input.
    let pjson : Parser<Value> = spaces >>. pvalue .>> spaces .>> eof

    /// Parser for multiple JSON documents concatenated in a string.
    let pjsons : Parser<Value List> = spaces >>. sepEndBy pvalue spaces .>> eof

    let private fromParserResult : ('a, 'b) ParserResult -> 'a Option = function
      | Success (output, _, _) -> Some output
      | Failure (_, _, _) -> None

    /// Attempt to parse a single JSON document.
    let tryParseJSON : string -> Value Option =
      run pjson >> fromParserResult
    /// Attempt to parse multiple, concatenated JSON documents.
    let tryParseJSONs : string -> (Value List) Option =
      run pjsons >> fromParserResult

    /// Parse a single JSON document. Raise an exception on failure to parse.
    let parseJSON : string -> Value =
      tryParseJSON >> function
        | Some json -> json
        | None -> failwith "parse failure for single JSON document"
    /// Parse multiple, concatenated JSON documents. Raise an exception on
    /// failure to parse.
    let parseJSONs : string -> Value List =
      tryParseJSONs >> function
        | Some jsons -> jsons
        | None -> failwith "parse failure for multiple JSON documents"
