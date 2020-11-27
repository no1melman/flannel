#r "packages/FParsec/lib/netstandard2.0/FParsecCS.dll"
#r "packages/FParsec/lib/netstandard2.0/FParsec.dll"

open FParsec
open System

type Yaml =
  | YValue of string
  | YObject of Map<string, Yaml>
  | YArray of Yaml list

type YamlState = {
    CurrentIndentation: int
  }
  with
    static member Default = { CurrentIndentation = 0 }

let ws = spaces

let stringParser = manySatisfy (fun c -> c <> ' ' && c <> ':')

let consumeSpaces = manySatisfy (fun c -> c = ' ')

let consumeNewLine = consumeSpaces >>. pchar '\n'

let keyParser = stringParser .>> pchar ':' |>> string

let valueParser = consumeSpaces >>. stringParser .>> consumeSpaces |>> YValue 

let keyValueParser = keyParser .>>. valueParser

let toString (a: char array) = String ( a )
let repeatChar a no = String ( a, no )
let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let consumeIndentation : Parser<unit, YamlState> = 
  fun stream ->
    let { CurrentIndentation = currentIndentation } = stream.UserState
    let readIndent = currentIndentation + 2
    let readString = seq { 0..(readIndent - 1) } |> Seq.fold (fun r i -> stream.Read() :: r) [] |> Array.ofList |> toString
    let nextChar = stream.Peek()
    let correctIndentation = readString = "  "
    let nextCharIsntSpace = nextChar <> ' '
    printfn "-->%s<-- -->%c" readString nextChar
    if correctIndentation && nextCharIsntSpace then
      stream.UserState <- { CurrentIndentation = readIndent }
      Reply(())
    else
      stream.Skip(-readIndent)
      let msg =
        match correctIndentation, nextCharIsntSpace with
        | false, _ -> sprintf "Incorrect Indentation, expected %i spaces" readIndent
        | true, false -> sprintf "Incorrect Indentation, got more than %i spaces" readIndent
        | _ -> sprintf "Indent error"
      Reply(FatalError, messageError msg)

let yamlObject, yamlObjectRef = createParserForwardedToRef()

let parseObject : Parser<Yaml, YamlState> =
  let tryNextPropertyOrObject =
    (attempt (consumeNewLine >>. yamlObject)) <|> (eof >>% (Map.empty |> YObject))
  let attemptKeyValue =
    (fun stream -> 
      printfn "attempting key value, %c" (stream.Peek())
      Reply(())
    ) >>. keyValueParser .>>. ((fun stream ->
      printfn "attempting key value consume new line, %c" (stream.Peek())
      Reply(())
    ) >>. tryNextPropertyOrObject) |>> (function
      | a, YObject innerobj -> [a] |> Map.ofList |> join innerobj |> YObject
      | _ -> YObject Map.empty)
  let attemptKey =
    (fun stream ->
      printfn "attempting key, %c" (stream.Peek())
      Reply(())
    ) >>.
    keyParser .>>. (  
      (fun stream ->
        printfn "attempting key consume new line, %c" (stream.Peek())
        Reply(())
      ) >>.
      consumeNewLine >>. consumeIndentation >>. yamlObject
    ) |>> (function pair -> YObject (Map.ofList [pair]))
  (fun stream -> 
    printfn "diving in, %c" (stream.Peek())
    Reply(())
  ) >>. ((attempt attemptKeyValue) <|> attemptKey)

do yamlObjectRef := parseObject

let file = yamlObject .>> eof

let runYaml p s = runParserOnString p YamlState.Default "" s

runYaml file "a: \n  b: c" |> printfn "%A"

// runYaml ((keyValueParser |>> (fun a -> [a] |> Map.ofList |> YObject) ) <|> (keyParser |>> (fun a -> [a,YObject (Map.empty)] |> Map.ofList |> YObject))) "a: " |> printfn "%A"
