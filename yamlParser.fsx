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
    PreviousIndentation: int
    RewindingObject: bool
  }
  with
    static member Default = { 
      CurrentIndentation = 0; 
      PreviousIndentation = 0; 
      RewindingObject = false }

let ws = spaces

let mapFromPair a = [a] |> Map.ofList
let toString (a: char array) = String ( a )
let repeatChar a no = String ( a, no )
let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
  Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])
let emptyYObject = YObject Map.empty
let createEmptyYamlObject () = YObject Map.empty

let stringParser = many1Satisfy (fun c -> (int c) >= 65 && (int c) <= 122)
let consumeSpaces = manySatisfy (fun c -> c = ' ')
let consumeEndOfLine = consumeSpaces .>> (attempt newline <|> attempt (eof >>% 'a') <|> (preturn 'a'))

let changeIndentation f : Parser<unit, YamlState> =
  fun stream ->
    stream.UserState <- { 
        CurrentIndentation = (f stream.UserState); 
        PreviousIndentation = stream.UserState.CurrentIndentation;
        RewindingObject = stream.UserState.RewindingObject
      }
    Reply(())
let increaseIndentation : Parser<unit, YamlState> = changeIndentation (fun state -> state.CurrentIndentation + 2)
let decreaseIndentation : Parser<unit, YamlState> = changeIndentation (fun state -> state.CurrentIndentation - 2)

let consumeIndentationHelper peeker reader =
  let mutable continueLooping = true
  let mutable readLength = 0
  while continueLooping do
    let peeked = peeker()
    if peeked <> ' ' then
      continueLooping <- false
    else
      reader() |> ignore
      readLength <- readLength + 1
  readLength

type TriState =
  | TooMany
  | NotEnough
  | Correct

let getIndentationState currentIndentation readLength =
  if readLength = currentIndentation then Correct
  elif readLength < currentIndentation then NotEnough
  else TooMany

let consumeIndentation : Parser<unit, YamlState> = 
  fun stream ->
    let { CurrentIndentation = currentIndentation } = stream.UserState
    let readLength = consumeIndentationHelper stream.Peek stream.Read 
    let indentationState = getIndentationState currentIndentation readLength
    printfn "%A :: %i" indentationState currentIndentation
    match indentationState with
      //  this is when the indentation is less than what we expect, not a fatal, just need to start unwiding
    | NotEnough -> 
        stream.Skip(-readLength) |> ignore
        Reply(Error, messageError "Soft error")
    | TooMany -> 
        stream.Skip(-readLength) |> ignore
        let msg = sprintf "Incorrect Indentation, expected %i spaces, got %i" currentIndentation readLength
        Reply(FatalError, messageError msg)
    | Correct -> Reply(())

let yamlObject, yamlObjectRef = createParserForwardedToRef()
let valueParser = stringParser .>> consumeEndOfLine |>> YValue
let keyParser = stringParser .>> pchar ':' .>> consumeSpaces |>> string
let keyValueParser = keyParser .>>. valueParser |>> (mapFromPair >> YObject)
let keyObjectParser = 
  keyParser .>>. (consumeEndOfLine >>. (fun stream ->
    let { CurrentIndentation = currentIndentation } = stream.UserState



    Reply(())
  ) |>> (mapFromPair >> YObject)


let stopObjectCollectionOrContinue (consumeParser: Parser<unit, YamlState>) (continuationParser: Parser<Yaml, YamlState>) : Parser<Yaml, YamlState> =
  fun stream ->
    let consumeReply = consumeParser stream
    printfn "Status :: %A" consumeReply.Status
    match consumeReply.Status with
    | FatalError -> Reply(FatalError, consumeReply.Error)
      // well here means that consuming indentation didn't get to where it expected to
      // but it was incorrect, so we need to fall
    | Error -> 
      printfn "Well we're going to fall out gracefully now"
      decreaseIndentation stream |> ignore
      printfn "New Indentation :: %A" stream.UserState
      Reply(createEmptyYamlObject())
    | Ok -> continuationParser stream
    | _ -> Reply(FatalError, messageError "Had an error stopping object collection or continue")

let parseObject : Parser<Yaml, YamlState> =
  let attemptKey =
    keyParser .>>. (consumeEndOfLine >>. increaseIndentation >>. yamlObject) |>> (function pair -> YObject (Map.ofList [pair]))
  let tryKeyValueThenKey = (attempt keyValueParser) <|> attemptKey 
  // what is happening is it is trying to do the attemptKeyValue and failing - obviously falling out the 
  // recursion, so need to figure out what to do about attempting objects - seems like consumeIndent may not be detecting or something
  stopObjectCollectionOrContinue consumeIndentation (fun stream ->
    let rpl = many tryKeyValueThenKey |>> (fun yobjs ->
        List.fold (fun yreturn yobj ->
          match yobj, yreturn with
          | YObject o, YObject o2 -> YObject (join o o2)
          | _ -> YObject (Map.empty)
        ) emptyYObject yobjs
      )
    printfn "Done trying... exiting parseObject"
    rpl stream
  )

let wrapParseObject : Parser<Yaml, YamlState> = 
  attempt (many (fun stream ->
    printfn "running base parser :: %c" (stream.Peek())
    let rtn =  yamlObject stream
    rtn
  ) |>> (fun yobjs -> 
    let rtn =
      List.fold (fun yreturn yobj ->
        printfn "got here with %A\n%A" yreturn yobj 
        match yobj, yreturn with
        | YObject o, YObject o2 -> YObject (join o o2)
        | _ -> YObject (Map.empty)
      ) emptyYObject yobjs
    printfn "whatup ===== " |> ignore
    rtn
  )) .>> eof

do yamlObjectRef := parseObject

let file = wrapParseObject .>> eof

let runYaml p s = runParserOnString p YamlState.Default "" s

runYaml (many (attempt (keyValueParser |>> (fun a -> YObject (Map.ofList [a]))) <|> (keyParser |>> (fun a -> YObject (Map.ofList [a, createEmptyYamlObject()]))))) "a: b\nc:d\ne:  f\n" |> printfn "%A"

// runYaml ((keyValueParser |>> (fun a -> [a] |> Map.ofList |> YObject) ) <|> (keyParser |>> (fun a -> [a,YObject (Map.empty)] |> Map.ofList |> YObject))) "a: " |> printfn "%A"
