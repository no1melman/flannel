#r "packages/FParsec/lib/netstandard2.0/FParsecCS.dll"
#r "packages/FParsec/lib/netstandard2.0/FParsec.dll"

open FParsec
open System
open System.Text

type Yaml =
  | YValue of string
  | YObject of Map<string, Yaml>
  | YArray of Yaml list

type YamlState = {
    CurrentIndentation: int
    PreviousIndentation: int
  }
  with
    static member Default = { 
      CurrentIndentation = 0 
      PreviousIndentation = 0 
    }

let mapFromPair a = Map.ofList [a]
let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
  Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])
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

let stringParser = many1Satisfy (fun c -> (int c) >= 65 && (int c) <= 122)
let spaceConsumer = manySatisfy (fun c -> c = ' ')
let consumeIndentation : Parser<_,YamlState>=
  fun stream ->
    let stateTag = stream.StateTag
    let { CurrentIndentation = currentInd } = stream.UserState
    let readLength = consumeIndentationHelper stream.Peek stream.Read
    if readLength = currentInd then Reply(())
    else
      stream.Skip(-readLength) |> ignore // re position the stream back to original
      stream.StateTag <- stateTag // reset state tag
      Reply(Error, NoErrorMessages)

let valueParser = stringParser .>> spaceConsumer |>> (YValue)
let keyParser = stringParser .>> pchar ':' .>> spaceConsumer

let keyValueParser = (consumeIndentation >>. keyParser) .>>. valueParser .>> restOfLine true |>> (mapFromPair >> YObject)

let recursiveKeyValueOrKeyParser, refRecursiveKeyValueOrKeyParser = createParserForwardedToRef()

let changeState inc state = { state with CurrentIndentation = state.CurrentIndentation + inc }

let gotoNextObj : Parser<Yaml, YamlState> = updateUserState (changeState 2) >>. recursiveKeyValueOrKeyParser .>> updateUserState (changeState -2)
let keyValueOrKeyParser = attempt (consumeIndentation >>. keyParser .>> newline .>>. gotoNextObj |>> (mapFromPair >> YObject)) <|> keyValueParser

do refRecursiveKeyValueOrKeyParser := many keyValueOrKeyParser |>> (fun yobjs -> 
    List.fold (fun st yobj -> 
      match st, yobj with
      | YObject a, YObject b -> YObject (join a b)
      | _ -> st
      ) (YObject Map.empty) yobjs
  )

let addRange (sb: StringBuilder) range = range |> List.iter (sb.AppendLine >> ignore)

let rec printYamlObj (indentToPrint: int) (yobj: Yaml) : string =
  let indentation = String (' ', indentToPrint)
  let sb = StringBuilder()
  match yobj with
  | YObject o ->
    o 
    |> Map.toList
    |> List.map (fun (k, v) -> 
        let newline = function YObject _ -> "\n" | _ -> ""
        sprintf "%s%s: %s%s" indentation k (v |> newline) (printYamlObj (indentToPrint + 2) v)
      )
    |> List.reduce (fun a b ->
        sprintf "%s\n%s" a b
      )
    |> fun a -> sb.Append(a)
    |> ignore
    sb.ToString()
  | YArray os ->
    os 
    |> List.map (fun yobj -> sprintf "%s- %s" indentation (printYamlObj (indentToPrint + 2) yobj))
    |> addRange sb
    |> ignore
    sb.ToString()
  | YValue v -> v

let runYaml p s = runParserOnString p YamlState.Default "" s
let runYamlCustom p s st = runParserOnString p st "" s

runYamlCustom recursiveKeyValueOrKeyParser "a: \n  b:c\n  d:\n    e:efr\n    fr:br\nf:g\nh:\n  i:j" { YamlState.Default with CurrentIndentation = 0 } 
|> function Success (s,_,_) -> printYamlObj 0 s | Failure (msg, _ , _) -> msg
|> printfn "%s"