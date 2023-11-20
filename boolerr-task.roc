app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, pf.Task] # pf.Task.{ await }
    provides [main] to pf

Maybe elem : Result elem {}
Doc : { head : Maybe Head }
Head : { title : Maybe Str }
Summary : { title : Maybe Str, ok : Bool }
Error : [BadRead Str]

readDoc : Str -> Task.Task Doc _
readDoc = \url ->
    has = \text -> Str.contains url text
    if has "fail" then
        Task.err (BadRead url)
    else
        doc =
            when {} is
                _ if has "head-missing" -> { head: Err {} }
                _ if has "title-missing" -> { head: Ok { title: Err {} } }
                _ if has "title-empty" -> { head: Ok { title: Ok "" } }
                _ -> { head: Ok { title: Ok "Title of \(url)" } }
        Task.ok doc

buildSummary : Doc -> Summary
buildSummary = \doc ->
    title = mapOkMaybe doc.head \head -> head.title
    { title, ok: Bool.true }

HandlingTask a b ok err : a, (b -> Task.Task ok err) -> Task.Task ok err

readAndBuildSummary : HandlingTask Str Summary _ _
readAndBuildSummary = \url, handle ->
    docResult <- Task.attempt (readDoc url)
    summary =
        when docResult is
            Ok doc -> buildSummary doc
            Err _ -> { title: Err {}, ok: Bool.false }
    handle summary

isTitleNonEmpty : Doc -> Maybe Bool
isTitleNonEmpty = \doc ->
    mapOkMaybe doc.head \head ->
        Result.map head.title \title ->
            !(Str.isEmpty title)

readWhetherTitleNonEmpty : Str -> Task.Task (Maybe Bool) _
readWhetherTitleNonEmpty = \url ->
    Task.map (readDoc url) \doc -> isTitleNonEmpty doc

main : Task.Task {} I32
main =
    urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
    List.walk urls (Task.ok {}) \task, url ->
        _ <- Task.await task
        _ <- Task.await (Stdout.line "Checking \"https://\(url)/\":")
        # Summary.
        summary <- readAndBuildSummary url
        _ <- Task.await (Stdout.line "  Summary: \(summaryToStr summary)")
        title = Result.withDefault summary.title ""
        _ <- Task.await (Stdout.line "  Title: \(title)")
        # Has title.
        hasTitle <- Task.attempt (readWhetherTitleNonEmpty url)
        hasTitleSure =
            Result.withDefault hasTitle (Ok Bool.false)
            |> Result.withDefault Bool.false
        hasTitleText = resultMaybeBoolToStr hasTitle
        Stdout.line "  Has title: \(hasTitleText) vs \(boolToStr hasTitleSure)"

# Helpers.

boolToStr : Bool -> Str
boolToStr = \bool -> if bool then "true" else "false"

mapOkMaybe : Result ok err, (ok -> Maybe ok2) -> Maybe ok2
mapOkMaybe = \result, transformOk ->
    Result.map result transformOk
    |> Result.withDefault (Err {})

maybeStrToStr : Maybe Str -> Str
maybeStrToStr = \maybe -> Result.withDefault maybe "null"

resultMaybeBoolToStr : Result (Maybe Bool) Error -> Str
resultMaybeBoolToStr = \result ->
    when result is
        Ok maybe ->
            content =
                Result.map maybe \bool -> boolToStr bool
                |> maybeStrToStr
            "(Ok \(content))"

        Err (BadRead url) -> "(BadRead \(url))"

summaryToStr : Summary -> Str
summaryToStr = \summary ->
    title = maybeStrToStr summary.title
    "{ title: \(title), ok: \(boolToStr summary.ok) }"

# On formatting, see also:
# https://github.com/roc-lang/roc/blob/6ccee5360a8d61a3f60b031d113502e08a894b1e/crates/compiler/builtins/roc/Inspect.roc
# https://github.com/roc-lang/roc/blob/6ccee5360a8d61a3f60b031d113502e08a894b1e/examples/inspect-logging.roc
# https://github.com/roc-lang/roc/blob/6ccee5360a8d61a3f60b031d113502e08a894b1e/examples/Community.roc#L94
