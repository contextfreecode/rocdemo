app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, pf.Task] # pf.Task.{ await }
    provides [main] to pf

Maybe elem : Result elem {}
Doc : { head : Maybe Head }
Head : { title : Maybe Str }
Summary : { title : Maybe Str, ok : Bool }
Error : Str

readDoc : Str -> Result Doc Error
readDoc = \url ->
    has = \text -> Str.contains url text
    if has "fail" then
        Err "Bad read of \(url)"
    else
        doc =
            when url is
                _ if has "head-missing" -> { head: Err {} }
                _ if has "title-missing" -> { head: Ok { title: Err {} } }
                _ if has "title-empty" -> { head: Ok { title: Ok "" } }
                _ -> { head: Ok { title: Ok "Title of \(url)" } }
        Ok doc

buildSummary : Doc -> Summary
buildSummary = \doc ->
    title =
        when doc.head is
            Ok head -> head.title
            Err _ -> Err {}
    { title, ok: Bool.true }

readAndBuildSummary : Str -> Summary
readAndBuildSummary = \url ->
    when readDoc url is
        Ok doc -> buildSummary doc
        Err _ -> { title: Err {}, ok: Bool.false }

isTitleNonEmpty : Doc -> Maybe Bool
isTitleNonEmpty = \doc ->
    when doc.head is
        Ok head ->
            when head.title is
                Ok title -> Ok (!(Str.isEmpty title))
                Err _ -> Err {}

        Err _ -> Err {}

readWhetherTitleNonEmpty : Str -> Result (Maybe Bool) Error
readWhetherTitleNonEmpty = \url ->
    when readDoc url is
        Ok doc -> Ok (isTitleNonEmpty doc)
        Err err -> Err err

main : Task.Task {} I32
main =
    urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
    List.walk urls (Task.ok {}) \task, url ->
        _ <- Task.await task
        _ <- Task.await (Stdout.line "Checking \"https://\(url)/\":")
        # Summary.
        summary = readAndBuildSummary url
        # _ <- Task.await (Stdout.line "  Summary: \(summary)")
        title =
            when summary.title is
                Ok actualTitle -> actualTitle
                Err _ -> ""
        _ <- Task.await (Stdout.line "  Title: \(title)")
        # Has title.
        hasTitle = readWhetherTitleNonEmpty url
        hasTitleSure =
            when hasTitle is
                Ok hasTitleMaybe ->
                    when hasTitleMaybe is
                        Ok reallyHas -> reallyHas
                        Err _ -> Bool.false

                Err _ -> Bool.false
        # Stdout.line "  Has title: \(hasTitle) \(boolToStr hasTitleSure)"
        Stdout.line "  Has title: \(boolToStr hasTitleSure)"

boolToStr = \b -> if b then "true" else "false"
