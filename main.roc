app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }]
    provides [main] to pf

Maybe elem : Result elem {}
Doc : { head : Maybe Head }
Head : { title : Maybe Str }
Summary : { title : Maybe Str, ok : Bool }
Error : Str

readDoc : Str -> Result Doc Error
readDoc = \url ->
    has = \text -> Str.contains url text
    if has "fail" then Err "Bad read of \(url)"
    else Ok (
        when url is
            _ if has "head-missing" -> { head: Err {} }
            _ if has "title-missing" -> { head: Ok { title: Err {} } }
            _ if has "title-empty" -> { head: Ok { title: Ok "" } }
            _ -> { head: Ok { title: Ok "Title of \(url)" } }
    )

main =
    urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
    List.walk urls (Task.ok {}) \task, url ->
        _ <- await task
        doc = readDoc url
        Stdout.line "Also \(url)"
