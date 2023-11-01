app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }]
    provides [main] to pf

main =
    _ <- await (Stdout.line "There are \(total) animals.")
    Stdout.line "Also \(stoplightStr) is a color."

# The `note` field is unused by addAndStringify
total = addAndStringify { birds: 4, iguanas: 3, note: "Whee!" }

addAndStringify = \{ birds, iguanas } ->
    Num.toStr (birds + iguanas)

stoplightColor = Red

stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green -> "green"
        Yellow -> "yellow"

# something =
#     List.map [StrElem "A", StrElem "b", NumElem 1, StrElem "c", NumElem -3] \elem ->
#         when elem is
#             NumElem num -> Num.isNegative num
#             StrElem str -> Str.isCapitalized str
