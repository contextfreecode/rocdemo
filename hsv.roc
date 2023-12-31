app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, pf.Task] # pf.Task.{ await }
    provides [main] to pf

Point3 : (Frac F32, Frac F32, Frac F32)

Color : [
    Hsv Point3,
    Rgb Point3,
    NamedColor Str,
]

namedColors : Dict Str [Rgb Point3]
namedColors =
    Dict.empty {}
    |> Dict.insert "red" (Rgb (1, 0, 0))
    |> Dict.insert "yellow" (Rgb (1, 1, 0))
    |> Dict.insert "blue" (Rgb (0, 0, 1))

hsvToRgb : Point3 -> Point3
hsvToRgb = \(h, s, v) ->
    c = s * v
    h1 = h * 6.0
    x = c * (1 - (Num.abs ((fracRem h1 2) - 1)))
    (r, g, b) =
        when {} is
            _ if h1 < 1 -> (c, x, 0)
            _ if h1 < 2 -> (x, c, 0)
            _ if h1 < 3 -> (0, c, x)
            _ if h1 < 4 -> (0, x, c)
            _ if h1 < 5 -> (x, 0, c)
            _ -> (c, 0, x)
    m = v - c
    (r + m, g + m, b + m)

colorToRgb : Color -> Result [Rgb Point3] [KeyNotFound]
colorToRgb = \color ->
    when color is
        Rgb rgb -> Ok (Rgb rgb)
        Hsv hsv -> Ok (Rgb (hsvToRgb hsv))
        NamedColor name -> Dict.get namedColors name

main : Task.Task {} I32
main =
    colors = [
        Rgb (1, 0.1, 0.1),
        NamedColor "yellow",
        Hsv (2 / 3, 5 / 6, 1),
        NamedColor "whatever",
    ]
    List.walkWithIndex colors (Task.ok {}) \task, color, index ->
        _ <- Task.await task
        rgb = colorToRgb color
        Stdout.line "Color: \(Num.toStr index): \(maybeColorToStr rgb)"

# Helpers.

colorToStr : [Rgb Point3] -> Str
colorToStr = \color ->
    when color is
        Rgb rgb -> "Rgb \(point3ToStr rgb)"
        # Hsv hsv -> "Hsv \(point3ToStr hsv)"
        # NamedColor name -> "NamedColor \(name)"

fracRem : Frac a, Frac a -> Frac a
fracRem = \x, y ->
    quotient = x / y
    quotient - Num.toFrac (Num.floor quotient)

maybeColorToStr : Result [Rgb Point3] [KeyNotFound] -> Str
maybeColorToStr = \maybeColor ->
    when maybeColor is
        Ok color -> colorToStr color
        Err KeyNotFound -> "KeyNotFound"

point3ToStr = \(x, y, z) ->
    joined =
        List.map [x, y, z] \n -> Num.toStr n
        |> Str.joinWith ", "
    "(\(joined))"
