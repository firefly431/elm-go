import Html
import Html.App as App
import Html.Attributes as HtmlAt
import Html.Events as HtmlEv
import Html.Keyed as HtmlK
import Json.Decode as JsonD exposing ((:=))
import Svg
import Svg.Keyed as SvgK
import Svg.Attributes as SvgAt
import Svg.Events as SvgEv
import Char
import String
import Array

-- TODO: resign button
--       results in SGF

bg_color : String
bg_color = "#f2b06d"

sizes : List Int
sizes = [9, 13, 19]

main = App.beginnerProgram { model = model, view = view, update = update }

type Color = Black | White

otherColor : Color -> Color
otherColor c = case c of
    Black -> White
    White -> Black

colorToChar : Color -> String
colorToChar c = case c of
    Black -> "B"
    White -> "W"

colorToString : Color -> String
colorToString c = case c of
    Black -> "black"
    White -> "white"

type alias Grid = Array.Array (Array.Array (Maybe Color))

type alias GameState = {
    size: Int,
    grid: Grid,
    turn: Color, -- clicking will create this color
    last: Grid,
    history: List ((Int, Int), Color) -- backwards
}

type alias Model = {
    state: GameState,
    hover: (Int, Int),
    scoring: Bool,
    orig: Grid, -- store original board so can revert after scoring
    komi: Float
}

createGrid : Int -> Grid
createGrid size = Array.repeat size (Array.repeat size Nothing)

initialState : Int -> GameState
initialState size = let empty = createGrid size in {
    grid = empty,
    turn = Black,
    last = empty,
    history = [],
    size = size}

replay : Int -> List ((Int, Int), Color) -> Maybe GameState
replay size history =
    case history of
        first :: rest ->
            let play move (_, gr) = uncurry clickGrid move gr |> Maybe.map (\ng -> (gr, ng))
                empty = createGrid size
            in
                List.foldr (flip Maybe.andThen << play) ((empty, empty) |> Just) history
                |> Maybe.map (\(last, grid) -> {
                grid = grid,
                turn = first |> snd |> otherColor,
                last = last,
                history = history,
                size = size})
        [] -> Just (initialState size)
createModel : Int -> Model
createModel size = let is = initialState size
                   in { state = is, hover = (-1, -1), scoring = False, orig = is.grid, komi = 7.5 }

model : Model
model = createModel 19

type Msg = Click (Int, Int)
         | Hover (Int, Int)
         | Pass
         | Undo
         | ScoreChk Bool
         | ChangeKomi String
         | ChangeSize Int

-- returns Nothing if invalid (does not check for ko)
clickGrid : (Int, Int) -> Color -> Grid -> Maybe Grid
clickGrid pos color grid = case gridGet grid pos of
    Nothing -> Nothing -- somehow clicked outside the board
    Just (Just _) -> Nothing -- already placed
    Just Nothing -> case let og = gridSet grid pos (Just color) in
            List.foldl (captureHelper2 (otherColor color)) (False, og) (neighbors pos) of
                (True, ng) -> Just ng
                (False, og) ->
                    -- check if suicide
                    case captureHelper2 color pos (False, og) of
                        (True, _) -> Nothing
                        (False, _) -> Just og

gridGet : Grid -> (Int, Int) -> Maybe (Maybe Color)
gridGet grid (x, y) = Array.get y grid |> flip Maybe.andThen (\row -> Array.get x row)

gridSet : Grid -> (Int, Int) -> Maybe Color -> Grid
gridSet grid (x, y) color = let newrow : Maybe (Array.Array (Maybe Color))
                                newrow =
                                let oldrow : Maybe (Array.Array (Maybe Color))
                                    oldrow = Array.get y grid in
                                    Maybe.map (\row -> Array.set x color row) oldrow in
                                        Maybe.map (\nrow -> Array.set y nrow grid) newrow
                                        |> Maybe.withDefault grid

-- capture stones of given color (so other color's turn)
capture : Color -> Grid -> (Int, Int) -> Maybe Grid
capture color grid pos = captureHelper color grid pos grid

neighbors : (Int, Int) -> List (Int, Int)
neighbors (x, y) = [(x + 1, y), (x, y - 1), (x - 1, y), (x, y + 1)]

captureHelper2 : Color -> (Int, Int) -> (Bool, Grid) -> (Bool, Grid)
captureHelper2 color pos (done, grid) = case gridGet grid pos of
    Nothing -> (done, grid)
    Just c -> case c of
        Nothing -> (done, grid)
        Just cc -> if cc /= color then (done, grid) else
                      case capture color grid pos of
                          Nothing -> (done, grid)
                          Just ng -> (True, ng)

-- takes an accumulator (currently captured pieces)
captureHelper : Color -> Grid -> (Int, Int) -> Grid -> Maybe Grid
captureHelper color grid pos acc = case gridGet grid pos of
    Nothing -> Just acc -- out of bounds
    Just stone -> case stone of
        Nothing -> Nothing -- if there's a liberty, no capture
        Just scol -> if scol /= color then Just acc -- other color; we're OK but do nothing
                        else if gridGet acc pos == Just Nothing then Just acc -- already processed
                            else List.foldl
                                (captureHelperHelper color grid)
                                (gridSet acc pos Nothing |> Just)
                                (neighbors pos)

captureHelperHelper : Color -> Grid -> (Int, Int) -> Maybe Grid -> Maybe Grid
captureHelperHelper color grid pos macc = case macc of
    Nothing -> Nothing
    Just acc -> captureHelper color grid pos acc

click : (Int, Int) -> GameState -> GameState
click pos state = case clickGrid pos state.turn state.grid of
    Just ng -> if ng == state.last then state
               else { state | turn = otherColor state.turn,
                              grid = ng, last = state.grid,
                              history = (pos, state.turn) :: state.history }
    Nothing -> state

pass : GameState -> GameState
pass state = { state | turn = otherColor state.turn }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Click pos -> { model | state = (if model.scoring then scoreClick else click) pos model.state }
        Hover pos -> { model | hover = pos }
        Pass -> { model | state = pass model.state }
        Undo -> { model | state = case List.tail model.state.history of
            Just t -> case replay model.state.size t of
                Just st -> st
                Nothing -> model.state
            Nothing -> model.state }
        ScoreChk b -> let nstate = model.state in
            case b of
                False -> { model | scoring = False, state = { nstate | grid = model.orig } }
                True  -> { model | scoring = True, orig = model.state.grid,
                           state = { nstate | grid = fillDame nstate.size nstate.grid } }
        ChangeKomi k -> case String.toFloat k of
            Ok f -> { model | komi = f }
            Err _ -> model
        ChangeSize s -> createModel s

generateGrid : Int -> Svg.Svg msg
generateGrid size = let stroke = [ SvgAt.stroke "black", SvgAt.strokeWidth "0.1" ] in
    Svg.rect ([
        SvgAt.x "0.5", SvgAt.y "0.5", SvgAt.width (toString (size - 1)), SvgAt.height (toString (size - 1)),
        SvgAt.fill "none"
    ] ++ stroke) [] ::
        List.concatMap (\i -> let p = toFloat i - 0.5
                                  s = toFloat size - 0.5 in [
        Svg.line ([ SvgAt.x1 (toString p), SvgAt.y1 "0.5",
                    SvgAt.x2 (toString p), SvgAt.y2 (toString s) ] ++ stroke) [],
        Svg.line ([ SvgAt.x1 "0.5", SvgAt.y1 (toString p),
                    SvgAt.x2 (toString s), SvgAt.y2 (toString p) ] ++ stroke) []
        ]) [0..size - 1]
    |> Svg.g []

corners : Int -> Int -> List (Int, Int)
corners size border = [
    (border, border),
    (border, size - border + 1),
    (size - border + 1, border),
    (size - border + 1, size - border + 1)]

sides : Int -> Int -> List (Int, Int)
sides size border = [
    (border, (size + 1) // 2),
    ((size + 1) // 2, border),
    (size - border + 1, (size + 1) // 2),
    ((size + 1) // 2, size - border + 1)]

center : Int -> (Int, Int)
center size = ((size + 1) // 2, (size + 1) // 2)

generateStars : Int -> Svg.Svg msg
generateStars size = (case size of
        19 -> corners size 4 ++ sides size 4 |> (::) (center size)
        13 -> corners size 4 |> (::) (center size)
        9 -> corners size 3 |> (::) (center size)
        _ -> []
    )
    |> List.map (\(ix, iy) ->
        let x = toString (toFloat ix - 0.5)
            y = toString (toFloat iy - 0.5) in
                Svg.circle [ SvgAt.cx x, SvgAt.cy y, SvgAt.r "0.15" ] [] )
    |> Svg.g []

generateTargets : Int -> Svg.Svg Msg
generateTargets size = List.concatMap (\y -> List.map (\x -> Svg.rect [
        SvgAt.x (toString x), SvgAt.y (toString y),
        SvgAt.width "1", SvgAt.height "1",
        SvgAt.visibility "hidden",
        SvgAt.style "pointer-events: fill",
        SvgEv.onClick (Click (x, y)),
        SvgEv.onMouseOver (Hover (x, y))
    ] []) [0..size - 1]) [0..size - 1]
    |> Svg.g [ SvgEv.onMouseOut (Hover (-1, -1)) ]

circleAttributes : (Int, Int) -> Color -> List (Svg.Attribute msg)
circleAttributes (xz, yz) stone = [
    SvgAt.cx (toString (toFloat xz + 0.5)), SvgAt.cy (toString (toFloat yz + 0.5)), SvgAt.r "0.40",
    SvgAt.stroke "black", SvgAt.strokeWidth "0.1", SvgAt.fill (colorToString stone)]

generateCircles : Grid -> Svg.Svg msg
generateCircles grid = List.indexedMap (\yz row -> List.indexedMap (\xz ->
    Maybe.map (\stone -> (colorToChar stone ++ toString xz ++ "," ++ toString yz,
        Svg.circle (circleAttributes (xz, yz) stone) [])))
        (Array.toList row) |> List.filterMap identity) (Array.toList grid)
    |> List.concat
    |> SvgK.node "g" []

generateHover : GameState -> (Int, Int) -> Svg.Svg msg
generateHover state pos = case pos of
    (-1, -1) -> Svg.g [] []
    _ -> case gridGet state.grid pos of
        Just Nothing -> Svg.circle (SvgAt.opacity "0.5" :: circleAttributes pos state.turn) []
        _ -> Svg.g [] []

generateSGF : Int -> List ((Int, Int), Color) -> String
generateSGF size history = "(;FF[4]GM[1]SZ[" ++ toString size ++ "](" ++ generateSGFMoves history ++ "))"

toLetter : Int -> String
toLetter x = Char.fromCode (x + 97) |> String.fromChar

generateSGFMoves : List ((Int, Int), Color) -> String
generateSGFMoves history = case history of
    (((x, y), color) :: moves) -> generateSGFMoves moves ++ ";" ++ colorToChar color ++ "[" ++ toLetter x ++ toLetter y ++ "]"
    [] -> ""

generateMarker : Maybe ((Int, Int), Color) -> Svg.Svg msg
generateMarker mm = case mm of
    Nothing -> Svg.g [] []
    Just ((x, y), _) -> let fx = toFloat x + 0.5
                            fy = toFloat y + 0.5
                            sr = 0.15 in Svg.rect [SvgAt.fill "red",
        fx - sr |> toString |> SvgAt.x,
        fy - sr |> toString |> SvgAt.y,
        sr * 2 |> toString |> SvgAt.width,
        sr * 2 |> toString |> SvgAt.height] []

scoreClick : (Int, Int) -> GameState -> GameState
scoreClick pos state = case gridGet state.grid pos of
    Nothing -> state
    Just mc -> case mc of
        Nothing -> { state | -- filling dame
            turn = otherColor state.turn,
            grid = gridSet state.grid pos (Just state.turn)
                   |> flip (List.foldl (floodfillColor state.turn)) (neighbors pos) }
        Just c -> { state | grid = floodfillRemove c pos state.grid
                                |> floodfillColor (otherColor c) pos } -- floodfill remove

floodfillRemove : Color -> (Int, Int) -> Grid -> Grid
floodfillRemove color pos grid = case gridGet grid pos of
    Nothing -> grid
    Just mc -> case mc of
        Nothing -> grid
        Just c -> if c /= color then grid else
                     List.foldl (floodfillRemove color) (gridSet grid pos Nothing) (neighbors pos)

floodfillColorHelper : Color -> (Int, Int) -> Grid -> Maybe Grid
floodfillColorHelper color pos grid = case gridGet grid pos of
    Nothing -> Just grid
    Just mc -> case mc of
        Just c -> if c == color then Just grid else Nothing
        Nothing -> List.foldl (flip Maybe.andThen << floodfillColorHelper color) (gridSet grid pos (Just color) |> Just) (neighbors pos)

floodfillColor : Color -> (Int, Int) -> Grid -> Grid
floodfillColor color pos grid = floodfillColorHelper color pos grid |> Maybe.withDefault grid

floodfillAny : (Int, Int) -> Grid -> Grid
floodfillAny pos grid = let ff c = floodfillColorHelper c pos grid in
                        List.map ff [Black, White] |> Maybe.oneOf |> Maybe.withDefault grid

fillDame : Int -> Grid -> Grid
fillDame size =
    List.concatMap (\y -> List.map ((,) y) [0..size - 1]) [0..size - 1]
    |> flip (List.foldl floodfillAny)

count : Grid -> (Int, Int) -- (B, W)
count grid = grid
    |> Array.toList
    |> List.concatMap Array.toList
    |> List.map (\mc -> case mc of
        Nothing -> (0, 0)
        Just Black -> (1, 0)
        Just White -> (0, 1))
    |> List.foldl (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) (0, 0)

radioSize : Int -> Int -> Html.Html Msg
radioSize set size = let btnId = "sz" ++ toString size in Html.span [] [
    Html.input [
        HtmlAt.type' "radio",
        HtmlAt.name "size",
        HtmlAt.id btnId,
        HtmlAt.checked (set == size),
        HtmlEv.onClick (ChangeSize size)] [],
    Html.label [ HtmlAt.for btnId ] [ Html.text (
        toString size ++ "x" ++ toString size )]]

view : Model -> Html.Html Msg
view model =
    HtmlK.node "div" [] [
    ("svg", SvgK.node "svg" [
        SvgAt.viewBox ("0 0 " ++ (toString model.state.size) ++ " " ++ (toString model.state.size)),
        SvgAt.width "380", SvgAt.height "380"] [
        ("bgrect", Svg.rect [SvgAt.x "0", SvgAt.y "0", SvgAt.width (toString model.state.size), SvgAt.height (toString model.state.size), SvgAt.fill bg_color] []),
        ("grid", generateGrid model.state.size),
        ("stars", generateStars model.state.size),
        ("circles", generateCircles model.state.grid),
        ("marker", generateMarker (if model.scoring then Nothing else List.head model.state.history)),
        ("hover", generateHover model.state model.hover),
        ("targets", generateTargets model.state.size)
    ]),
    ("controls", HtmlK.node "div" [] [
        ("sizepanel", HtmlK.node "div" [] [
            ("sizelabel", Html.text "Size:"),
            ("sizebuttons", sizes |> List.map (radioSize model.state.size) |> Html.div [])
        ]),
        ("pass", Html.button [ HtmlEv.onClick Pass ] [ Html.text "Pass" ]),
        ("undo", Html.button [ HtmlEv.onClick Undo, HtmlAt.disabled (model.scoring) ] [ Html.text "Undo" ])
    ]),
    ("scorepanel", HtmlK.node "div" [] [
        ("scoremode",
            Html.span [] [ Html.input [ HtmlAt.type' "checkbox", HtmlEv.on "change" (HtmlEv.targetChecked |> JsonD.map ScoreChk), HtmlAt.id "score" ] [ ],
                           Html.label [ HtmlAt.for "score" ] [ Html.text "Scoring mode" ] ]),
        ("komi", Html.div [] [ Html.label [ HtmlAt.for "komi"] [ Html.text "Komi: " ],
                               Html.input [ HtmlAt.type' "text", HtmlAt.id "komi",
                                            HtmlAt.value (toString model.komi),
                                            HtmlEv.on "keydown" (
                                                JsonD.object2 (,)
                                                    (JsonD.oneOf [
                                                        "key" := JsonD.string
                                                            |> JsonD.map ((==) "Enter"),
                                                        "keyIdentifier" := JsonD.string
                                                            |> JsonD.map ((==) "Enter"),
                                                        "keyCode" := JsonD.int
                                                            |> JsonD.map ((==) 13)])
                                                    HtmlEv.targetValue
                                                |> flip JsonD.andThen (\(b, v) -> if b then JsonD.succeed v else JsonD.fail "no string")
                                                |> JsonD.map ChangeKomi
                                                )
                                                ] [] ]),
        ("results", if model.scoring then
            let (b, w) = count model.state.grid in
                Html.div [] [
                    Html.p [] [ Html.text (
                        "Black: " ++ toString b ++ ", White: " ++ toString w
                    ) ],
                    Html.p [] [ Html.text (
                        let bs = toFloat b
                            ws = toFloat w + model.komi in
                                if bs > ws then "Black wins by " ++ toString (bs - ws)
                           else if ws > bs then "White wins by " ++ toString (ws - bs)
                                           else "Tie"
                    ) ]
                ]
            else Html.div [] [])
    ]),
    ("sgf", Html.textarea [ HtmlAt.readonly True ] [ Html.text (generateSGF model.state.size model.state.history) ])]
