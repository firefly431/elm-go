import Html
import Html.App as App
import Html.Attributes as HtmlAt
import Html.Keyed as HtmlK
import Json.Decode as JsonD exposing (..)
import Svg
import Svg.Keyed as SvgK
import Svg.Attributes as SvgAt
import Svg.Events as SvgEv
import String
import Array

bg_color : String
bg_color = "#f2b06d"

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
    grid: Grid,
    turn: Color -- cilcking will create this color
}

type alias Model = {
    size: Int,
    state: GameState
}

createGrid : Int -> Grid
createGrid size = Array.repeat size (Array.repeat size Nothing)

initialState : Int -> GameState
initialState size = { grid = createGrid size, turn = Black }

createModel : Int -> Model
createModel size = { size = size, state = initialState size }

model : Model
model = createModel 19

type Msg = Click Int Int

-- returns Nothing if suicide
clickGrid : Int -> Int -> Color -> Grid -> Maybe Grid
clickGrid x y color grid = let pos = (x, y) in case gridGet grid pos of
    Nothing -> Nothing -- somehow clicked outside the board
    Just (Just _) -> Nothing -- already placed
    Just Nothing -> case gridSet grid pos (Just color)
        |> (\og -> List.foldl (captureHelper2 (otherColor color)) (False, og) (neighbors pos)) of
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
captureHelper2 color pos (done, grid) = case capture color grid pos of
    Just ng -> (True, ng)
    Nothing -> (done, grid)

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

click : Int -> Int -> GameState -> GameState
click x y state = case clickGrid x y state.turn state.grid of
    Just ng -> { state | turn = otherColor state.turn, grid = ng }
    Nothing -> state

update : Msg -> Model -> Model
update msg model =
    case msg of
        Click x y -> { model | state = click x y model.state }

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
        SvgEv.onClick (Click x y)
    ] []) [0..size - 1]) [0..size - 1]
    |> Svg.g []

generateCircles : Grid -> Svg.Svg msg
generateCircles grid = List.indexedMap (\yz row -> List.indexedMap (\xz ->
    Maybe.map (\stone -> (colorToChar stone ++ toString xz ++ "," ++ toString yz,
        Svg.circle [SvgAt.cx (toString (toFloat xz + 0.5)), SvgAt.cy (toString (toFloat yz + 0.5)), SvgAt.r "0.40",
                    SvgAt.stroke "black", SvgAt.strokeWidth "0.1", SvgAt.fill (colorToString stone)] [])))
        (Array.toList row) |> List.filterMap identity) (Array.toList grid)
    |> List.concat
    |> SvgK.node "g" []

view : Model -> Html.Html Msg
view model =
    HtmlK.node "div" [] [
    ("svg", SvgK.node "svg" [
        SvgAt.viewBox ("0 0 " ++ (toString model.size) ++ " " ++ (toString model.size)),
        SvgAt.width "380", SvgAt.height "380"] [
        ("bgrect", Svg.rect [SvgAt.x "0", SvgAt.y "0", SvgAt.width (toString model.size), SvgAt.height (toString model.size), SvgAt.fill bg_color] []),
        ("grid", generateGrid model.size),
        ("stars", generateStars model.size),
        ("circles", generateCircles model.state.grid),
        ("targets", generateTargets model.size)
    ]),
    ("status", Html.div [] [ Html.text ("It is " ++ colorToString model.state.turn ++ "'s turn") ])
    ]
