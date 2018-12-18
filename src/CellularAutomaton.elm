module CellularAutomaton exposing (Model, Msg(..), init, main, update, view)

import Bitwise
import Browser
import Html exposing (Html, div, input, label, text, table, tr, td, h1, br, button)
import Html.Attributes exposing (type_, for, value, name, id, checked)
import Html.Events exposing (onInput, onClick)

import Reference exposing (Reference)
import Reference.List

import Styles

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }



---- MODEL ----

type alias Model =
  { origin : List State
  , generations : Int
  , rule : Int
  , edge : EdgeType
  , ruleDetail : Bool
  }

type State
  = Alive
  | Dead

type alias Rule = (State, State, State) -> State

type EdgeType
  = AlwaysDead
  | AlwaysAlive
  | Loop

init : Model
init =
  { origin = [Dead, Alive, Dead, Dead]
  , generations = 6
  , rule = 30
  , ruleDetail = False
  , edge = AlwaysDead
  }

flip state =
  case state of
    Alive -> Dead
    Dead -> Alive

toState bool =
  if bool then Alive else Dead

ruleFromInt : Int -> Rule
ruleFromInt index =
  ( \pgen ->
    case pgen of
      ( Dead,  Dead,  Dead) -> bitTest 0 index
      ( Dead,  Dead, Alive) -> bitTest 1 index
      ( Dead, Alive,  Dead) -> bitTest 2 index
      ( Dead, Alive, Alive) -> bitTest 3 index
      (Alive,  Dead,  Dead) -> bitTest 4 index
      (Alive,  Dead, Alive) -> bitTest 5 index
      (Alive, Alive,  Dead) -> bitTest 6 index
      (Alive, Alive, Alive) -> bitTest 7 index
  )

bitTest : Int -> Int -> State
bitTest int index =
  let
    bool =
      index
        |> Bitwise.shiftRightBy int
        |> Bitwise.and 1
        |> (==) 1
  in
    if bool then Alive else Dead


---- UPDATE ----

type Msg
  = ColumnChange Int
  | GenerationChange Int
  | EdgeChange EdgeType
  | RuleChange Int
  | DetailVisibility
  | RuleFlip Int
  | OriginFlip (Reference State (List State))

update : Msg -> Model -> Model
update msg model =
  case msg of
    ColumnChange columns ->
      resize model columns

    GenerationChange generations ->
      { model | generations = generations }

    EdgeChange edge ->
      { model | edge = edge }

    RuleChange rule ->
      { model | rule = rule }

    DetailVisibility ->
      { model | ruleDetail = not model.ruleDetail }

    RuleFlip bit ->
      { model | rule = model.rule |> Bitwise.xor (Bitwise.shiftLeftBy bit 1) }

    OriginFlip ref ->
      { model | origin = ref |> Reference.modify flip |> Reference.root }

resize : Model -> Int -> Model
resize model size =
  let currentSize = model.origin |> List.length in
  let
    nextOrigin =
      if currentSize < size
      then List.append model.origin (Dead |> List.repeat (size - currentSize))
      else List.take size model.origin
  in
    { model | origin = nextOrigin }



---- VIEW ----

view : Model -> Html Msg
view model =
  div [] ( List.concat
    [ [h1[][text "せるお～とまとん"]]
    , numberInput "ルール #" "rule" RuleChange model.rule 0 255
    , [button[onClick DetailVisibility][text "詳細"]]
    , ruleDetailView model.ruleDetail model.rule
    , [br[][]]
    , numberInput "セル数" "cells" ColumnChange (model.origin |> List.length) 1 100
    , [br[][]]
    , numberInput "世代数" "generations" GenerationChange model.generations 1 300
    , [br[][]]
    , radioButtons "末端処理" "edge" EdgeChange model.edge
        [ ("死亡", AlwaysDead)
        , ("生存", AlwaysAlive)
        , ("末端をループ", Loop)
        ]
    , [ originView model.origin ]
    , [ descendantsView model.origin model.edge (ruleFromInt model.rule) model.generations ]
    ]
  )

numberInput : String -> String -> (Int -> msg) -> Int -> Int -> Int -> List (Html msg)
numberInput title name toMsg value min max =
  [ label [ for name ][ text title ]
  , input
      [ type_ "number"
      , Html.Attributes.value <| String.fromInt <| value
      , Html.Attributes.min <| String.fromInt <| min
      , Html.Attributes.max <| String.fromInt <| max
      , onInput (String.toInt >> Maybe.withDefault value >> toMsg) ][]
  ]

radioButtons : String -> String -> (a -> msg) -> a -> List (String, a) -> List (Html msg)
radioButtons title group toMsg selected candidates  =
  (::)
   (label [ for group ][ text title ])
   (candidates
    |> List.map (radioButton group toMsg selected)
    |> List.concat)

radioButton : String -> (a -> msg) -> a -> (String, a) -> List (Html msg)
radioButton group toMsg selected (name_, candidate) =
  [ input
    [ type_ "radio"
    , name group
    , id name_
    , onClick (candidate |> toMsg)
    , checked (candidate == selected)][]
  , label [ for name_ ][ text name_ ]
  ]

ruleDetailView : Bool -> Int -> List (Html Msg)
ruleDetailView visible ruleIndex =
  if not visible
    then []
    else
      div Styles.ruleDetail
        (List.range 0 7
          |> List.map (editableRule ruleIndex))
        |> List.singleton

editableRule : Int -> Int -> Html Msg
editableRule ruleIndex bit =
  let
    cellStyle alive =
      List.append
        Styles.cell
        ( if alive then Styles.alive else Styles.dead )
  in
    div Styles.ruleEditor
      [ table Styles.resultTable
        [ tr[]
          [ td(Styles.cell ++ (bitTest 0 bit |> toStyle))[]
          , td(Styles.cell ++ (bitTest 1 bit |> toStyle))[]
          , td(Styles.cell ++ (bitTest 2 bit |> toStyle))[]
          ]
        ]
      , text "↓"
      , table Styles.originTable
        [tr[][ td(
          Styles.cell
          ++ [onClick (RuleFlip bit)]
          ++ (bitTest bit ruleIndex |> toStyle))[]
        ]]
      ]

originView : List State -> Html Msg
originView origin =
  let
    cellView : Reference State (List State) -> Html Msg
    cellView ref =
      td (List.concat
        [ Styles.cell
        , Reference.this ref |> toStyle
        , [ onClick (OriginFlip ref) ]
        ]) []
  in
    table Styles.originTable
      (Reference.top origin |> Reference.List.unwrap cellView)

descendantsView : List State -> EdgeType -> Rule -> Int -> Html Msg
descendantsView origin edgeType rule limit =
  let
    step : List State -> List State
    step gen =
      let
        getTerm farSide =
          case edgeType of
            AlwaysDead -> Dead
            AlwaysAlive -> Alive
            Loop -> farSide |> Maybe.withDefault Dead
      in
      let lterm = getTerm <| List.head <| List.reverse <| gen in
      let rterm = getTerm <| List.head <| gen in
        [lterm] ++ gen ++ [rterm]
          |> window3
          |> List.map rule
  in

  let
    generationView : List State -> Html Msg
    generationView cells =
      let
        cellView state =
          td (Styles.cell ++ (state |> toStyle)) []
      in
        tr [] ( cells
          |> List.map cellView )
  in

  let
    descendantsView_ trs lim gen =
      if lim == 0
      then
        trs |> List.reverse
      else
        descendantsView_ (generationView gen :: trs) (lim - 1) (step gen)
  in
    descendantsView_ [] limit origin
      |> table Styles.resultTable

toStyle state =
      case state of
        Alive -> Styles.alive
        Dead -> Styles.dead

window3 : List a -> List (a, a, a)
window3 list =
  let
    window3_ : List (a, a, a) -> List a -> List (a, a, a)
    window3_ result rest =
      case rest of
        hd1 :: hd2 :: hd3 :: tl ->
          window3_ ((hd1, hd2, hd3) :: result) (hd2 :: hd3 :: tl)
        _ -> result
  in
    window3_ [] list |> List.reverse