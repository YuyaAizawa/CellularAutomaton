module CellularAutomaton exposing (Model, Msg(..), init, main, update, view)

import Bitwise
import Browser
import Html exposing (Html, div, input, label, text, table, tr, td, h1, br)
import Html.Attributes exposing (type_, for, value, name, id, checked)
import Html.Events exposing (onInput, onClick)

import Styles

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



---- MODEL ----

type alias Model =
  { origin : List State
  , generations : Int
  , rule : Int
  , edge : EdgeType
  }

type State
  = Alive
  | Dead

type alias Rule = (State, State, State) -> State

type EdgeType
  = AlwaysDead
  | AlwaysAlive
  | Loop

flip state =
  case state of
    Alive -> Dead
    Dead -> Alive

init : () -> ( Model, Cmd Msg )
init _ =
  (
    { origin = [Dead, Alive, Dead, Dead]
    , generations = 6
    , rule = 30
    , edge = AlwaysDead
    }
  , Cmd.none
  )

ruleFromInt : Int -> Rule
ruleFromInt index =
  let bitAt i = index |> Bitwise.shiftRightBy i |> Bitwise.and 1 |> (==) 1 in
  ( \r ->
    case r of
      ( Dead,  Dead,  Dead) -> if bitAt 0 then Alive else Dead
      ( Dead,  Dead, Alive) -> if bitAt 1 then Alive else Dead
      ( Dead, Alive,  Dead) -> if bitAt 2 then Alive else Dead
      ( Dead, Alive, Alive) -> if bitAt 3 then Alive else Dead
      (Alive,  Dead,  Dead) -> if bitAt 4 then Alive else Dead
      (Alive,  Dead, Alive) -> if bitAt 5 then Alive else Dead
      (Alive, Alive,  Dead) -> if bitAt 6 then Alive else Dead
      (Alive, Alive, Alive) -> if bitAt 7 then Alive else Dead
  )


---- UPDATE ----

type Msg
  = NoOp
  | ColumnNumber String
  | GenerationChanged String
  | EdgeChanged EdgeType
  | RuleChanged String
  | Flip Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> ( model, Cmd.none )

    ColumnNumber str ->
      let
        nextModel =
          str
            |> String.toInt
            |> resize model
      in
      ( nextModel, Cmd.none )

    GenerationChanged str ->
      let
        nextGeneration =
          str
            |> String.toInt
            |> Maybe.withDefault model.generations
      in
      ( { model | generations = nextGeneration }, Cmd.none )

    EdgeChanged edge ->
      ( { model | edge = edge }, Cmd.none )

    RuleChanged rule ->
      ( { model | rule = rule |> String.toInt |> Maybe.withDefault model.rule }, Cmd.none )

    Flip index ->
      let
        origin =
          model.origin
            |> List.indexedMap (\i -> \s -> if i == index then flip s else s)
      in
      ( { model | origin = origin }, Cmd.none )

resize : Model -> Maybe Int -> Model
resize model size =
  let currentSize = model.origin |> List.length in
  let nextSize = size |> Maybe.withDefault currentSize in
  let
    nextOrigin =
      if currentSize < nextSize
      then List.append model.origin (Dead |> List.repeat (nextSize - currentSize))
      else List.take nextSize model.origin
  in
    { model | origin = nextOrigin }



---- VIEW ----

view : Model -> Html Msg
view model =
  div [] ( List.concat
    [ numberInput "セル数" "cells" (model.origin |> List.length) 1 100
        |> (List.map << Html.map) ColumnNumber
    , [br[][]]
    , numberInput "世代数" "generations" model.generations 1 300
        |> (List.map << Html.map) GenerationChanged
    , [br[][]]
    , numberInput "ルール #" "rule" model.rule 0 255
        |> (List.map << Html.map) RuleChanged
    , [br[][]]
    , radioButtons "末端処理" "edge" model.edge
        [ ("死亡", AlwaysDead)
        , ("生存", AlwaysAlive)
        , ("末端をループ", Loop)
        ]
        |> (List.map << Html.map) EdgeChanged

    , [ originView model.origin ]
    , [ descendantsView model.origin model.edge (ruleFromInt model.rule) model.generations ]
    ]
  )

numberInput : String -> String -> Int -> Int -> Int -> List (Html String)
numberInput title name value min max =
  [ label [ for name ][ text title ]
  , input
      [ type_ "number"
      , Html.Attributes.value (value |> String.fromInt)
      , Html.Attributes.min (min |> String.fromInt)
      , Html.Attributes.max (max |> String.fromInt)
      , onInput identity ][]
  ]

radioButtons : String -> String -> msg -> List (String, msg) -> List (Html msg)
radioButtons title group selected candidates =
  (::)
   (label [ for group ][ text title ])
   (candidates
    |> List.map (radioButton group selected)
    |> List.concat)

radioButton : String -> msg -> (String, msg) -> List (Html msg)
radioButton group selected (name_, msg) =
  [ input [ type_ "radio", name group, id name_, onClick msg, checked (msg == selected)][]
  , label [ for name_ ][ text name_ ]
  ]

originView : List State -> Html Msg
originView origin =
  let
    cellView : Int -> State -> Html Msg
    cellView index state =
      td (List.concat
        [ Styles.cell
        , case state of
            Alive -> Styles.alive
            Dead -> Styles.dead
        , [ onClick (Flip index) ]
        ]) []
  in
    table
      Styles.originTable
      ( origin |> List.indexedMap cellView )

descendantsView : List State -> EdgeType -> Rule -> Int -> Html Msg
descendantsView origin edgeType rule limit =
  let
    step : List State -> List State
    step gen =
      let
        getLterm cells =
          case edgeType of
            AlwaysDead -> Dead
            AlwaysAlive -> Alive
            Loop ->
              cells
                |> List.reverse
                |> List.head
                |> Maybe.withDefault Dead
      in
      let
        getRterm cells =
          case edgeType of
            AlwaysDead -> Dead
            AlwaysAlive -> Alive
            Loop ->
              cells
                |> List.head
                |> Maybe.withDefault Dead
      in

      let
        inner ngen l c pgen =
          case pgen |> List.head of
            Nothing ->
              rule (l, c, getRterm gen) :: ngen
                |> List.reverse
            Just r ->
              inner ((rule (l, c, r)) :: ngen) c r (pgen |> List.tail |> Maybe.withDefault [])
      in
        case gen of
          [] ->
            []
          c :: [] ->
            inner [] (getLterm gen) c []
          l :: c :: rest ->
            inner [] (getLterm gen) l (c :: rest)
  in

  let
    generationView : List State -> Html Msg
    generationView cells =
      let
        cellView cell =
          td (List.concat
          [ Styles.cell
          , case cell of
              Alive -> Styles.alive
              Dead -> Styles.dead
          , []
          ]) []
      in
        tr [] ( cells
          |> List.map cellView )
  in

  let
    inner trs lim gen =
      if lim == 0
      then
        trs |> List.reverse
      else
        inner (generationView gen :: trs) (lim - 1) (step gen)
  in
    inner [] limit origin
      |> table Styles.resultTable



subscriptions : Model -> Sub Msg
subscriptions model = Sub.none