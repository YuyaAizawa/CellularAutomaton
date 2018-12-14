module CellularAutomaton exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, input, label, text, table, tr, td, h1)
import Html.Attributes exposing (type_, for)
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
  , rule : Rule
  }

type State
  = Alive
  | Dead

type alias Rule = (State, State, State) -> State

flip state =
  case state of
    Alive -> Dead
    Dead -> Alive

init : () -> ( Model, Cmd Msg )
init _ =
  (
    { origin = []
    , generations = 0
    , rule = ( \r ->
        case r of
          ( Dead,  Dead,  Dead) ->  Dead
          ( Dead,  Dead, Alive) -> Alive
          ( Dead, Alive,  Dead) -> Alive
          ( Dead, Alive, Alive) -> Alive
          (Alive,  Dead,  Dead) -> Alive
          (Alive,  Dead, Alive) ->  Dead
          (Alive, Alive,  Dead) ->  Dead
          (Alive, Alive, Alive) ->  Dead
      )
    }
  , Cmd.none
  )



---- UPDATE ----

type Msg
  = NoOp
  | ColumnNumber String
  | GenerationChanged String
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

    Flip index ->
      let
        origin =
          model.origin
            |> List.indexedMap (\i -> \s -> if i == index then flip s else s)
      in
      ( { model | origin = origin }, Cmd.none )

resize : Model -> Maybe Int -> Model
resize model size =
  { model | origin =
    size
      |> Maybe.map
        ( \s ->
          List.range 1 s
            |> List.map ( \_ -> Dead )
        )
      |> Maybe.withDefault model.origin
  }



---- VIEW ----

view : Model -> Html Msg
view model =
  div [] ( List.concat
    [ numberInput "セル数" ColumnNumber "cells"
    , numberInput "世代数" GenerationChanged "generations"
    , [ originView model.origin ]
    , [ descendantsView model.origin model.rule model.generations ]
    ]
  )

numberInput str dst name =
  [ label [ for name ][ text str ]
  , input [ type_ "number", onInput dst ][]
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

descendantsView : List State -> Rule -> Int -> Html Msg
descendantsView origin rule limit =
  let rterm = Dead in
  let lterm = Dead in
  let
    step : List State -> List State
    step gen =
      let
        inner ngen l c pgen =
          case pgen |> List.head of
            Nothing ->
              rule (l, c, rterm) :: ngen
                |> List.reverse
            Just r ->
              inner ((rule (l, c, r)) :: ngen) c r (pgen |> List.tail |> Maybe.withDefault [])
      in
        case gen of
          [] ->
            []
          c :: [] ->
            inner [] lterm c []
          l :: c :: rest ->
            inner [] lterm l (c :: rest)
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