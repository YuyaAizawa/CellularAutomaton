module Styles exposing (cell, alive, dead, originTable, resultTable, ruleEditor, ruleDetail)

import Html.Attributes exposing (style)

cell =
  [ style "border" "solid 1px black"
  , style "width" "1em"
  , style "height" "1em"
  ]

alive =
  [ style "background" "black" ]

dead =
  [ style "background" "white" ]

originTable =
  [ style "border" "solid 2px gray"]

resultTable =
  []

ruleDetail =
  [ style "display" "flex"
  , style "flex-flow" "row wrap"
  ]

ruleEditor =
  [ style "display" "flex"
  , style "flex-flow" "column nowrap"
  , style "justify-content" "center"
  , style "align-items" "center"
  , style "margin" "0.3em"
  ]
