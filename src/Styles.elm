module Styles exposing (cell, alive, dead, originTable, resultTable)

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
