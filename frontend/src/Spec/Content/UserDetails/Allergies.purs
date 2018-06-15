module Spec.Content.UserDetails.Allergies where

import Prelude

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R


type State = Unit

initialState :: State
initialState = unit

type Action = Unit


spec :: forall eff. T.Spec eff State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ R.text "Allergies" ]


allergies :: R.ReactElement
allergies =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
