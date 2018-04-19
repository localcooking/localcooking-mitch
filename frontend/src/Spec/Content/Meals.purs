module Spec.Content.Meals where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.TextField (textField)
import MaterialUI.TextField as TextField
import MaterialUI.Divider (divider)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer



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
      [ typography
        { variant: Typography.display1
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Meals"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , Drawer.withStyles
        (\_ -> {paper: createStyles {position: "relative", width: "200px", zIndex: 1000}})
        \{classes} -> drawer
          { variant: Drawer.permanent
          , anchor: Drawer.left
          , classes: Drawer.createClasses classes
          }
          [ textField
            { select: true
            , label: R.text "Search Tags"
            , helperText: R.text "Available tags"
            }
            [ R.option
              []
              [R.text "Test"]
            ]
          ]
      ]


meals :: R.ReactElement
meals =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
