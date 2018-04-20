module Spec.Content.Meals where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.TextField (textField)
import MaterialUI.TextField as TextField
import MaterialUI.Divider (divider)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.Input (inputAdornment)
import MaterialUI.Input as Input
import MaterialUI.Chip (chip)
import MaterialUI.Paper (paper)
import MaterialUI.Collapse (collapse)



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
        (\_ -> {paper: createStyles {position: "relative", width: "200px", zIndex: 1000, height: "30em"}})
        \{classes} -> drawer
          { variant: Drawer.permanent
          , anchor: Drawer.left
          , classes: Drawer.createClasses classes
          }
          [ textField
            { label: R.text "Search"
            , "InputProps":
              { startAdornment:
                inputAdornment
                  { position: Input.start
                  }
                  searchIcon
              }
            } []
          , R.div
            [RP.style {height: "5em", overflowY: "hidden"}]
            [ chip
              { label: R.text "One"
              }
            , chip
              { label: R.text "Two"
              }
            , chip
              { label: R.text "Three"
              }
            , chip
              { label: R.text "Four"
              }
            ]
          , divider {}
          , typography
            { variant: Typography.subheading
            } [R.text "Suggested"]
          , R.div
            [RP.style {height: "5em", overflowY: "hidden"}]
            [ chip
              { label: R.text "One"
              }
            , chip
              { label: R.text "Two"
              }
            , chip
              { label: R.text "Three"
              }
            , chip
              { label: R.text "Four"
              }
            ]
          , divider {}
          , typography
            { variant: Typography.subheading
            } [R.text "Delivering"]
          , R.div []
            [ button
              { variant: Button.raised
              }
              [R.text "Before"]
            , button
              { variant: Button.raised
              , disabled: true
              }
              [R.text "After"]
            ]
          ]
      ]


meals :: R.ReactElement
meals =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
