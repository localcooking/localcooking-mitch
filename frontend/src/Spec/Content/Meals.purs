module Spec.Content.Meals where

import Spec.Tag (tag)
import Spec.Dialogs.Datepicker (datepicker, initDatepicked)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Date (Date, diff)
import Data.Date.Extra (humanReadableDuration, plusTwoWeeks)
import Data.Time.Duration (Days (..))
import Data.DateTime.Locale (LocalValue (..))
import Data.Int as Int
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.TextField (textField)
import MaterialUI.Divider (divider)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Input (inputAdornment)
import MaterialUI.Input as Input
import MaterialUI.Paper (paper)

import Queue.One.Aff as OneIO




type State =
  { datepicked :: Date
  }

initialState :: {initDatepicked :: Date} -> State
initialState {initDatepicked} =
  { datepicked: initDatepicked
  }

data Action
  = ClickedOpenDatepicker


type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { pickDate :: OneIO.IOQueues (Effects eff) Unit Date
        }
     -> T.Spec (Effects eff) State Unit Action
spec {pickDate} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ClickedOpenDatepicker -> do
        date <- liftBase (OneIO.callAsync pickDate unit)
        void $ T.cotransform _ { datepicked = date }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ datepicker {pickDate}
      , typography
        { variant: Typography.display1
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Meals"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , R.div [RP.style {position: "relative", width: "100%"}]
        [ Drawer.withStyles
          (\_ ->
            { paper: createStyles
              { position: "relative"
              , width: "200px"
              , zIndex: 1000
              , height: "30em"
              , paddingRight: "1em"
              }
            })
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
              [ RP.style
                { height: "15em"
                , overflowY: "hidden"
                , padding: "0.5em"
                }
              ] $
              map (\label -> tag {label,onClick: Just (pure unit), onDelete: Nothing})
                [ "One"
                , "Two"
                , "Three"
                , "Four"
                , "Foo"
                , "Bar"
                , "Baz"
                , "Qux"
                ]
            , divider {}
            , typography
              { variant: Typography.subheading
              } [R.text "Delivering"]
            , R.div []
              [ R.div [RP.style {marginBottom: "1em"}]
                [ button
                  { variant: Button.raised
                  , style: createStyles {width: "50%"}
                  }
                  [R.text "Before"]
                , button
                  { variant: Button.raised
                  , disabled: true
                  , style: createStyles {width: "50%"}
                  }
                  [R.text "After"]
                ]
              , button
                { variant: Button.raised
                , fullWidth: true
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedOpenDatepicker
                } [ R.text $ show $ humanReadableDuration $
                    let Days x = diff state.datepicked today
                    in  Int.floor x
                  , R.br [] []
                  , R.text "From Now"]
              ]
            ]
        , R.div
          [ RP.style {position: "absolute", left: "200px", top: "1em", paddingLeft: "1em", width: "100%", maxWidth: "1048px"}
          ]
          [ paper
            { style: createStyles
              { width: "100%"
              , padding: "0.5em"
              , marginBottom: "1em"
              , display: "flex"
              }
            } $
            map (\label -> tag {label,onDelete: Just (pure unit), onClick: Nothing})
              [ "One"
              , "Two"
              , "Three"
              , "Four"
              ]
          , grid {container: true, spacing: Grid.spacing8}
            [ grid {item: true, xs: 4}
              [ paper {style: createStyles {width: "100%", padding: "0.5em"}}
                [ typography
                  { variant: Typography.headline
                  } [R.text "Wienerschnitzel"]
                , typography
                  { variant: Typography.body1
                  } [R.text "Foo"]
                ]
              ]
            ]
          ]
        ]
      ]

    today = unsafePerformEff $ do
      LocalValue _ d <- nowDate
      pure d


meals :: R.ReactElement
meals =
  let init =
        { initDatepicked: unsafePerformEff initDatepicked
        }
      {spec: reactSpec, dispatcher} = T.createReactSpec (spec {pickDate}) (initialState init)
  in  R.createElement (R.createClass reactSpec) unit []
  where
    pickDate = unsafePerformEff OneIO.newIOQueues
