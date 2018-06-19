module Spec.Content.Meals where

import LocalCooking.Spec.Tag (tag, AnyTag (..))
import Spec.Snackbar (messages)
import Spec.Dialogs.Datepicker (datepicker, initDatepicked)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Date (Date, diff)
import Data.Date.Extra (humanReadableDuration)
import Data.Time.Duration (Days (..), Milliseconds (..))
import Data.DateTime.Locale (LocalValue (..))
import Data.Int as Int
import Data.Generic (class Generic, gEq)
import Data.String.Yarn (fromString)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (br, div, text, em) as R
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



data DatepickDirection
  = Before
  | After

derive instance genericDatepickDirection :: Generic DatepickDirection

instance eqDatepickDirection :: Eq DatepickDirection where
  eq = gEq



type State =
  { datepicked :: Date
  , datepickDirection :: DatepickDirection
  }

initialState :: {initDatepicked :: Date} -> State
initialState {initDatepicked} =
  { datepicked: initDatepicked
  , datepickDirection: Before
  }

data Action
  = ClickedOpenDatepicker
  | ChangedDatepickDirection DatepickDirection


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
        liftBase $ delay $ Milliseconds 300.0
        void $ T.cotransform _ { datepicked = date }
      ChangedDatepickDirection x ->
        void $ T.cotransform _ { datepickDirection = x }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ datepicker {pickDate, initDatepicked: state.datepicked}
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
              ]
              [ tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagDiet $ fromString "Paleo"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagDiet $ fromString "Vegan"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagDiet $ fromString "Gluten Free"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagIngredient $ fromString "Ginger"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagMeal $ fromString "Curry"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagIngredient $ fromString "Seafood"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagDiet $ fromString "Raw"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagCulture $ fromString "German"
                }
              , tag
                { onClick: Just (pure unit)
                , onDelete: Nothing
                , tag: TagCulture $ fromString "Nepalese"
                }
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
                  , disabled: state.datepickDirection == Before
                  , onTouchTap: mkEffFn1 \_ -> dispatch (ChangedDatepickDirection Before)
                  }
                  [R.text "Before"]
                , button
                  { variant: Button.raised
                  , style: createStyles {width: "50%"}
                  , disabled: state.datepickDirection == After
                  , onTouchTap: mkEffFn1 \_ -> dispatch (ChangedDatepickDirection After)
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
                  , R.text "From Now"
                  ]
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
            }
            [ tag
              { onDelete: Just (pure unit)
              , onClick: Nothing
              , tag: TagDiet $ fromString "Vegitarian"
              }
            , tag
              { onDelete: Just (pure unit)
              , onClick: Nothing
              , tag: TagIngredient $ fromString "Parmesan"
              }
            , tag
              { onDelete: Just (pure unit)
              , onClick: Nothing
              , tag: TagCulture $ fromString "Hebrew"
              }
            , tag
              { onDelete: Just (pure unit)
              , onClick: Nothing
              , tag: TagMeal $ fromString "Pasta"
              }
            ]
          , typography
            { variant: Typography.headline
            } [R.em [] [R.text "Under Construction"]]
            -- grid {container: true, spacing: Grid.spacing8}
            -- [ grid {item: true, xs: 4}
            --   [ paper {style: createStyles {width: "100%", padding: "0.5em"}}
            --     [ typography
            --       { tag: Typography.headline
            --       } [R.text "Wienerschnitzel"]
            --     , typography
            --       { tag: Typography.body1
            --       } [R.text "Foo"]
            --     ]
            --   ]
            -- ]
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
