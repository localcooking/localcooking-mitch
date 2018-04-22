module Spec.Content.Meals where

import Spec.Tag (tag)

import Prelude
import Data.Monoid ((<>))
import Data.Monoid.Endo (Endo (..))
import Data.Maybe (Maybe (..), fromJust)
import Data.Tuple (Tuple (..))
import Data.Date (Date, month, year, day, canonicalDate, weekday, lastDayOfMonth)
import Data.Date.Component (Month (January, December), Year, Weekday (..), Day)
import Data.Date.Extra (getCalendar)
import Data.DateTime.Locale (LocalValue (..))
import Data.Enum (class Enum, pred, succ, toEnum, fromEnum)
import Data.Array as Array
import Data.Unfoldable (unfoldr)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Now (nowDate)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Partial.Unsafe (unsafePartial)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Icons.ChevronLeft (chevronLeftIcon)
import MaterialUI.Icons.ChevronRight (chevronRightIcon)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.TextField (textField)
import MaterialUI.TextField as TextField
import MaterialUI.Divider (divider)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.IconButton (iconButton)
import MaterialUI.IconButton as IconButton
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Input (inputAdornment)
import MaterialUI.Input as Input
import MaterialUI.Chip (chip)
import MaterialUI.Paper (paper)
import MaterialUI.Table (table, tableRow, tableHead, tableCell, tableBody)
import MaterialUI.Table as Table
import MaterialUI.Collapse (collapse)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogActions (dialogActions)



type State =
  { datepickerDialog :: Boolean
  , datepicked :: Date
  }

initialState :: {initDatepicked :: Date} -> State
initialState {initDatepicked} =
  { datepickerDialog: false
  , datepicked: initDatepicked
  }

data Action
  = ClickedOpenDatepicker
  | ClickedCloseDatepicker
  | ClickedPrevMonth
  | ClickedNextMonth
  | ClickedDate Date


type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ClickedOpenDatepicker -> void $ T.cotransform _ { datepickerDialog = true }
      ClickedCloseDatepicker -> void $ T.cotransform _ { datepickerDialog = false }
      ClickedPrevMonth ->
        let m = case month state.datepicked of
              January -> December
              x -> unsafePartial $ fromJust $ pred x
            y = case month state.datepicked of
              January -> unsafePartial $ fromJust $ pred $ year state.datepicked
              _ -> year state.datepicked
        in  void $ T.cotransform _ { datepicked = canonicalDate y m (day state.datepicked) }
      ClickedNextMonth ->
        let m = case month state.datepicked of
              December -> January
              x -> unsafePartial $ fromJust $ succ x
            y = case month state.datepicked of
              December -> unsafePartial $ fromJust $ succ $ year state.datepicked
              _ -> year state.datepicked
        in  void $ T.cotransform _ { datepicked = canonicalDate y m (day state.datepicked) }
      ClickedDate d ->
        void $ T.cotransform _ { datepicked = d, datepickerDialog = false }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ dialog
        { open: state.datepickerDialog
        , onClose: mkEffFn1 \_ -> dispatch ClickedCloseDatepicker
        }
        [ dialogTitle {}
          []
        , dialogContent {}
          [ grid {container: true}
            [ grid {item: true, xs: 2}
              [iconButton {onTouchTap: mkEffFn1 \_ -> dispatch ClickedPrevMonth} chevronLeftIcon]
            , grid {item: true, xs: 8}
              [ typography
                { variant: Typography.headline
                , align: Typography.center
                } [ R.text $ show $ month state.datepicked
                  ]
              ]
            , grid {item: true, xs: 2}
              [iconButton {onTouchTap: mkEffFn1 \_ -> dispatch ClickedNextMonth} chevronRightIcon]
            , grid {item: true, xs: 12}
              [ table {} $
                let xs = getCalendar (year state.datepicked) (month state.datepicked)
                    week {sun,mon,tue,wed,thu,fri,sat} = tableRow {} $
                      let cell {current,day,month,year} =
                            Table.withStylesCell
                              (\_ -> { root: if current
                                                then createStyles {}
                                                else createStyles {background: "#aaa"}
                                     }
                              )
                              \{classes} ->
                              tableCell
                                { padding: Table.dense
                                , classes: Table.createClassesCell classes
                                , onClick: mkEffFn1 \_ -> dispatch $ ClickedDate $ canonicalDate year month day
                                } $ R.text $ show $ fromEnum day
                      in  [ cell sun
                          , cell mon
                          , cell tue
                          , cell wed
                          , cell thu
                          , cell fri
                          , cell sat
                          ]
                in  [ tableHead {}
                      [ tableRow {}
                        [ tableCell {padding: Table.dense} $ R.text "Sun"
                        , tableCell {padding: Table.dense} $ R.text "Mon"
                        , tableCell {padding: Table.dense} $ R.text "Tue"
                        , tableCell {padding: Table.dense} $ R.text "Wed"
                        , tableCell {padding: Table.dense} $ R.text "Thu"
                        , tableCell {padding: Table.dense} $ R.text "Fri"
                        , tableCell {padding: Table.dense} $ R.text "Sat"
                        ]
                      ]
                    , tableBody {} (week <$> xs)
                    ]
              ]
            ]
          ]
        , dialogActions {}
          [ button
            { variant: Button.flat
            , onTouchTap: mkEffFn1 \_ -> dispatch ClickedCloseDatepicker
            } [R.text "Close"]
          ]
        ]
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
                } [R.text "2 Weeks", R.br [] [], R.text "From Now"]
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


meals :: R.ReactElement
meals =
  let init =
        { initDatepicked: unsafePerformEff $ do
             LocalValue _ d <- nowDate
             pure d
        }
      {spec: reactSpec, dispatcher} = T.createReactSpec spec (initialState init)
  in  R.createElement (R.createClass reactSpec) unit []

