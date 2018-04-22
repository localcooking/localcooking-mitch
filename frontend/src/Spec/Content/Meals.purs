module Spec.Content.Meals where

import Spec.Tag (tag)

import Prelude
import Data.Monoid ((<>))
import Data.Monoid.Endo (Endo (..))
import Data.Maybe (Maybe (..), fromJust)
import Data.Tuple (Tuple (..))
import Data.Date (Date, month, year, day, canonicalDate, weekday, lastDayOfMonth)
import Data.Date.Component (Month (January, December), Year, Weekday (..), Day)
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
                      let cell {current,day} =
                            tableCell {padding: Table.dense} $ R.text $ show $ fromEnum day
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



type Week a =
  { sun :: a
  , mon :: a
  , tue :: a
  , wed :: a
  , thu :: a
  , fri :: a
  , sat :: a
  }

getCalendar :: Year -> Month -> Array (Week {current :: Boolean, day :: Day})
getCalendar y m =
  [ firstWeek
  ] <> ( let go sunday
               | sunday >= secondToLastWeekSunday = Nothing
               | otherwise = Just (Tuple (buildWeek sunday) (succN 7 sunday))
         in  unfoldr go secondWeekSunday
       )
    <>
  [ lastWeek
  ]
  where
    secondWeekSunday :: Day
    secondWeekSunday = case weekday firstDay of
      Sunday -> succN 7 x
      Monday -> succN 6 x
      Tuesday -> succN 5 x
      Wednesday -> succN 4 x
      Thursday -> succN 3 x
      Friday -> succN 2 x
      Saturday -> succN 1 x
      where
        x = day firstDay
    secondToLastWeekSunday :: Day
    secondToLastWeekSunday = case weekday (canonicalDate y m lastDay) of
      Sunday -> predN 7 lastDay
      Monday -> predN 8 lastDay
      Tuesday -> predN 9 lastDay
      Wednesday -> predN 10 lastDay
      Thursday -> predN 11 lastDay
      Friday -> predN 12 lastDay
      Saturday -> predN 13 lastDay
    firstDay :: Date
    firstDay = canonicalDate y m $ unsafePartial $ fromJust $ toEnum 1
    lastDay :: Day
    lastDay = lastDayOfMonth y m
    lastWeek :: Week {current :: Boolean, day :: Day}
    lastWeek = case weekday (canonicalDate y m lastDay) of
      Sunday ->
        { sun: {current: true, day: lastDay}
        , mon: {current: false, day: firstDayNextMonth}
        , tue: {current: false, day: succN 1 firstDayNextMonth}
        , wed: {current: false, day: succN 2 firstDayNextMonth}
        , thu: {current: false, day: succN 3 firstDayNextMonth}
        , fri: {current: false, day: succN 4 firstDayNextMonth}
        , sat: {current: false, day: succN 5 firstDayNextMonth}
        }
      Monday ->
        { sun: {current: true, day: predN 1 lastDay}
        , mon: {current: true, day: lastDay}
        , tue: {current: false, day: firstDayNextMonth}
        , wed: {current: false, day: succN 1 firstDayNextMonth}
        , thu: {current: false, day: succN 2 firstDayNextMonth}
        , fri: {current: false, day: succN 3 firstDayNextMonth}
        , sat: {current: false, day: succN 4 firstDayNextMonth}
        }
      Tuesday ->
        { sun: {current: true, day: predN 2 lastDay}
        , mon: {current: true, day: predN 1 lastDay}
        , tue: {current: true, day: lastDay}
        , wed: {current: false, day: firstDayNextMonth}
        , thu: {current: false, day: succN 1 firstDayNextMonth}
        , fri: {current: false, day: succN 2 firstDayNextMonth}
        , sat: {current: false, day: succN 3 firstDayNextMonth}
        }
      Wednesday ->
        { sun: {current: true, day: predN 3 lastDay}
        , mon: {current: true, day: predN 2 lastDay}
        , tue: {current: true, day: predN 1 lastDay}
        , wed: {current: true, day: lastDay}
        , thu: {current: false, day: firstDayNextMonth}
        , fri: {current: false, day: succN 1 firstDayNextMonth}
        , sat: {current: false, day: succN 2 firstDayNextMonth}
        }
      Thursday ->
        { sun: {current: true, day: predN 4 lastDay}
        , mon: {current: true, day: predN 3 lastDay}
        , tue: {current: true, day: predN 2 lastDay}
        , wed: {current: true, day: predN 1 lastDay}
        , thu: {current: true, day: lastDay}
        , fri: {current: false, day: firstDayNextMonth}
        , sat: {current: false, day: succ' firstDayNextMonth}
        }
      Friday ->
        { sun: {current: true, day: predN 5 lastDay}
        , mon: {current: true, day: predN 4 lastDay}
        , tue: {current: true, day: predN 3 lastDay}
        , wed: {current: true, day: predN 2 lastDay}
        , thu: {current: true, day: predN 1 lastDay}
        , fri: {current: true, day: lastDay}
        , sat: {current: false, day: firstDayNextMonth}
        }
      Saturday -> buildWeek (predN 6 lastDay)
      where
        firstDayNextMonth :: Day
        firstDayNextMonth = day $ case m of
          December -> canonicalDate (succ' y) January x
          _ -> canonicalDate y (succ' m) x
          where
            x :: Day
            x = unsafePartial (fromJust (toEnum 1))
    firstWeek :: Week {current :: Boolean, day :: Day}
    firstWeek = case weekday firstDay of
      Sunday -> buildWeek firstDay'
      Monday ->
        { sun: {current: false, day: lastDayPreviousMonth}
        , mon: {current: true, day: firstDay'}
        , tue: {current: true, day: succN 1 firstDay'}
        , wed: {current: true, day: succN 2 firstDay'}
        , thu: {current: true, day: succN 3 firstDay'}
        , fri: {current: true, day: succN 4 firstDay'}
        , sat: {current: true, day: succN 5 firstDay'}
        }
      Tuesday ->
        { sun: {current: false, day: pred' lastDayPreviousMonth}
        , mon: {current: false, day: lastDayPreviousMonth}
        , tue: {current: true, day: firstDay'}
        , wed: {current: true, day: succN 1 firstDay'}
        , thu: {current: true, day: succN 2 firstDay'}
        , fri: {current: true, day: succN 3 firstDay'}
        , sat: {current: true, day: succN 4 firstDay'}
        }
      Wednesday ->
        { sun: {current: false, day: predN 2 lastDayPreviousMonth}
        , mon: {current: false, day: predN 1 lastDayPreviousMonth}
        , tue: {current: false, day: lastDayPreviousMonth}
        , wed: {current: true, day: firstDay'}
        , thu: {current: true, day: succN 1 firstDay'}
        , fri: {current: true, day: succN 2 firstDay'}
        , sat: {current: true, day: succN 3 firstDay'}
        }
      Thursday ->
        { sun: {current: false, day: predN 3 lastDayPreviousMonth}
        , mon: {current: false, day: predN 2 lastDayPreviousMonth}
        , tue: {current: false, day: predN 1 lastDayPreviousMonth}
        , wed: {current: false, day: lastDayPreviousMonth}
        , thu: {current: true, day: firstDay'}
        , fri: {current: true, day: succN 1 firstDay'}
        , sat: {current: true, day: succN 2 firstDay'}
        }
      Friday ->
        { sun: {current: false, day: predN 4 lastDayPreviousMonth}
        , mon: {current: false, day: predN 3 lastDayPreviousMonth}
        , tue: {current: false, day: predN 2 lastDayPreviousMonth}
        , wed: {current: false, day: predN 1 lastDayPreviousMonth}
        , thu: {current: false, day: lastDayPreviousMonth}
        , fri: {current: true, day: firstDay'}
        , sat: {current: true, day: succ' firstDay'}
        }
      Saturday ->
        { sun: {current: false, day: predN 5 lastDayPreviousMonth}
        , mon: {current: false, day: predN 4 lastDayPreviousMonth}
        , tue: {current: false, day: predN 3 lastDayPreviousMonth}
        , wed: {current: false, day: predN 2 lastDayPreviousMonth}
        , thu: {current: false, day: predN 1 lastDayPreviousMonth}
        , fri: {current: false, day: lastDayPreviousMonth}
        , sat: {current: true, day: firstDay'}
        }
      where
        lastDayPreviousMonth :: Day
        lastDayPreviousMonth = case m of
          January -> lastDayOfMonth (pred' y) December
          _ -> lastDayOfMonth y (pred' m)
        firstDay' = day firstDay
    buildWeek :: Day -> Week {current :: Boolean, day :: Day}
    buildWeek n =
      { sun: {current: true, day: n}
      , mon: {current: true, day: succN 1 n}
      , tue: {current: true, day: succN 2 n}
      , wed: {current: true, day: succN 3 n}
      , thu: {current: true, day: succN 4 n}
      , fri: {current: true, day: succN 5 n}
      , sat: {current: true, day: succN 6 n}
      }
    -- succ7 :: forall a. Enum a => a -> a
    -- succ7 = fold $ replicate 7 succ'
    succ' :: forall a. Enum a => a -> a
    succ' x = unsafePartial (fromJust (succ x))
    pred' :: forall a. Enum a => a -> a
    pred' x = unsafePartial (fromJust (pred x))
    succN :: forall a. Enum a => Int -> a -> a
    succN n = runEndo (Array.fold (Array.replicate n (Endo succ')))
    predN :: forall a. Enum a => Int -> a -> a
    predN n = runEndo (Array.fold (Array.replicate n (Endo pred')))
    runEndo :: forall a. Endo a -> a -> a
    runEndo (Endo x) = x
