module Spec.Dialogs.Datepicker where

import Prelude
import Data.Maybe (fromJust)
import Data.Date (Date, month, year, day, canonicalDate)
import Data.Date.Component (Month (January, December))
import Data.Date.Extra (getCalendar, plusTwoWeeks)
import Data.DateTime.Locale (LocalValue (..))
import Data.Enum (pred, succ, fromEnum)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDate)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Partial.Unsafe (unsafePartial)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Icons.ChevronLeft (chevronLeftIcon)
import MaterialUI.Icons.ChevronRight (chevronRightIcon)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.IconButton (iconButton)
import MaterialUI.Grid (grid)
import MaterialUI.Table (table, tableRow, tableHead, tableCell, tableBody)
import MaterialUI.Table as Table
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogActions (dialogActions)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO




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
      . { pickDateOutput :: One.Queue (write :: WRITE) (Effects eff) Date
        }
     -> T.Spec (Effects eff) State Unit Action
spec {pickDateOutput} = T.simpleSpec performAction render
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
      ClickedDate d -> do
        void $ T.cotransform _ { datepicked = d, datepickerDialog = false }
        liftEff $ One.putQueue pickDateOutput d

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ dialog
        { open: state.datepickerDialog
        , onClose: mkEffFn1 \_ -> dispatch ClickedCloseDatepicker
        }
        [ dialogTitle {} []
        , dialogContent {}
          [ grid {container: true}
            [ grid {item: true, xs: 2}
              [ iconButton
                { onTouchTap: mkEffFn1 \_ -> dispatch ClickedPrevMonth
                , disabled: month state.datepicked == month today
                } chevronLeftIcon
              ]
            , grid {item: true, xs: 8}
              [ typography
                { variant: Typography.headline
                , align: Typography.center
                } [ R.text $ show (month state.datepicked)
                          <> " "
                          <> show (fromEnum $ year state.datepicked)
                  ]
              ]
            , grid {item: true, xs: 2}
              [iconButton {onTouchTap: mkEffFn1 \_ -> dispatch ClickedNextMonth} chevronRightIcon]
            , grid {item: true, xs: 12}
              [ table {} $
                let xs = getCalendar (year state.datepicked) (month state.datepicked)
                    week {sun,mon,tue,wed,thu,fri,sat} = tableRow {} $
                      let cell {current,day,month,year} =
                            let date = canonicalDate year month day
                            in  Table.withStylesCell
                              (\theme ->
                                { root: createStyles
                                  { textAlign: "center"
                                  , background: case unit of
                                    _ | date < today -> "#aaa"
                                      | date == today -> theme.palette.secondary.light
                                      | current -> ""
                                      | otherwise -> "#eee"
                                  , border: case unit of
                                    _ | state.datepicked == date -> "2px solid "
                                        <> theme.palette.primary.light
                                      | otherwise -> ""
                                  }
                                }
                              )
                              \{classes} ->
                                let f | date < today =
                                        tableCell
                                          { padding: Table.dense
                                          , classes: Table.createClassesCell classes
                                          }
                                      | otherwise =
                                        tableCell
                                          { padding: Table.dense
                                          , classes: Table.createClassesCell classes
                                          , onClick: mkEffFn1 \_ -> dispatch $ ClickedDate date
                                          }
                                in  f $ R.text $ show $ fromEnum day
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
                        [ tableCell {padding: Table.dense} (R.text "Sun")
                        , tableCell {padding: Table.dense} (R.text "Mon")
                        , tableCell {padding: Table.dense} (R.text "Tue")
                        , tableCell {padding: Table.dense} (R.text "Wed")
                        , tableCell {padding: Table.dense} (R.text "Thu")
                        , tableCell {padding: Table.dense} (R.text "Fri")
                        , tableCell {padding: Table.dense} (R.text "Sat")
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
      ]

    today = unsafePerformEff $ do
      LocalValue _ d <- nowDate
      pure d



datepicker :: forall eff
            . { pickDate :: OneIO.IOQueues (Effects eff) Unit Date
              } -> R.ReactElement
datepicker {pickDate: OneIO.IOQueues {output: pickDateOutput, input: pickDateInput}} =
  let init =
        { initDatepicked: unsafePerformEff initDatepicked
        }
      {spec: reactSpec, dispatcher} = T.createReactSpec (spec {pickDateOutput}) (initialState init)
      reactSpec' =
          Queue.whileMountedOne pickDateInput
            (\this _ -> unsafeCoerceEff $ dispatcher this ClickedOpenDatepicker)
        $ reactSpec
  in  R.createElement (R.createClass reactSpec) unit []


initDatepicked :: forall eff. Eff (now :: NOW | eff) Date
initDatepicked = do
  LocalValue _ d <- nowDate
  pure (plusTwoWeeks d)
