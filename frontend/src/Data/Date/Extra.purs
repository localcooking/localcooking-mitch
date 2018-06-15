module Data.Date.Extra where

import Prelude
import Data.Monoid.Endo (Endo (..))
import Data.Maybe (Maybe (..), fromJust)
import Data.Tuple (Tuple (..))
import Data.Date (Date, year, month, day, weekday, canonicalDate, lastDayOfMonth, diff)
import Data.Time.Duration (Days (..))
import Data.Date.Component (Year, Month (January, December), Weekday (..), Day)
import Data.Enum (class Enum, pred, succ, toEnum, fromEnum)
import Data.Array as Array
import Data.Unfoldable (unfoldr)
import Data.Int as Int
import Partial.Unsafe (unsafePartial)



type Week a =
  { sun :: a
  , mon :: a
  , tue :: a
  , wed :: a
  , thu :: a
  , fri :: a
  , sat :: a
  }

getCalendar :: Year
            -> Month
            -> Array
               ( Week
                 { current :: Boolean
                 , day :: Day
                 , month :: Month
                 , year :: Year
                 }
               )
getCalendar y m =
  [ firstWeek
  ] <> ( let go sunday
               | sunday > secondToLastWeekSunday = Nothing
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

    lastWeek :: Week {current :: Boolean, day :: Day, month :: Month, year :: Year}
    lastWeek = case weekday (canonicalDate y m lastDay) of
      Sunday ->
        { sun: {current: true, year: y, month: m, day: lastDay}
        , mon: {current: false, year: nextMonthYear, month: nextMonth, day: firstDayNextMonth}
        , tue: {current: false, year: nextMonthYear, month: nextMonth, day: succN 1 firstDayNextMonth}
        , wed: {current: false, year: nextMonthYear, month: nextMonth, day: succN 2 firstDayNextMonth}
        , thu: {current: false, year: nextMonthYear, month: nextMonth, day: succN 3 firstDayNextMonth}
        , fri: {current: false, year: nextMonthYear, month: nextMonth, day: succN 4 firstDayNextMonth}
        , sat: {current: false, year: nextMonthYear, month: nextMonth, day: succN 5 firstDayNextMonth}
        }
      Monday ->
        { sun: {current: true, year: y, month: m, day: predN 1 lastDay}
        , mon: {current: true, year: y, month: m, day: lastDay}
        , tue: {current: false, year: nextMonthYear, month: nextMonth, day: firstDayNextMonth}
        , wed: {current: false, year: nextMonthYear, month: nextMonth, day: succN 1 firstDayNextMonth}
        , thu: {current: false, year: nextMonthYear, month: nextMonth, day: succN 2 firstDayNextMonth}
        , fri: {current: false, year: nextMonthYear, month: nextMonth, day: succN 3 firstDayNextMonth}
        , sat: {current: false, year: nextMonthYear, month: nextMonth, day: succN 4 firstDayNextMonth}
        }
      Tuesday ->
        { sun: {current: true, year: y, month: m, day: predN 2 lastDay}
        , mon: {current: true, year: y, month: m, day: predN 1 lastDay}
        , tue: {current: true, year: y, month: m, day: lastDay}
        , wed: {current: false, year: nextMonthYear, month: nextMonth, day: firstDayNextMonth}
        , thu: {current: false, year: nextMonthYear, month: nextMonth, day: succN 1 firstDayNextMonth}
        , fri: {current: false, year: nextMonthYear, month: nextMonth, day: succN 2 firstDayNextMonth}
        , sat: {current: false, year: nextMonthYear, month: nextMonth, day: succN 3 firstDayNextMonth}
        }
      Wednesday ->
        { sun: {current: true, year: y, month: m, day: predN 3 lastDay}
        , mon: {current: true, year: y, month: m, day: predN 2 lastDay}
        , tue: {current: true, year: y, month: m, day: predN 1 lastDay}
        , wed: {current: true, year: y, month: m, day: lastDay}
        , thu: {current: false, year: nextMonthYear, month: nextMonth, day: firstDayNextMonth}
        , fri: {current: false, year: nextMonthYear, month: nextMonth, day: succN 1 firstDayNextMonth}
        , sat: {current: false, year: nextMonthYear, month: nextMonth, day: succN 2 firstDayNextMonth}
        }
      Thursday ->
        { sun: {current: true, year: y, month: m, day: predN 4 lastDay}
        , mon: {current: true, year: y, month: m, day: predN 3 lastDay}
        , tue: {current: true, year: y, month: m, day: predN 2 lastDay}
        , wed: {current: true, year: y, month: m, day: predN 1 lastDay}
        , thu: {current: true, year: y, month: m, day: lastDay}
        , fri: {current: false, year: nextMonthYear, month: nextMonth, day: firstDayNextMonth}
        , sat: {current: false, year: nextMonthYear, month: nextMonth, day: succ' firstDayNextMonth}
        }
      Friday ->
        { sun: {current: true, year: y, month: m, day: predN 5 lastDay}
        , mon: {current: true, year: y, month: m, day: predN 4 lastDay}
        , tue: {current: true, year: y, month: m, day: predN 3 lastDay}
        , wed: {current: true, year: y, month: m, day: predN 2 lastDay}
        , thu: {current: true, year: y, month: m, day: predN 1 lastDay}
        , fri: {current: true, year: y, month: m, day: lastDay}
        , sat: {current: false, year: nextMonthYear, month: nextMonth, day: firstDayNextMonth}
        }
      Saturday -> buildWeek (predN 6 lastDay)

    firstWeek :: Week {current :: Boolean, day :: Day, month :: Month, year :: Year}
    firstWeek = case weekday firstDay of
      Sunday -> buildWeek firstDay'
      Monday ->
        { sun: {current: false, year: previousMonthYear, month: previousMonth, day: lastDayPreviousMonth}
        , mon: {current: true, year: y, month: m, day: firstDay'}
        , tue: {current: true, year: y, month: m, day: succN 1 firstDay'}
        , wed: {current: true, year: y, month: m, day: succN 2 firstDay'}
        , thu: {current: true, year: y, month: m, day: succN 3 firstDay'}
        , fri: {current: true, year: y, month: m, day: succN 4 firstDay'}
        , sat: {current: true, year: y, month: m, day: succN 5 firstDay'}
        }
      Tuesday ->
        { sun: {current: false, year: previousMonthYear, month: previousMonth, day: pred' lastDayPreviousMonth}
        , mon: {current: false, year: previousMonthYear, month: previousMonth, day: lastDayPreviousMonth}
        , tue: {current: true, year: y, month: m, day: firstDay'}
        , wed: {current: true, year: y, month: m, day: succN 1 firstDay'}
        , thu: {current: true, year: y, month: m, day: succN 2 firstDay'}
        , fri: {current: true, year: y, month: m, day: succN 3 firstDay'}
        , sat: {current: true, year: y, month: m, day: succN 4 firstDay'}
        }
      Wednesday ->
        { sun: {current: false, year: previousMonthYear, month: previousMonth, day: predN 2 lastDayPreviousMonth}
        , mon: {current: false, year: previousMonthYear, month: previousMonth, day: predN 1 lastDayPreviousMonth}
        , tue: {current: false, year: previousMonthYear, month: previousMonth, day: lastDayPreviousMonth}
        , wed: {current: true, year: y, month: m, day: firstDay'}
        , thu: {current: true, year: y, month: m, day: succN 1 firstDay'}
        , fri: {current: true, year: y, month: m, day: succN 2 firstDay'}
        , sat: {current: true, year: y, month: m, day: succN 3 firstDay'}
        }
      Thursday ->
        { sun: {current: false, year: previousMonthYear, month: previousMonth, day: predN 3 lastDayPreviousMonth}
        , mon: {current: false, year: previousMonthYear, month: previousMonth, day: predN 2 lastDayPreviousMonth}
        , tue: {current: false, year: previousMonthYear, month: previousMonth, day: predN 1 lastDayPreviousMonth}
        , wed: {current: false, year: previousMonthYear, month: previousMonth, day: lastDayPreviousMonth}
        , thu: {current: true, year: y, month: m, day: firstDay'}
        , fri: {current: true, year: y, month: m, day: succN 1 firstDay'}
        , sat: {current: true, year: y, month: m, day: succN 2 firstDay'}
        }
      Friday ->
        { sun: {current: false, year: previousMonthYear, month: previousMonth, day: predN 4 lastDayPreviousMonth}
        , mon: {current: false, year: previousMonthYear, month: previousMonth, day: predN 3 lastDayPreviousMonth}
        , tue: {current: false, year: previousMonthYear, month: previousMonth, day: predN 2 lastDayPreviousMonth}
        , wed: {current: false, year: previousMonthYear, month: previousMonth, day: predN 1 lastDayPreviousMonth}
        , thu: {current: false, year: previousMonthYear, month: previousMonth, day: lastDayPreviousMonth}
        , fri: {current: true, year: y, month: m, day: firstDay'}
        , sat: {current: true, year: y, month: m, day: succ' firstDay'}
        }
      Saturday ->
        { sun: {current: false, year: previousMonthYear, month: previousMonth, day: predN 5 lastDayPreviousMonth}
        , mon: {current: false, year: previousMonthYear, month: previousMonth, day: predN 4 lastDayPreviousMonth}
        , tue: {current: false, year: previousMonthYear, month: previousMonth, day: predN 3 lastDayPreviousMonth}
        , wed: {current: false, year: previousMonthYear, month: previousMonth, day: predN 2 lastDayPreviousMonth}
        , thu: {current: false, year: previousMonthYear, month: previousMonth, day: predN 1 lastDayPreviousMonth}
        , fri: {current: false, year: previousMonthYear, month: previousMonth, day: lastDayPreviousMonth}
        , sat: {current: true, year: y, month: m, day: firstDay'}
        }
      where
        firstDay' = day firstDay

    buildWeek :: Day -> Week {current :: Boolean, day :: Day, month :: Month, year :: Year}
    buildWeek n =
      { sun: {current: true, month: m, year: y, day: n}
      , mon: {current: true, month: m, year: y, day: succN 1 n}
      , tue: {current: true, month: m, year: y, day: succN 2 n}
      , wed: {current: true, month: m, year: y, day: succN 3 n}
      , thu: {current: true, month: m, year: y, day: succN 4 n}
      , fri: {current: true, month: m, year: y, day: succN 5 n}
      , sat: {current: true, month: m, year: y, day: succN 6 n}
      }

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

    previousMonth :: Month
    previousMonth = case m of
      January -> December
      _ -> pred' m

    previousMonthYear :: Year
    previousMonthYear = case m of
      January -> pred' y
      _ -> y

    lastDayPreviousMonth :: Day
    lastDayPreviousMonth = lastDayOfMonth previousMonthYear previousMonth

    nextMonth :: Month
    nextMonth = case m of
      December -> January
      _ -> succ' m

    nextMonthYear :: Year
    nextMonthYear = case m of
      December -> succ' y
      _ -> y

    firstDayNextMonth :: Day
    firstDayNextMonth = day $ canonicalDate nextMonthYear nextMonth x
      where
        x :: Day
        x = unsafePartial (fromJust (toEnum 1))





data MaybeHalf
  = None
  | AHalf

instance showMaybeHalf :: Show MaybeHalf where
  show x = case x of
    None -> ""
    AHalf -> "½"


data Duration
  = DaysUnit Int
  | WeeksUnit (Tuple Int MaybeHalf)
  | MonthsUnit (Tuple Int MaybeHalf)

instance showDuration :: Show Duration where
  show x = case x of
    DaysUnit y ->
      show y <>
        if y == 1 then " Day" else " Days"
    WeeksUnit (Tuple y h) ->
      show y <> show h <>
        if y == 1 || y == 0 then " Week" else " Weeks"
    MonthsUnit (Tuple y h) ->
      show y <> show h <>
        if y == 1 || y == 0 then " Month" else " Months"


humanReadableDuration :: Int -> Duration
humanReadableDuration days
  | days < 4 = DaysUnit days
  | days < 6 = WeeksUnit (Tuple 0 AHalf)
  | days < 9 = WeeksUnit (Tuple 1 None)
  | days < 12 = WeeksUnit (Tuple 1 AHalf)
  | days < 15 = WeeksUnit (Tuple 2 None)
  | days < 19 = WeeksUnit (Tuple 2 AHalf)
  | days < 22 = WeeksUnit (Tuple 3 None)
  | days < 27 = WeeksUnit (Tuple 3 AHalf)
  | days < 32 = MonthsUnit (Tuple 1 None)
  | days < 55 = MonthsUnit (Tuple 1 AHalf)
  | days < 65 = MonthsUnit (Tuple 2 None)
  | days < 80 = MonthsUnit (Tuple 2 AHalf)
  | otherwise = MonthsUnit (Tuple 3 None)



plusTwoWeeks :: Date -> Date
plusTwoWeeks d = case unit of
  _ | Int.floor delta >= 14 ->
        canonicalDate (year d) (month d)
          (unsafePartial (fromJust (toEnum (fromEnum (day d) + 14))))
    | otherwise -> case month d of
      December ->
        canonicalDate (unsafePartial (fromJust (succ (year d)))) January
          (unsafePartial (fromJust (toEnum (14 - Int.floor delta))))
      _ ->
        canonicalDate (year d) (unsafePartial (fromJust (succ (month d))))
          (unsafePartial (fromJust (toEnum (14 - Int.floor delta))))
  where
    lastDay = lastDayOfMonth (year d) (month d)
    Days delta = diff (canonicalDate (year d) (month d) lastDay) d
