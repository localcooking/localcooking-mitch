module Spec.Content.UserDetails.Diet where

import LocalCooking.Common.Tag.Diet (DietTag (..))
import LocalCooking.Spec.Tag (tag, AnyTag (TagDiet))
-- import LocalCooking.Spec.Common.Form.Search as Search
-- import LocalCooking.Spec.Common.Form.SearchResults as SearchResults
-- import LocalCooking.Spec.Common.Form.Decisions as Decisions
-- import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Spec.Common.Form.TagSearch as TagSearch
import LocalCooking.Dependencies.Tag (TagSearch)

import Prelude
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R

import MaterialUI.Types (createStyles)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Button as Button

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import IxQueue (IxQueue)
import IxQueue as IxQueue
import Queue.Types (writeOnly, readOnly, READ, WRITE)
import Queue.One as One



type State = Unit

initialState :: State
initialState = unit

type Action = Unit


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


diets :: forall eff
       . { tagSearch :: TagSearch (Effects eff)
         , dietTagSearchResultsQueue :: One.Queue (write :: WRITE) (Effects eff) (Array DietTag)
         }
      -> R.ReactElement
diets {tagSearch,dietTagSearchResultsQueue} =
  TagSearch.genericTagSearch
    { tagSearch: tagSearch.searchDietTags
    , resultsQueue: dietTagSearchResultsQueue
    , label: "Diet Name"
    , headline: "Diets"
    , id: "diet-search"
    , constructTag: TagDiet
    }
