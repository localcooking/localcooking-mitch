module Spec.Content.UserDetails.Diet where

import LocalCooking.Common.Diet (Diet (..))
import LocalCooking.Spec.Tag (tag, TagVariant (TagDiet))
import LocalCooking.Spec.Common.Form.Search as Search
import LocalCooking.Spec.Common.Form.SearchResults as SearchResults
import LocalCooking.Spec.Common.Form.Decisions as Decisions

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R

import MaterialUI.Divider (divider)

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
  | eff)


spec :: forall eff
      . { search ::
          { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , signal :: IxSignal (Effects eff) String
          , setQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        , results ::
          { setQueue :: One.Queue (write :: WRITE) (Effects eff) (Array Diet)
          , addQueue :: One.Queue (write :: WRITE) (Effects eff) (Array Diet)
          , delQueue :: One.Queue (write :: WRITE) (Effects eff) Diet
          , signal :: IxSignal (Effects eff) (Array Diet)
          }
        , decisions ::
          { addQueue :: One.Queue (write :: WRITE) (Effects eff) Diet
          , delQueue :: One.Queue (write :: WRITE) (Effects eff) Diet
          , signal :: IxSignal (Effects eff) (Array Diet)
          }
        }
     -> T.Spec eff State Unit Action
spec {search,results,decisions} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ R.text "Diet"
      , divider {}
      , Search.search
        { label: R.text "Diet Name"
        , fullWidth: true
        , id: "diet-search"
        , updatedQueue: search.updatedQueue
        , searchSignal: search.signal
        , setQueue: search.setQueue
        }
      , SearchResults.results
        { setQueue: results.setQueue
        , addQueue: results.addQueue
        , delQueue: results.delQueue
        , resultsSignal: results.signal
        , renderA: \(Diet d) ->
            tag
            { onClick: Just (pure unit)
            , onDelete: Nothing
            , variant: TagDiet
            , label: d
            }
        }
      , Decisions.decisions
        { addQueue: decisions.addQueue
        , delQueue: decisions.delQueue
        , decisionsSignal: decisions.signal
        , renderA: \(Diet d) ->
            tag
            { onClick: Nothing
            , onDelete: Just (pure unit)
            , variant: TagDiet
            , label: d
            }
        }
      ]


diet :: R.ReactElement
diet =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { search:
              { updatedQueue: searchUpdatedQueue
              , signal: searchSignal
              , setQueue: searchSetQueue
              }
            , results:
              { setQueue: resultsSetQueue
              , addQueue: resultsAddQueue
              , delQueue: resultsDelQueue
              , signal: resultsSignal
              }
            , decisions:
              { addQueue: decisionsAddQueue
              , delQueue: decisionsDelQueue
              , signal: decisionsSignal
              }
            }
          )
          initialState
  in  R.createElement (R.createClass reactSpec) unit []
  where
    searchUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    searchSignal = unsafePerformEff $ IxSignal.make ""
    searchSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsAddQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsDelQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsSignal = unsafePerformEff $ IxSignal.make []
    decisionsAddQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    decisionsDelQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    decisionsSignal = unsafePerformEff $ IxSignal.make []
