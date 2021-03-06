module Spec.Content.UserDetails.Allergies where

import LocalCooking.Common.Tag.Ingredient (IngredientTag (..))
import LocalCooking.Spec.Tag (tag, AnyTag (TagIngredient))
import LocalCooking.Spec.Common.Form.Search as Search
import LocalCooking.Spec.Common.Form.SearchResults as SearchResults
import LocalCooking.Spec.Common.Form.Decisions as Decisions
import LocalCooking.Spec.Common.Form.Submit as Submit

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


spec :: forall eff
      . { search ::
          { updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , signal :: IxSignal (Effects eff) String
          , setQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        , submit ::
          { triggerQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , disabled :: IxSignal (Effects eff) Boolean
          }
        , results ::
          { setQueue :: One.Queue (write :: WRITE) (Effects eff) (Array IngredientTag)
          , addQueue :: One.Queue (write :: WRITE) (Effects eff) (Array IngredientTag)
          , clearQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
          , pendingQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
          , delQueue :: One.Queue (write :: WRITE) (Effects eff) IngredientTag
          , signal :: IxSignal (Effects eff) (SearchResults.SearchResults IngredientTag)
          }
        , decisions ::
          { addQueue :: One.Queue (write :: WRITE) (Effects eff) IngredientTag
          , delQueue :: One.Queue (write :: WRITE) (Effects eff) IngredientTag
          , signal :: IxSignal (Effects eff) (Array IngredientTag)
          }
        }
     -> T.Spec eff State Unit Action
spec {search,submit,results,decisions} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.headline
        } [R.text "Allergies"]
      , divider {}
      , grid {spacing: Grid.spacing8, container: true}
        [ grid {xs: 10, item: true}
          [ Search.search
            { label: R.text "Allergy Name"
            , fullWidth: true
            , id: "allergy-search"
            , updatedQueue: search.updatedQueue
            , searchSignal: search.signal
            , setQueue: search.setQueue
            }
          ]
        , grid {xs: 2, item: true}
          [ Submit.submit
            { color: Button.primary
            , variant: Button.raised
            , size: Button.medium
            , style: createStyles {width: "100%"}
            , triggerQueue: submit.triggerQueue
            , disabledSignal: submit.disabled
            , fullWidth: true
            } [R.text "Add"]
          ]
        ]
      , SearchResults.results
        { setQueue: results.setQueue
        , addQueue: results.addQueue
        , clearQueue: results.clearQueue
        , pendingQueue: results.pendingQueue
        , delQueue: results.delQueue
        , resultsSignal: results.signal
        , renderA: \i ->
            tag
            { onClick: Just (pure unit)
            , onDelete: Nothing
            , tag: TagIngredient i
            }
        }
      , Decisions.decisions
        { addQueue: decisions.addQueue
        , delQueue: decisions.delQueue
        , decisionsSignal: decisions.signal
        , renderA: \i ->
            tag
            { onClick: Nothing
            , onDelete: Just (pure unit)
            , tag: TagIngredient i
            }
        }
      ]


allergies :: R.ReactElement
allergies =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { search:
              { updatedQueue: searchUpdatedQueue
              , signal: searchSignal
              , setQueue: searchSetQueue
              }
            , submit:
              { triggerQueue: submitTriggerQueue
              , disabled: submitDisabled
              }
            , results:
              { setQueue: resultsSetQueue
              , addQueue: resultsAddQueue
              , clearQueue: resultsClearQueue
              , pendingQueue: resultsPendingQueue
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
    resultsClearQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsPendingQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsDelQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    resultsSignal = unsafePerformEff $ IxSignal.make SearchResults.None
    decisionsAddQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    decisionsDelQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    decisionsSignal = unsafePerformEff $ IxSignal.make []
    submitTriggerQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabled = unsafePerformEff $ IxSignal.make true
