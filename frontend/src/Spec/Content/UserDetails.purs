module Spec.Content.UserDetails where

import Links (SiteLinks (UserDetailsLink), UserDetailsLinks (..))
import User (UserDetails)
import Error (SiteError)
import Spec.Content.UserDetails.General (general)
import Spec.Content.UserDetails.Orders (orders)
import Spec.Content.UserDetails.Diet (diets)
import Spec.Content.UserDetails.Allergies (allergies)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, initLocalCookingState, performActionLocalCooking, LocalCookingAction, whileMountedLocalCooking)
import LocalCooking.Dependencies.Mitch (GetCustomerSparrowClientQueues, SetCustomerSparrowClientQueues)
import LocalCooking.Dependencies.Tag (TagSearch)
import LocalCooking.Common.Tag.Diet (DietTag)

import Prelude
import Data.Lens (Lens', lens)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R

import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Queue.Types (WRITE)
import Queue.One as One


type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)

type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { getCustomerQueues :: GetCustomerSparrowClientQueues (Effects eff)
        , setCustomerQueues :: SetCustomerSparrowClientQueues (Effects eff)
        , siteErrorQueue :: One.Queue (write :: WRITE) (Effects eff) SiteError
        , tagSearch :: TagSearch (Effects eff)
        , tagSearchResultsQueues ::
          { dietTags :: One.Queue (write :: WRITE) (Effects eff) (Array DietTag)
          }
        }
     -> T.Spec (Effects eff) State Unit Action
spec params
  { getCustomerQueues
  , setCustomerQueues
  , siteErrorQueue
  , tagSearch
  , tagSearchResultsQueues
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
        [ case state.localCooking.currentPage of
            -- TODO pack currentPageSignal listener to this level, so
            -- side buttons aren't redrawn
            UserDetailsLink mUserDetails -> case mUserDetails of
              Nothing -> general params
                { getCustomerQueues
                , setCustomerQueues
                , siteErrorQueue
                }
              Just x -> case x of
                UserDetailsGeneralLink -> general params
                  { getCustomerQueues
                  , setCustomerQueues
                  , siteErrorQueue
                  }
                UserDetailsOrdersLink -> orders
                UserDetailsDietLink -> diets
                  { tagSearch
                  , dietTagSearchResultsQueue: tagSearchResultsQueues.dietTags
                  }
                UserDetailsAllergiesLink -> allergies
                _ -> R.text ""
            _ -> R.text ""
        ]


userDetails :: forall eff
             . LocalCookingParams SiteLinks UserDetails (Effects eff)
            -> { getCustomerQueues :: GetCustomerSparrowClientQueues (Effects eff)
               , setCustomerQueues :: SetCustomerSparrowClientQueues (Effects eff)
               , siteErrorQueue :: One.Queue (write :: WRITE) (Effects eff) SiteError
               , tagSearch :: TagSearch (Effects eff)
               , tagSearchResultsQueues ::
                 { dietTags :: One.Queue (write :: WRITE) (Effects eff) (Array DietTag)
                 }
               }
            -> R.ReactElement
userDetails params args =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params args
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "Spec.Content.UserDetails"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
