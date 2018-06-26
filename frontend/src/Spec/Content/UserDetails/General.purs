module Spec.Content.UserDetails.General where

import Links (SiteLinks)
import User (UserDetails)
import Error (SiteError (SiteErrorCustomer), CustomerError (..))
import LocalCooking.Spec.Common.Pending (pending)
import LocalCooking.Spec.Common.Form.Name as Name
import LocalCooking.Spec.Common.Form.Address as Address
import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Thermite.Params (LocalCookingState, initLocalCookingState, LocalCookingAction, LocalCookingParams, whileMountedLocalCooking, performActionLocalCooking)
import LocalCooking.Semantics.Mitch (GetSetCustomer (..))
import LocalCooking.Dependencies.Mitch (GetCustomerSparrowClientQueues, SetCustomerSparrowClientQueues)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Address (USAAddress)
import Data.Lens (Lens', lens)
import Data.UUID (GENUUID)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import IxSignal.Extra (onAvailable, getAvailable, getWhen)
import Queue.Types (READ, WRITE, readOnly, writeOnly)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  , rerender :: Unit
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  , rerender: unit
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)
  | SubmitGeneral
  | ReRender


type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { name ::
          { signal       :: IxSignal (Effects eff) Name.NameState
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , setQueue     :: One.Queue (write :: WRITE) (Effects eff) Name.NameState
          }
        , address ::
          { signal       :: IxSignal (Effects eff) (Maybe USAAddress)
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , setQueue     :: One.Queue (write :: WRITE) (Effects eff) USAAddress
          }
        , submit ::
          { queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , disabledSignal :: IxSignal (Effects eff) Boolean
          }
        , pendingSignal    :: IxSignal (Effects eff) Boolean
        , setCustomerQueues :: SetCustomerSparrowClientQueues (Effects eff)
        , siteErrorQueue :: One.Queue (write :: WRITE) (Effects eff) SiteError
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params
  { name
  , address
  , submit
  , pendingSignal
  , setCustomerQueues
  , siteErrorQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
      ReRender -> void $ T.cotransform _ {rerender = unit}
      SubmitGeneral -> do
        liftEff $ IxSignal.set true pendingSignal
        authToken <- liftBase $ getAvailable params.authTokenSignal
        let whenNameGood mX = case mX of
              Name.NameGood x -> Just x
              _ -> Nothing
        name <- liftBase $ getWhen whenNameGood name.signal
        address <- liftBase $ getAvailable address.signal
        mErr <- liftBase $ OneIO.callAsync setCustomerQueues $
          AccessInitIn {token: authToken, subj: GetSetCustomer {name,address}}
        liftEff $ do
          One.putQueue siteErrorQueue $ case mErr of
            Nothing -> SiteErrorCustomer CustomerSaveFailed
            Just JSONUnit -> SiteErrorCustomer CustomerSaveSuccess
          -- FIXME threaded...?
          IxSignal.set false pendingSignal

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display1
        , align: Typography.center
        } [R.text "General"]
      , Name.name
        { label: R.text "Name"
        , fullWidth: true
        , name: "name"
        , id: "name"
        , nameSignal: name.signal
        , updatedQueue: name.updatedQueue
        , setQueue: name.setQueue
        }
      , Address.address
        { addressSignal: address.signal
        , updatedQueue: address.updatedQueue
        , setQueue: address.setQueue
        }
      , Submit.submit
        { color: Button.secondary
        , variant: Button.raised
        , size: Button.large
        , style: createStyles {marginTop: "1em"}
        , disabledSignal: submit.disabledSignal
        , triggerQueue: submit.queue
        , fullWidth: false
        } [R.text "Submit"]
      , pending
        { pendingSignal
        }
      ]


general :: forall eff
         . LocalCookingParams SiteLinks UserDetails (Effects eff)
        -> { getCustomerQueues :: GetCustomerSparrowClientQueues (Effects eff)
           , setCustomerQueues :: SetCustomerSparrowClientQueues (Effects eff)
           , siteErrorQueue :: One.Queue (write :: WRITE) (Effects eff) SiteError
           }
        -> R.ReactElement
general params {getCustomerQueues,setCustomerQueues,siteErrorQueue} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { name:
              { signal: nameSignal
              , updatedQueue: nameUpdatedQueue
              , setQueue: nameSetQueue
              }
            , address:
              { signal: addressSignal
              , updatedQueue: addressUpdatedQueue
              , setQueue: addressSetQueue
              }
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              }
            , pendingSignal
            , setCustomerQueues
            , siteErrorQueue
            }
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      submitValue this = do
        mName <- IxSignal.get nameSignal
        x <- case mName of
          Name.NameGood _ -> do
            mAddr <- IxSignal.get addressSignal
            case mAddr of
              Nothing -> pure true
              Just _ -> pure false
          _ -> pure true
        IxSignal.set x submitDisabledSignal
        unsafeCoerceEff $ dispatcher this ReRender
      reactSpec' =
          Queue.whileMountedIx
            submitQueue
            "onSubmit"
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitGeneral)
        $ Queue.whileMountedIx
            nameUpdatedQueue
            "nameUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            addressUpdatedQueue
            "addressUpdated"
            (\this _ -> submitValue this)
        $ whileMountedLocalCooking
            params
            "Spec.Content.UserDetails.General"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $   reactSpec
              { componentDidMount = \this -> do             
                  let getCustomerData authToken =
                        unsafeCoerceEff $ OneIO.callAsyncEff getCustomerQueues
                          (\mUser -> case mUser of
                            Nothing -> pure unit
                            Just (GetSetCustomer {name,address}) -> do
                              One.putQueue nameSetQueue (Name.NameGood name)
                              One.putQueue addressSetQueue address
                          )
                          (AccessInitIn {token: authToken, subj: JSONUnit})
                  unsafeCoerceEff $ onAvailable
                    getCustomerData
                    params.authTokenSignal
              }
  in  R.createElement (R.createClass reactSpec') unit []
  where
    nameSignal = unsafePerformEff $ IxSignal.make $ Name.NamePartial ""
    nameUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    nameSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    addressSignal = unsafePerformEff $ IxSignal.make Nothing
    addressUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    addressSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
