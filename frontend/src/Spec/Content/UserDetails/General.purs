module Spec.Content.UserDetails.General where

import Links (SiteLinks)
import User (UserDetails)
import Spec.Icons.NewPerson (newPerson)
import LocalCooking.Spec.Common.Pending (pending)
import LocalCooking.Spec.Common.Form.Name as Name
import LocalCooking.Spec.Common.Form.Address as Address
import LocalCooking.Spec.Common.Form.Submit as Submit
import LocalCooking.Thermite.Params (LocalCookingState, initLocalCookingState, LocalCookingAction, LocalCookingParams, whileMountedLocalCooking, performActionLocalCooking)

import Prelude
import Data.Address (USAAddress (..), USAState (CO))
import Data.Lens (Lens', Prism', lens, prism')
import Data.UUID (GENUUID)
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
import Queue.Types (READ, WRITE, readOnly, writeOnly)
import Queue.One as One
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
      . { name ::
          { signal       :: IxSignal (Effects eff) Name.NameState
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , setQueue     :: One.Queue (write :: WRITE) (Effects eff) Name.NameState
          }
        , address ::
          { signal       :: IxSignal (Effects eff) USAAddress
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , setQueue     :: One.Queue (write :: WRITE) (Effects eff) USAAddress
          }
        , submit ::
          { queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , disabledSignal :: IxSignal (Effects eff) Boolean
          }
        , pendingSignal    :: IxSignal (Effects eff) Boolean
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { name
  , address
  , submit
  , pendingSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
      ReRender -> void $ T.cotransform _ {rerender = unit}
      SubmitGeneral -> do
        liftEff $ IxSignal.set true pendingSignal
        -- authToken <- liftBase $ getAvailable params.authTokenSignal
        -- let whenEmailGood mX = case mX of
        --       Email.EmailGood x -> Just x
        --       _ -> Nothing
        -- email <- liftBase $ getWhen whenEmailGood email.signal
        -- mAuthPass <- liftBase $ OneIO.callAsync authenticateDialogQueue unit
        -- case mAuthPass of
        --   Nothing -> pure unit
        --   Just oldPassword -> do
        --     passwordString <- liftEff (IxSignal.get password.signal)
        --     newPassword <- liftBase $ hashPassword
        --       { password: passwordString
        --       , salt: env.salt
        --       }
            -- FIXME assigning new password is a restricted credential set
            -- case state.user of
            --   Nothing -> pure unit
            --   Just (User {id,socialLogin}) ->
            --     liftEff $ userDeltaIn
            --             $ UserDeltaInSetUser
            --             $ SetUser {id,email,socialLogin,oldPassword,newPassword}
        liftEff $ do
          -- One.putQueue globalErrorQueue $ case mErr of
          --   Nothing -> GlobalErrorSecurity SecuritySaveFailed
          --   Just JSONUnit -> GlobalErrorSecurity SecuritySaveSuccess
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
        , style: createStyles {marginTop: "1em", float: "right"}
        , disabledSignal: submit.disabledSignal
        , triggerQueue: submit.queue
        } [R.text "Submit"]
      , pending
        { pendingSignal
        }
      ]


general :: forall eff
         . LocalCookingParams SiteLinks UserDetails (Effects eff)
        -> R.ReactElement
general params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
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
            }
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      submitValue this = do
        mName <- IxSignal.get nameSignal
        x <- case mName of
          Name.NameGood _ -> pure false
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
        $ whileMountedLocalCooking
            params
            "Spec.Content.UserDetails.General"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    nameSignal = unsafePerformEff $ IxSignal.make $ Name.NamePartial ""
    nameUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    nameSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    addressSignal = unsafePerformEff $ IxSignal.make $ USAAddress {street: "", city: "", state: CO, zip: 80401}
    addressUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    addressSetQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
