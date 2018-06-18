module Spec.Content where

import Spec.Content.Root (root)
import Spec.Content.Chefs (chefs)
import Spec.Content.Meals (meals)
import Links (SiteLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, whileMountedLocalCooking, performActionLocalCooking, initLocalCookingState)

import Prelude
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import DOM.HTML.Window.Extra (WindowSize)
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Signal.WhileMounted as Signal

import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



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
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , console   :: CONSOLE
  , scrypt    :: SCRYPT
  | eff)


getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec
  params
  = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ case state.localCooking.currentPage of
          RootLink -> root {windowSizeSignal: params.windowSizeSignal,toURI: params.toURI,siteLinks: params.siteLinks}
          ChefsLink -> chefs
          MealsLink -> meals
          _ -> R.text ""
      ]



content :: forall eff
         . LocalCookingParams SiteLinks UserDetails (Effects eff)
        -> R.ReactElement
content
  params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params
          )
          (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
