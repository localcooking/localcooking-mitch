module Spec.Tag where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)



type State = Unit

initialState :: State
initialState = unit

data Action
  = Clicked
  | Deleted


type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff
      . { label :: String
        , onClick :: Maybe (Eff (Effects eff) Unit)
        , onDelete :: Maybe (Eff (Effects eff) Unit)
        }
     -> T.Spec eff State Unit Action
spec {label,onClick,onDelete} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ chip
        { label: R.text "Three"
        , onClick: mkEffFn1 \_ -> pure unit
        }
      ]
