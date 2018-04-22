module Spec.Tag where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)

import MaterialUI.Types (createStyles)
import MaterialUI.Chip (chip)
import MaterialUI.Chip as Chip




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
     -> T.Spec (Effects eff) State Unit Action
spec {label,onClick,onDelete} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ Chip.withStyles
        (\theme ->
          { root: createStyles
            { margin: theme.spacing.unit `div` 2
            }
          }
        )
        \{classes} ->
        case Tuple onClick onDelete of
          Tuple (Just onClick') (Just onDelete') -> chip
            { label: R.text label
            , onClick: mkEffFn1 \_ -> unsafeCoerceEff onClick'
            , onDelete: mkEffFn1 \_ -> unsafeCoerceEff onDelete'
            , classes: Chip.createClasses classes
            }
          Tuple Nothing (Just onDelete') -> chip
            { label: R.text label
            , onDelete: mkEffFn1 \_ -> unsafeCoerceEff onDelete'
            , classes: Chip.createClasses classes
            }
          Tuple (Just onClick') Nothing -> chip
            { label: R.text label
            , onClick: mkEffFn1 \_ -> unsafeCoerceEff onClick'
            , classes: Chip.createClasses classes
            }
          Tuple Nothing Nothing -> chip
            { label: R.text label
            , classes: Chip.createClasses classes
            }
      ]


tag :: forall eff
     . { label :: String
       , onClick :: Maybe (Eff (Effects eff) Unit)
       , onDelete :: Maybe (Eff (Effects eff) Unit)
       } -> R.ReactElement
tag params =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
