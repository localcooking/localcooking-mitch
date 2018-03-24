{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Auth where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Hashable (Hashable)
import Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class as NaCl


newtype AuthToken = AuthToken
  { getAuthToken :: Nonce
  } deriving (Eq, Hashable)

instance Show AuthToken where
  show (AuthToken x) = T.unpack (T.decodeUtf8 (NaCl.encode x))
