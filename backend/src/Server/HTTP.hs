{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , RecordWildCards
  , DataKinds
  , QuasiQuotes
  #-}

module Server.HTTP where

import Links (SiteLinks (..), UserDetailsLinks (..))

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))

import Web.Routes.Nested (RouterT, match, matchHere, matchGroup, matchAny, textOnly, l_, (</>), o_)
import Network.Wai.Trans (MiddlewareT)
import Network.HTTP.Types (status302)
import qualified Data.Text.Encoding        as T
import Data.URI (printURI)
import Data.Url (packLocation)
import qualified Data.Strict.Maybe as Strict
import Path.Extended (ToLocation (toLocation))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)




httpServer :: (SiteLinks -> MiddlewareT AppM) -> RouterT (MiddlewareT AppM) sec AppM ()
httpServer handleAuthToken = do
  Env{envHostname,envTls} <- lift ask

  -- main routes
  matchGroup (l_ "userDetails" </> o_) $ do
    matchHere $ handleAuthToken $ UserDetailsLink Nothing
    match (l_ "general" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsGeneral
    match (l_ "security" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsSecurity
    match (l_ "orders" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsOrders
    match (l_ "diet" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsDiet
    match (l_ "allergies" </> o_) $ handleAuthToken $ UserDetailsLink $ Just UserDetailsAllergies
  matchGroup (l_ "meals" </> o_) $
    matchHere $ handleAuthToken MealsLink
  matchGroup (l_ "chefs" </> o_) $
    matchHere $ handleAuthToken ChefsLink
  matchAny $ \_ _ resp -> do
    resp $ textOnly "" status302
      [ ( "Location"
        , T.encodeUtf8
          $ printURI
          $ packLocation
              (Strict.Just $ if envTls then "https" else "http")
              True
              envHostname
          $ toLocation RootLink
        )
      ]

  -- -- images -- FIXME handled by NGINX, so...
  -- matchGroup (l_ "images" </> o_) $ do
  --   forM_ images $ \(file, content) -> do
  --     let (file', ext) = T.breakOn "." (T.pack file)
  --     match (l_ file' </> o_) $ action $ get $
  --       bytestring (Other (T.dropWhile (== '.') ext)) (LBS.fromStrict content)

