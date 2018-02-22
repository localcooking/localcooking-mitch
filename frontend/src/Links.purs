module Links where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI.Location (Location (..))
import Data.Path.Pathy ((</>), dir, file, rootDir)


class ToLocation sym where
  toLocation :: sym -> Location


data LogoLinks
  = LogoPng
  | LogoWhitePng
  | IconPng
  | IconSvg


instance toLocationLogoLinks :: ToLocation LogoLinks where
  toLocation x = case x of
    LogoPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo.png") Nothing Nothing
    LogoWhitePng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white.png") Nothing Nothing
    IconPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.png") Nothing Nothing
    IconSvg -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.svg") Nothing Nothing