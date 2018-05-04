module Links where

import LocalCooking.Links.Class (class ToLocation, toLocation, class FromLocation, fromLocation, class LocalCookingSiteLinks, class LocalCookingUserDetailsLinks, replaceState', defaultSiteLinksPathParser)

import Prelude
import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..))
import Data.URI (Query (..))
import Data.URI.URI as URI
import Data.URI.Path as URIPath
import Data.URI.Location (Location (..), fromURI, printLocation)
import Data.StrMap as StrMap
import Data.Path.Pathy ((</>), dir, file, rootDir, Path, Rel, File, Sandboxed)
import Data.Generic (class Generic, gEq, gShow)
import Data.NonEmpty ((:|))
import Text.Parsing.StringParser (Parser, try, runParser)
import Text.Parsing.StringParser.String (char, string, eof)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, warn)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, history)
import DOM.HTML.Location (href)
import DOM.HTML.History (DocumentTitle (..))
import DOM.HTML.Types (HISTORY)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)




data ImageLinks
  = LogoPng
  | Logo40Png
  | LogoWhitePng
  | LogoWhite40Png
  | IconPng
  | IconSvg


instance toLocationImageLinks :: ToLocation ImageLinks where
  toLocation x = case x of
    LogoPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo.png") Nothing Nothing
    Logo40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-40.png") Nothing Nothing
    LogoWhitePng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white.png") Nothing Nothing
    LogoWhite40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white-40.png") Nothing Nothing
    IconPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.png") Nothing Nothing
    IconSvg -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.svg") Nothing Nothing



data AboutPageLinks
  = Paragraph1Png
  | Paragraph2Png
  | Paragraph3Png
  | Paragraph4Png

instance toLocationAboutPageLinks :: ToLocation AboutPageLinks where
  toLocation x = case x of
    Paragraph1Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "paragraph1-image.png") Nothing Nothing
    Paragraph2Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "paragraph2-image.png") Nothing Nothing
    Paragraph3Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "paragraph3-image.png") Nothing Nothing
    Paragraph4Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "paragraph4-image.png") Nothing Nothing


data UserDetailsLinks
  = UserDetailsGeneralLink
  | UserDetailsSecurityLink
  | UserDetailsOrdersLink
  | UserDetailsDietLink
  | UserDetailsAllergiesLink

derive instance genericUserDetailsLinks :: Generic UserDetailsLinks

instance eqUserDetailsLinks :: Eq UserDetailsLinks where
  eq = gEq

instance showUserDetailsLinks :: Show UserDetailsLinks where
  show = gShow

instance arbitraryUserDetailsLinks :: Arbitrary UserDetailsLinks where
  arbitrary = oneOf $
    ( pure UserDetailsGeneralLink
    ) :|
    [ pure UserDetailsSecurityLink
    , pure UserDetailsOrdersLink
    , pure UserDetailsDietLink
    , pure UserDetailsAllergiesLink
    ]

instance localCookingUserDetailsLinksUserDetailsLinks :: LocalCookingUserDetailsLinks UserDetailsLinks where
  userDetailsGeneralLink = UserDetailsGeneralLink
  userDetailsSecurityLink = UserDetailsSecurityLink
  toUserDetailsDocumentTitle x = case x of
    UserDetailsGeneralLink   -> "General - "
    UserDetailsSecurityLink  -> "Security - "
    UserDetailsOrdersLink    -> "Orders - "
    UserDetailsDietLink      -> "Diet - "
    UserDetailsAllergiesLink -> "Allergies - "

userDetailsLinksToPath :: UserDetailsLinks -> Path Rel File Sandboxed
userDetailsLinksToPath x = case x of
  UserDetailsGeneralLink -> file "general"
  UserDetailsSecurityLink -> file "security"
  UserDetailsOrdersLink -> file "orders"
  UserDetailsDietLink -> file "diet"
  UserDetailsAllergiesLink -> file "allergies"

userDetailsLinksParser :: Parser UserDetailsLinks
userDetailsLinksParser = do
  void divider
  let general = do
        void (string "general")
        pure UserDetailsGeneralLink
      security = do
        void (string "security")
        pure UserDetailsSecurityLink
      orders = do
        void (string "orders")
        pure UserDetailsOrdersLink
      diet = do
        void (string "diet")
        pure UserDetailsDietLink
      allergies = do
        void (string "allergies")
        pure UserDetailsAllergiesLink
  try general
    <|> try security
    <|> try orders
    <|> try diet
    <|> allergies
  where
    divider = char '/'


data SiteLinks
  = RootLink
  | MealsLink -- FIXME search terms
  | ChefsLink -- FIXME search terms or hierarchy
  | RegisterLink -- FIXME authenticated vs unauthenticated?
  | UserDetailsLink (Maybe UserDetailsLinks)
  | EmailConfirmLink

instance arbitrarySiteLinks :: Arbitrary SiteLinks where
  arbitrary = oneOf $
        (pure RootLink)
    :|  [ pure MealsLink
        , pure ChefsLink
        , pure RegisterLink
        , do mUserDetails <- arbitrary
             pure (UserDetailsLink mUserDetails)
        , pure EmailConfirmLink
        ]

initSiteLinks :: forall eff
               . Eff ( console :: CONSOLE
                     , dom     :: DOM
                     , history :: HISTORY
                     | eff) SiteLinks
initSiteLinks = do
  w <- window
  l <- location w
  h <- history w
  p <- href l
  case URI.parse p of
    Left e -> do
      warn $ "Href parsing error: " <> show e
      replaceState' RootLink h
      pure RootLink
    Right uri -> case fromURI uri of
      Nothing -> do
        warn $ "URI can't be a location: " <> show uri
        replaceState' RootLink h
        pure RootLink
      Just {location: location@(Location _ mQuery _)} -> case fromLocation location of
        Left e -> do
          warn $ "Location can't be a SiteLinks: " <> e <> ", " <> show location
          replaceState' RootLink h
          pure RootLink
        Right (x :: SiteLinks) -> do
          -- FIXME only adjust for authToken when it's parsable? Why?
          case mQuery of
            Nothing -> pure unit
            Just (Query qs) -> do
              case
                    StrMap.lookup "authToken" (StrMap.fromFoldable qs)
                <|> StrMap.lookup "formData" (StrMap.fromFoldable qs)
                <|> StrMap.lookup "emailToken" (StrMap.fromFoldable qs)
                of
                Nothing -> pure unit
                Just _ -> flip replaceState' h $ case x of
                  EmailConfirmLink -> RootLink
                  _ -> x
          pure x

derive instance genericSiteLinks :: Generic SiteLinks

instance showSiteLinks :: Show SiteLinks where
  show = printLocation <<< toLocation

instance eqSiteLinks :: Eq SiteLinks where
  eq = gEq

instance toLocationSiteLinks :: ToLocation SiteLinks where
  toLocation x = case x of
    RootLink  -> Location (Left rootDir) Nothing Nothing
    MealsLink -> Location (Right $ rootDir </> file "meals") Nothing Nothing
    ChefsLink -> Location (Right $ rootDir </> file "chefs") Nothing Nothing
    RegisterLink -> Location (Right $ rootDir </> file "register") Nothing Nothing
    EmailConfirmLink -> Location (Right $ rootDir </> file "emailConfirm") Nothing Nothing
    UserDetailsLink mUserDetails ->
      Location
        ( Right $ case mUserDetails of
             Nothing -> rootDir </> file "userDetails"
             Just d -> rootDir </> dir "userDetails" </> userDetailsLinksToPath d
        ) Nothing Nothing


instance localCookingSiteLinksSiteLinks :: LocalCookingSiteLinks SiteLinks UserDetailsLinks where
  rootLink = RootLink
  registerLink = RegisterLink
  userDetailsLink = UserDetailsLink
  getUserDetailsLink link = case link of
    UserDetailsLink mDetails -> Just mDetails
    _ -> Nothing
  toDocumentTitle x = case x of
    MealsLink -> "Meals - "
    ChefsLink -> "Chefs - "
    _ -> ""
  subsidiaryTitle _ = ""


-- Policy: don't fail on bad query params / fragment unless you have to
instance fromLocationSiteLinks :: FromLocation SiteLinks where
  fromLocation (Location path mQuery mFrag) = do
    case runParser siteLinksPathParser (URIPath.printPath path) of
      Left e -> Left (show e)
      Right link -> pure link
    where
      siteLinksPathParser :: Parser SiteLinks
      siteLinksPathParser = do
        divider
        let def = defaultSiteLinksPathParser userDetailsLinksParser
            meals = do
              void (string "meals")
              pure MealsLink
            chefs = do
              void (string "chefs")
              pure ChefsLink -- FIXME search parameters or hierarchy
            emailConfirm = do
              void (string "emailConfirm")
              pure EmailConfirmLink
        try meals
          <|> try chefs
          <|> try emailConfirm
          <|> def
        where
          divider = void (char '/')

