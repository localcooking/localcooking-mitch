module Spec.Content.Root where

import Links (SiteLinks (RegisterLink, MealsLink, ChefsLink), AboutPageLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, whileMountedLocalCooking, performActionLocalCooking, initLocalCookingState)

import Prelude
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location, toLocation)
import Data.Lens (Lens', lens)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import DOM.HTML.Window.Extra (WindowSize (Laptop))
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, em, img, strong, text) as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Icons.PictureInPicture (pictureInPictureIcon)
import MaterialUI.Icons.ShoppingCart (shoppingCartIcon)
import MaterialUI.Icons.Timelapse (timelapseIcon)
import MaterialUI.Icons.LocalShipping (localShippingIcon)
import MaterialUI.Icons.RestaurantMenu (restaurantMenuIcon)




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
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)


getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec params@{toURI,siteLinks} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Locally Sourced Cuisine for Working Families"]
      ] <> ( if state.localCooking.windowSize < Laptop
                then paragraph1 {toURI,siteLinks}
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid {xs: 8, item: true} $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph1 {toURI,siteLinks} <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    , grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph1Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em"}
                        ] []
                      ]
                    ]
                  ]
           ) <>
      [ divider {}
      , typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.left
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Simple Personalized Ordering"]
      ] <> ( if state.localCooking.windowSize < Laptop
                then paragraph2
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph2Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em"}
                        ] []
                      ]
                    , grid {xs: 8, item: true} $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph2 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    ]
                  ]
           ) <>
      [ divider {}
      , typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "How Long Does it Take, and Why?"]
      ] <> ( if state.localCooking.windowSize < Laptop
                then paragraph3
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid
                      { xs: 8
                      , item: true
                      } $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph3 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    , grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph3Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em"}
                        ] []
                      ]
                    ]
                  ]
           )


root :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> R.ReactElement
root params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []


paragraph1 :: forall eff
            . { toURI :: Location -> URI
              , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
              }
           -> Array R.ReactElement
paragraph1 {toURI,siteLinks} =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , paragraph: true
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Local Cooking is a marketplace for individual chefs, dedicated to providing hand-made, healthy, creative meals to the public at competitive prices. Our platform allows chefs to showcase "
    , R.strong [] [R.text "their own"]
    , R.text " menus and culinary artistry — customers can search for a specific dish, or for a style of talent through our app."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Our chefs are paid on an order-by-order basis, as independent contractors; they receive the majority of profit on every order, while our app allows them reach more customers and maintain their own portfolio. We want to make the experience of ordering a hand-cooked meal "
    , R.em [] [R.text "personal"]
    , R.text " again, yet "
    , R.em [] [R.text "streamlined"]
    , R.text " to meet the needs of our modern world."
    ]
  , R.div [RP.style {textAlign: "right"}]
    [ button
      { href: URI.print $ toURI $ toLocation MealsLink
      , color: Button.primary
      , onClick: mkEffFn1 preventDefault
      , onTouchTap: mkEffFn1 \e -> do
        preventDefault e
        unsafeCoerceEff (siteLinks MealsLink)
      } [R.text "Browse Meals"]
    , button
      { href: URI.print $ toURI $ toLocation ChefsLink
      , color: Button.secondary
      , onClick: mkEffFn1 preventDefault
      , onTouchTap: mkEffFn1 \e -> do
        preventDefault e
        unsafeCoerceEff (siteLinks ChefsLink)
      } [R.text "Browse Chefs"]
    , button
      { href: URI.print $ toURI $ toLocation RegisterLink
      , color: Button.primary
      , variant: Button.raised
      , onClick: mkEffFn1 preventDefault
      , onTouchTap: mkEffFn1 \e -> do
        preventDefault e
        unsafeCoerceEff (siteLinks RegisterLink)
      } [R.text "Sign Up Now"]
    ]
  ]


-- FIXME links!!
paragraph2 :: Array R.ReactElement
paragraph2 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "The process of ordering a meal is pretty simple:"
    ]
  , list {dense: true}
    [ listItem {}
      [ listItemIcon {} searchIcon
      , listItemText
        {primary: "Browse our Chefs and Menus"}
      ]
    , listItem {}
      [ listItemIcon {} pictureInPictureIcon
      , listItemText
        {primary: "View the details on specific meals — the ingredients, the culture and history, and how to prepare it"}
      ]
    , listItem {}
      [ listItemIcon {} shoppingCartIcon
      , listItemText
        {primary: "Add meals to your cart, ordering before the chef's chosen deadline (so they have enough time to care for everyone's meals)"}
      ]
    , listItem {}
      [ listItemIcon {} timelapseIcon
      , listItemText
        {primary: "Checkout your cart, wait for updates on your order"}
      ]
    , listItem {}
      [ listItemIcon {} localShippingIcon
      , listItemText
        {primary: "Receive your delivery of frozen meals; store them, or prepare and enjoy!"}
      ]
    ]
  ]


paragraph3 :: Array R.ReactElement
paragraph3 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "Every chef gets to decide their own schedule and deadline — each menu has its own shipping date, and each chef has their own work schedule to fill their orders in bulk. This allows chefs to "
    , R.em [] [R.text "care"]
    , R.text " about each meal and give their full attention to their craft, without having to worry about wasteful time constraints. Each chef:"
    ]
  , list {dense: true}
    [ listItem {}
      [ listItemIcon {} restaurantMenuIcon
      , listItemText
        {primary: "Manages their own work schedule and kitchen time"}
      ]
    , listItem {}
      [ listItemIcon {} restaurantMenuIcon
      , listItemText
        {primary: "Creates their own menus, with their own deadlines"}
      ]
    , listItem {}
      [ listItemIcon {} restaurantMenuIcon
      , listItemText
        {primary: "Builds their own consumer base and reputation"}
      ]
    ]
  ]
