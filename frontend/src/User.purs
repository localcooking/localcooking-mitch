module User where

import LocalCooking.Global.User.Class (class UserDetails)
import LocalCooking.Semantics.Common (User)

import Data.Maybe (Maybe)
import Data.Generic (class Generic)
import Text.Email.Validate (EmailAddress)



data PreUserDetails = PreUserDetails (Maybe User)


newtype UserDetails = UserDetails
  { user :: User
  }

derive instance genericUserDetails :: Generic UserDetails

instance userDetailsUserDetails :: UserDetails UserDetails where
  getUser (UserDetails {user}) = user
  setUser user (UserDetails x) = UserDetails x { user = user }
