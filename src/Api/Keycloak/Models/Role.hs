{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Keycloak.Models.Role
  ( RealmRole(..)
  , RoleCreateRequest(..)
  ) where

import           Data.Aeson
import           Data.Text  (Text)

data RealmRole = RealmRole
  { roleId          :: !Text
  , roleName        :: !Text
  , roleDescription :: !Text
  } deriving (Show, Eq)

instance FromJSON RealmRole where
  parseJSON = withObject "RealmRole" $ \v -> RealmRole
    <$> v .: "id"
    <*> v .: "name"
    <*> v .:? "description" .!= ""

data RoleCreateRequest = RoleCreateRequest
  { reqRoleName :: !Text
  } deriving (Show, Eq)

instance ToJSON RoleCreateRequest where
  toJSON (RoleCreateRequest { .. }) = object ["name" .= String reqRoleName]
