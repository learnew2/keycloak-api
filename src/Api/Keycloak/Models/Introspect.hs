{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Keycloak.Models.Introspect
  ( IntrospectRequest(..)
  , IntrospectResponse(..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap  as KM
import           Web.FormUrlEncoded
import           Web.HttpApiData

data IntrospectRequest = IntrospectRequest
  { reqToken        :: !String
  , reqClientID     :: !String
  , reqClientSecret :: !String
  } deriving Show

instance ToForm IntrospectRequest where
  toForm (IntrospectRequest { .. }) =
    [ ("token", toQueryParam reqToken)
    , ("client_id", toQueryParam reqClientID)
    , ("client_secret", toQueryParam reqClientSecret)
    ]

data IntrospectResponse = InactiveToken |
  ActiveToken
  { tokenRealmRoles        :: ![String]
  , tokenAccountRoles      :: ![String]
  , tokenScope             :: !String
  , tokenUsername          :: !String
  , tokenPreferredUsername :: !String
  , tokenName              :: !(Maybe String)
  , tokenEmail             :: !(Maybe String)
  } deriving Show

instance FromJSON IntrospectResponse where
  parseJSON = withObject "IntrospectResponse" $ \v -> case KM.lookup "active" v of
    Just (Bool False) -> pure InactiveToken
    Just (Bool True)  -> ActiveToken
      <$> (v .: "realm_access" >>= (.: "roles"))
      <*> (v .: "resource_access" >>= (.: "account") >>= (.: "roles"))
      <*> v .: "scope"
      <*> v .: "username"
      <*> v .: "preferred_username"
      <*> v .:? "name"
      <*> v .:? "email"
    _anyOther -> fail $ "IntrospectResponse got invalid active value: " <> show _anyOther

instance ToJSON IntrospectResponse where
  toJSON InactiveToken = object ["active" .= Bool False]
  toJSON ActiveToken { .. } = object
    [ "active" .= Bool True
    , "realm_access" .= object [ "roles" .= tokenRealmRoles ]
    , "resource_access" .= object [ "account" .= object [ "roles" .= tokenAccountRoles ]]
    , "scope" .= tokenScope
    , "username" .= tokenUsername
    , "preferred_username" .= tokenPreferredUsername
    , "name" .= tokenName
    , "email" .= tokenEmail
    ]
