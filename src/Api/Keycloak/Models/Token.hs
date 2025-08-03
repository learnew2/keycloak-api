{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Keycloak.Models.Token
  ( GrantRequest(..)
  , GrantResponse(..)
  ) where

import           Data.Aeson
import           Data.Text          (Text)
import           Web.FormUrlEncoded
import           Web.HttpApiData    (ToHttpApiData (toQueryParam))

data GrantRequest = AuthorizationCodeRequest
  { reqCode         :: !Text
  , reqRedirectUri  :: !Text
  , reqClientID     :: !Text
  , reqClientSecret :: !Text
  } | ClientCredentialsRequest
  { reqClientID     :: !Text
  , reqClientSecret :: !Text
  } deriving Show

instance ToForm GrantRequest where
  toForm AuthorizationCodeRequest { .. } =
    [ ("grant_type", "authorization_code")
    , ("redirect_uri", toQueryParam reqRedirectUri)
    , ("code", toQueryParam reqCode)
    , ("client_id", toQueryParam reqClientID)
    , ("client_secret", toQueryParam reqClientSecret)
    ]
  toForm ClientCredentialsRequest { .. } =
    [ ("grant_type", "client_credentials")
    , ("client_id", toQueryParam reqClientID)
    , ("client_secret", toQueryParam reqClientSecret)
    ]

data GrantResponse = GrantResponse
  { accessToken        :: !Text
  , accessTokenExpires :: !Int
  , tokenScope         :: !Text
  } deriving Show

instance FromJSON GrantResponse where
  parseJSON = withObject "GrantResponse" $ \v -> GrantResponse
    <$> v .: "access_token"
    <*> v .: "expires_in"
    <*> v .: "scope"

