{-# LANGUAGE DataKinds #-}
module Api.Keycloak.Client
  ( validateToken
  ) where

import           Api.Keycloak
import           Data.Proxy
import           Servant.Client

api :: Proxy KeycloakAPI
api = Proxy

validateToken = client api
