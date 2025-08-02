{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.Keycloak
  ( KeycloakAPI
  ) where

import           Api.Keycloak.Models.Introspect
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                     (Value)
import qualified Data.Map                       as M
import           Data.Text
import           Network.HTTP.Conduit
import           Servant.API
import           Servant.Client
import           Web.FormUrlEncoded

type RealmCapture = Capture "realm" Text

type KeycloakAPI = "realms" :> RealmCapture :> "protocol" :> "openid-connect" :> "token" :> "introspect" :> ReqBody '[FormUrlEncoded] IntrospectRequest :> Post '[JSON] IntrospectResponse
