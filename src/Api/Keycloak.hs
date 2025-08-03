{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.Keycloak
  ( KeycloakAPI
  ) where

import           Api.Keycloak.Models
import           Api.Keycloak.Models.Introspect
import           Api.Keycloak.Models.Role
import           Api.Keycloak.Models.Token
import           Data.Text
import           Servant.API

type RealmCapture = Capture "realm" Text
type RoleIDCapture = Capture "roleId" Text
type AuthHeader = Header' '[Required] "Authorization" BearerWrapper

type KeycloakAPI = "realms" :> RealmCapture :> "protocol" :> "openid-connect" :> "token" :> "introspect" :> ReqBody '[FormUrlEncoded] IntrospectRequest :> Post '[JSON] IntrospectResponse
  :<|> "realms" :> RealmCapture :> "protocol" :> "openid-connect" :> "token" :> ReqBody '[FormUrlEncoded] GrantRequest :> Post '[JSON] GrantResponse
  :<|> "admin" :> "realms" :> RealmCapture :> "roles" :> AuthHeader :> Get '[JSON] [RealmRole]
  :<|> "admin" :> "realms" :> RealmCapture :> "roles" :> AuthHeader :> ReqBody '[JSON] RoleCreateRequest :> Post '[JSON] ()
  :<|> "admin" :> "realms" :> RealmCapture :> "roles-by-id" :> RoleIDCapture :> AuthHeader :> Delete '[JSON] ()
