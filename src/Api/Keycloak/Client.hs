{-# LANGUAGE DataKinds #-}
module Api.Keycloak.Client
  ( validateToken
  , grantToken
  , getRealmRoles
  , createRealmRole
  , deleteRealmRole
  ) where

import           Api.Keycloak
import           Api.Keycloak.Models
import           Api.Keycloak.Models.Introspect
import           Api.Keycloak.Models.Role
import           Api.Keycloak.Models.Token
import           Control.Monad.Except           (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Functor                   ((<&>))
import           Data.List                      (intercalate)
import qualified Data.Map                       as M
import           Data.Proxy
import           Data.Text                      (Text, isInfixOf, pack)
import           GHC.Generics
import           Network.HTTP.Types             (Status (..))
import           Servant.API
import           Servant.Client
import qualified Servant.Client.Streaming       as S

api :: Proxy KeycloakAPI
api = Proxy

validateToken
  :<|> grantToken
  :<|> getRealmRoles
  :<|> createRealmRole'
  :<|> deleteRealmRole' = client api

noContentStatusWrapper :: ClientM a -> ClientM ()
noContentStatusWrapper req = do
  state <- ask
  res <- liftIO $ runClientM req state
  case res of
    (Right _) -> pure ()
    (Left exception@(FailureResponse _ (Response { responseStatusCode = status, responseBody = body }))) -> do
      let stCode = statusCode status
      if stCode >= 200 && stCode < 300 then pure () else throwError exception
    (Left otherException) -> throwError otherException

createRealmRole realm token req = do
  noContentStatusWrapper (createRealmRole' realm token req)

deleteRealmRole realm roleId' token = noContentStatusWrapper (deleteRealmRole' realm roleId' token)
