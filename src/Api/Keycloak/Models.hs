{-# LANGUAGE OverloadedStrings #-}
module Api.Keycloak.Models
  ( BearerWrapper(..) ) where

import qualified Data.ByteString.Char8 as BS
import           Data.String
import           Data.Text
import           Servant.API

newtype BearerWrapper = BearerWrapper Text

instance ToHttpApiData BearerWrapper where
  toHeader (BearerWrapper token) = (BS.pack . unpack) $ "Bearer " <> token

instance IsString BearerWrapper where
  fromString = (BearerWrapper . pack)
